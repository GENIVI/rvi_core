//---- BEGIN COPYRIGHT -------------------------------------------------------
//
// Copyright (C) 2007 - 2014, Rogvall Invest AB, <tony@rogvall.se>
//
// This software is licensed as described in the file COPYRIGHT, which
// you should have received as part of this distribution. The terms
// are also available at http://www.rogvall.se/docs/copyright.txt.
//
// You may opt to use, copy, modify, merge, publish, distribute and/or sell
// copies of the Software, and permit persons to whom the Software is
// furnished to do so, under the terms of the COPYRIGHT file.
//
// This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
// KIND, either express or implied.
//
//---- END COPYRIGHT ---------------------------------------------------------
//
// WebSocket/Erlang interface
//
//

// operations:
//     {rsync,IRef,Request}  -> {reply,IRef,Reply}
//     {nsync,IRef,Request}  -> {noreply,IRef}
//     {async,IRef,Request}  -> void
//     {dsync,IRef,Request}  -> void  ... {reply,IRef,Reply}
//
// request:
//     {new, Class, Arguments}
//        -> {object,ID}
//
//     {newf, ParamNames, Body}
//        -> {function,ID}
//
//     {delete, ID}
//        -> ok | {error,Reason}
//
//     {call, Object, method, This, Arguments}
//        -> {ok,Value} | {error,Reason}
//
//     {call, Function, This, Arguments}
//        -> {ok,Value} | {error,Reason}
//
//     {get, Object, AttrIndex} 
//        -> {ok,Value} | {error,Reason}
//     {set, Object, AttrIndex, Value}
//        -> {ok,Value} | {error,Reason}
//
// Reverse operations:
//     {start, Mod, Fun, Args} 
//     {call,  Mod, Fun, Args} -> Value
//     {cast,  Mod, Fun, Args}
//     {register, Name}
//     {unregister}
//
//     {notify, ID, Data}
//     {info,  String}
//
//  Values:
//     number => integer|float
//     string => list of chars
//     atom   => string
//     boolean => true|false
//     array  => list of values
//     function => {function, ID}
//     object   => {object,ID}
//
(function() {
    id_counter = 1;
    Object.defineProperty(Object.prototype, "__uniqueId", {
        writable: true
    });
    Object.defineProperty(Object.prototype, "uniqueId", {
        get: function() {
            if (this.__uniqueId == undefined)
                this.__uniqueId = id_counter++;
            return this.__uniqueId;
        }
    });
}());

var wse_console_debug = true;
//
// window.onerror = function(message, url, line)
//    console.low("window error was invoked with message = " +
//       message + ", url = " + url + ", line = " + line);
// }
// 

function WseClass(enable_console) {
    this.win         = window;
    this.ws          = undefined;
    this.state       = "closed";

    this.objects     = {};  // object of objects

    this.iref        = 1;
    this.requests    = new Array();
    this.reply_fun   = {};
    this.reply_obj   = {};
    this.reply_ref   = {};

    this.OkTag       = Ei.atom("ok");
    this.ErrorTag    = Ei.atom("error");
    this.ExceptionTag = Ei.atom("exception");
    this.ObjectTag   = Ei.atom("object");
    this.FunctionTag = Ei.atom("function");
    this.ReplyTag    = Ei.atom("reply");
    this.NoReplyTag  = Ei.atom("noreply");

    this.dummyConsole = {  
	assert : function(){},  
	log : function(){},  
	warn : function(){},  
	error : function(){},  
	debug : function(){},  
	dir : function(){},  
	info : function(){}
    };
    if (enable_console && (window.console !== undefined))
	this.console = window.console;
    else
	this.console = this.dummyConsole;
};

WseClass.prototype.enable_console_output = function(enable) {
    if (enable && (window.console !== undefined))
	this.console = window.console;
    else
	this.console = this.dummyConsole;
}

WseClass.prototype.getWse = function(id) {
    var i;
    for (i = 0; i < this.win.frames.length; i++) {
	if (('Wse' in this.win.frames[i]) &&
	    (this.win.frames[i].Wse.id == id))
	    return this.win.frames[i].Wse;
    }
    return null;
}

// Fixme: check binaryType for the wanted encoding ?!
WseClass.prototype.encode = function(Obj) {
//    return Base64.encode(Ei.encode(Obj));
    return Ei.encode(Obj);
};

// Fixme: handle base64 string & Blob and ArrayBuffer!
WseClass.prototype.decode = function(Data) {
//   return Ei.decode(Base64.decode(Data));
    return Ei.decode(Data, 0);
};

// Decide an Erlang term that represent a json object into a
// native json object i.e {struct,[{a,1},{b,2}]} => { a:1, b:2 }
WseClass.prototype.decode_js = function(Data) {
    switch(typeof(Data)) {
    case "string": return Data;
    case "number": return Data;
    case "object":
	if (Ei.eqAtom(Data, "true")) return true;
	else if (Ei.eqAtom(Data, "false")) return false;
	else if (Ei.eqAtom(Data, "null")) return null;
	else if (Ei.isTupleSize(Data,2)) {
	    var elem = Data.value;
	    if (Ei.eqAtom(elem[0], "array") && Ei.isArray(elem[1])) {
		var iArr = elem[1];
		var arr = new Array();
		var len, i;
		for (i = 0; i < iArr.length; i++)
		    arr[i] = this.decode_js(iArr[i]);
		return arr;
	    }
	    else if (Ei.eqAtom(elem[0],"struct") && Ei.isArray(elem[1])) {
		var iArr = elem[1];
		var obj = new Object();
		var len, i;
		for (i = 0; i < iArr.length; i++) {
		    if (Ei.isTupleSize(iArr[i],2)) {
			var pair = iArr[i].value;
			if (typeof(pair[0]) == "string")
			    obj[pair[0]] = this.decode_js(pair[1]);
		    }
		}
		return obj;
	    }
	    else {
		this.console.debug("unable to decode pair " + Data);
		return null;
	    }
	}
	else if (Ei.isArray(Data)) {
	    var arr = new Array();
	    var len, i;
	    for (i = 0; i < Data.length; i++)
		    arr[i] = this.decode_js(Data[i]);
		return arr
	}
	else {
	    this.console.debug("unable to decode object " + Data);
	    return null;
	}
	break;
    default:
	this.console.debug("unable to decode data " + Data);
	return null;
    }
};

WseClass.prototype.open = function(url) {
    var wse = this;  // save WebSocket closure

    if ("WebSocket" in window) {
	this.state = "connecting";
	this.ws = new WebSocket(url);
	this.ws.binaryType = "arraybuffer";
	

	this.ws.onopen = function() {
            var info = Ei.tuple(Ei.atom("info"),"connected");
	    wse.state = "open";
            this.send(wse.encode(info));
	    for (ref in wse.requests) {
		var cmd = wse.requests[ref];
		if (cmd !== undefined)
		    this.send(wse.encode(cmd));
	    }
	    // overwrite the old request array with a new and let
	    // the old array be garbage collected. The new array is 
	    // not used right now, so we could set it to null, but keep
	    // it for future ideas.
	    wse.requests = new Array();
	};
	
	this.ws.onmessage = function(evt) {
            var Request = wse.decode(evt.data);
            var val = wse.dispatch(Request);
            if (val != undefined)
		this.send(wse.encode(val));
	};

	this.ws.onclose = function() {
	    wse.state = "closed";
	    wse.ws = undefined;
	};
	return true;
    }
    return false;
};

//
// Remove all children (DOM util)
//
WseClass.prototype.remove_children = function(Cell) {
    if (Cell.hasChildNodes()) {
	while(Cell.childNodes.length >= 1)
	    Cell.removeChild(Cell.firstChild);
    }
}

WseClass.prototype.lookup_object = function(index) {
    var obj = this.objects[index];
    this.console.debug("lookup object "+index+" = "+obj);
    return obj;
}

WseClass.prototype.insert_object = function(index,obj) {
    this.console.debug("insert object "+index+" = "+obj);
    this.objects[index] = obj;
}

WseClass.prototype.delete_object = function(index) {
    this.console.debug("deleting object "+index);
    delete this.objects[index];
}

//
// Decode javascript object into BERT rpc values
//
WseClass.prototype.encode_value = function(Obj) {
    switch(typeof(Obj)) {
    case "number": return Obj;
    case "string": return Obj;
    case "boolean": return Obj ? Ei.atom("true") : Ei.atom("false");
    case "object":
	// {object, window}    - the current window object
	// {object, document}  - the current document object
	// {object, id}        - DOM object with id field
	// {object, num}       - Stored in objects object!
	if (Obj == window.self)
	    return Ei.tuple(this.ObjectTag,Ei.atom("window"));
	else if (Obj == window.document) 
	    return Ei.tuple(this.ObjectTag,Ei.atom("document"));
	else if (Obj == screen)
	    return Ei.tuple(this.ObjectTag,Ei.atom("screen"));
	else if (Obj == navigator)
	    return Ei.tuple(this.ObjectTag,Ei.atom("navigator"));
	else if (('id' in Obj) && Obj.id) {
	    if (Obj == document.getElementById(Obj.id))
		return Ei.tuple(this.ObjectTag,Obj.id);
	}
	this.insert_object(Obj.uniqueId, Obj);
	return Ei.tuple(this.ObjectTag,Obj.uniqueId);
    case "function":
	this.insert_object(Obj.uniqueId, Obj);
	return Ei.tuple(this.FunctionTag,Obj.uniqueId);
    case "undefined":
	return Ei.atom("undefined");
    }
};

//
// Decode BERT rpc values into javascript objects
// {object, window}   =>  window.self
// {object, document} =>  window.document
// {object, screen}   =>  screen
// {object, navigator} => navigator
// {object, id}       => window.document.getElelementById(id)
// {object, num}      => objects[num]
// {function,num}     => objects[num]
// [H1,H2...Hn]       => Array
//

WseClass.prototype.decode_value = function(Obj) {
    switch(typeof(Obj)) {
    case "number":  return Obj;
    case "string":  return Obj;
    case "boolean": return Obj;
    case "object":
	if (Ei.isAtom(Obj))
	    return Obj.value;
	else if (Ei.isTuple(Obj)) {
	    var elem = Obj.value;
	    if ((elem.length==2) && Ei.eqAtom(elem[0],"object")) {
		if (Ei.eqAtom(elem[1], "window"))
		    return window.self;
		else if (Ei.eqAtom(elem[1],"document"))
		    return window.document;
		else if (Ei.eqAtom(elem[1], "screen"))
		    return screen;
		else if (Ei.eqAtom(elem[1], "navigator"))
		    return navigator;
		else if (typeof(elem[1]) == "number")
		    return this.lookup_object(elem[1]);
		else if (typeof(elem[1]) == "string")
		    return window.document.getElementById(elem[1]);
	    }
	    // this is a garbage collected version {object,num,res-bin}
	    if ((elem.length==3) && Ei.eqAtom(elem[0],"object")) {
		if ((typeof(elem[1]) == "number") && Ei.isBinary(elem[2])) {
		    return this.lookup_object(elem[1]);
		}
		return undefined;
	    }
	    if ((elem.length==2) && Ei.eqAtom(elem[0],"function")) {
		if (typeof(elem[1]) == "number")
		    return this.lookup_object(elem[1]);
		return undefined;
	    }
	    // this is a garbage collected version {function,num,res-bin}
	    if ((elem.length==3) && Ei.eqAtom(elem[0],"function")) {
		if ((typeof(elem[1]) == "number") && Ei.isBinary(elem[2])) {
		    return this.lookup_object(elem[1]);
		}
		return undefined;
	    }
	}
	else if (Ei.isArray(Obj)) {
	    var i;
	    var arr = new Array();
	    for (i = 0; i < Obj.length; i++)
		arr[i] = this.decode_value(Obj[i]);
	    return arr;
	}
	return undefined;
    default:
	this.console.debug("unhandled object "+ Obj);
	return Obj;
    }
};
//
// Decode ehtml to DOM 
// Elem =  {Tag,Attributes,Children}
//       | {Tag,Attributes}
// Tag is EiAtom
// Attributes is Array of Tuple(Atom,Value) or Atom
// optional Children is Array of Elem
// Return top level DOM element
//
WseClass.prototype.decode_ehtml = function (Obj) {
    var element = undefined;

    if (typeof(Obj) == "string") {
	element = document.createTextNode(Obj);
    }
    else if (typeof(Obj) == "number") {
	element = document.createTextNode(Obj.toString());	
    }
    else if (Ei.isArray(Obj)) {
	element = document.createDocumentFragment();
	for (i = 0; i < Obj.length; i++) {
	    var child = this.decode_ehtml(Obj[i]);
	    if (child != undefined)
		element.appendChild(child);
	}
    }
    else if (Ei.isTuple(Obj)) {
	var argv = Obj.value;

	if ((argv.length == 0) || !Ei.isAtom(argv[0]))
	    return undefined;

	element = document.createElement(argv[0].value);

	if ((argv.length > 1) && Ei.isArray(argv[1])) {
	    var attrs = argv[1];
	    var i;
	    for (i = 0; i < attrs.length; i++) {
		if (Ei.isTuple(attrs[i])) {
		    var key = this.decode_value(attrs[i].value[0]);
		    var value;
		    if (attrs[0].length > 1)
			value = this.decode_value(attrs[i].value[1]);
		    else
			value = true;
		    element.setAttribute(key, value);
		}
	    }
	    if ((argv.length > 2) && Ei.isArray(argv[2])) {
		var children = argv[2];
		for (i = 0; i < children.length; i++) {
		    var child = this.decode_ehtml(children[i]);
		    if (child != undefined)
			element.appendChild(child);
		}
	    }
	}
    }
    return element;
};

//
// Dispatch remote operations
//
WseClass.prototype.dispatch = function (Request) {
    var iref, aref;
    var value, rvalue;
    var r, t;
    var is_dsync = false;

    if (Ei.isTupleSize(Request, 3)) {
	var argv = Request.value;
	aref = argv[1];
	if (Ei.eqAtom(argv[0],      "rsync"))
	    iref = aref;
	else if (Ei.eqAtom(argv[0], "nsync"))
	    iref = -aref;
	else if (Ei.eqAtom(argv[0], "async"))
	    iref = 0;
	else if (Ei.eqAtom(argv[0], "dsync")) {
	    is_dsync = true;
	    iref = 0;
	}
	else if (Ei.eqAtom(argv[0], "reply")) {
	    var fn,obj,ref;
	    iref = aref;
	    value = argv[2];
	    fn = this.reply_fun[iref];
	    obj = this.reply_obj[iref];
	    ref = this.reply_ref[iref];
	    this.console.debug("got reply "+iref+","+value+" fn="+fn+" obj="+obj+" ref="+ref);
	    if (fn != undefined) {
		delete this.reply_fun[iref];
		delete this.reply_obj[iref];
		delete this.reply_ref[iref];
		try {
		    fn(obj,ref,value);
		}
		catch (err) {
		    this.console.info("reply function crashed "+err.message);
		}
	    }
	    return undefined;
	}
	else
	    return undefined;  // signal protocol error
	r = argv[2];
    }
    else
	return undefined;

    if (Ei.isTuple(r)) {
	var argv = r.value;
	if ((argv.length == 3) && Ei.eqAtom(argv[0],"send")) {
	    var Cell = document.getElementById(argv[1]);

	    try {
		if (typeof(argv[2]) == "string") {
		    Cell.innerHTML = Ei.pp(argv[2]);
		}
		else if (Ei.isTuple(argv[2]) || Ei.isArray(argv[2])) {
		    var elem = this.decode_ehtml(argv[2]);
		    this.remove_children(Cell);
		    Cell.appendChild(elem);
		}
		value = this.OkTag;
	    }
	    catch(err) {
		value = Ei.tuple(this.ExceptionTag, err.message);
	    }
	}
	else if ((argv.length == 3) && Ei.eqAtom(argv[0],"new")) {
	    var obj = new Object();
	    var fn  = window[this.decode_value(argv[1])];

	    try {
		fn.apply(obj, this.decode_value(argv[2]));
		obj.__proto__ = fn.prototype;
		rvalue = this.encode_value(obj);
		value = rvalue;
	    }
	    catch(err) {
		value = Ei.tuple(this.ExceptionTag, err.message);
	    }
	}
	else if ((argv.length == 3) && Ei.eqAtom(argv[0],"newf")) {
	    this.console.debug("new Function("+argv[1]+","+argv[2]+")");
	    try {
		var fn = new Function(argv[1],argv[2]);
		this.console.debug("function = "+fn);
		rvalue = this.encode_value(fn);
		value = rvalue;
	    }
	    catch(err) {
		value = Ei.tuple(this.ExceptionTag, err.message);
	    }
	}
	else if ((argv.length == 4) && Ei.eqAtom(argv[0],"call")) {
	    var fn   = this.decode_value(argv[1]);
	    var objb = this.decode_value(argv[2]);
	    var args = this.decode_value(argv[3]);
	    var val;

	    try {
		val = window[fn].apply(objb, args);
		rvalue = this.encode_value(val);
		this.console.debug("call/3=" + Ei.pp(argv[1]) + "," + Ei.pp(argv[2]) + "," + Ei.pp(argv[3]));
		if (is_dsync && (fn === "call") && (val % 1 === 0)) {
		    // val is a reference in this case
		    this.console.debug("set reply_obj["+val+"] = "+objb);
		    objb.reply_obj[val] = this; // patch object
		    objb.reply_ref[val] = aref; // original ref
		}
		value = Ei.tuple(this.OkTag, rvalue);
	    }
	    catch(err) {
		value = Ei.tuple(this.ErrorTag, err.message);
	    }
	}
	else if ((argv.length == 5) && Ei.eqAtom(argv[0],"call")) {
	    var obja = this.decode_value(argv[1]);
	    var meth = this.decode_value(argv[2]);
	    var objb = this.decode_value(argv[3]);
	    var args = this.decode_value(argv[4]);
	    var val;

	    try {
		val  = (obja[meth]).apply(objb, args);
		rvalue = this.encode_value(val);
		this.console.debug("call/4=" + Ei.pp(argv[1]) + "," + Ei.pp(argv[2]) + "," + Ei.pp(argv[3]) + "," + Ei.pp(argv[4]));
		if (is_dsync && (meth === "call") && (val % 1 === 0)) {
		    // val is a reference in this case
		    this.console.debug("set obja.reply_obj["+val+"] = "+this);
		    obja.reply_obj[val] = this; // patch object
		    obja.reply_ref[val] = aref; // original ref
		}
		value = Ei.tuple(this.OkTag, rvalue);
	    }
	    catch(err) {
		value = Ei.tuple(this.ErrorTag, err.message);
	    }
	}
	else if ((argv.length == 3) && Ei.eqAtom(argv[0],"get")) {
	    var obj   = this.decode_value(argv[1]);
	    var attr  = this.decode_value(argv[2]);
	    try {
		rvalue = obj[attr]; // both array and object attribute!
		this.console.debug(argv[1]+".get: "+attr+"="+rvalue);
		value = Ei.tuple(this.OkTag, this.encode_value(rvalue));
	    }
	    catch (err) {
		value = Ei.tuple(this.ErrorTag, err.message);
	    }
	}
	else if ((argv.length == 4) && Ei.eqAtom(argv[0],"set")) {
	    var obj   = this.decode_value(argv[1]);
	    var attr  = this.decode_value(argv[2]);
	    rvalue = this.decode_value(argv[3]);
	    this.console.debug(argv[1]+".set: "+attr+"="+argv[3]+"("+rvalue+")");
	    try {
		obj[attr] = rvalue;  // both array and object attribute!
		value = this.OkTag;
	    }
	    catch (err) {
		value = Ei.tuple(this.ErrorTag, err.message);
	    }
	}
	else if ((argv.length === 2) && Ei.eqAtom(argv[0],"delete")) {
	    // argv[1] must be the uniqID integer 
	    this.delete_object(argv[1]);
	    rvalue = null;
	    value = this.OkTag;
	}
    }
    if (iref == 0) {
	// this.console.debug("ival=0");
	return undefined;
    }
    else if (iref > 0) {
	if (value == undefined) {
	    value = Ei.tuple(this.ErrorTag, Ei.atom("badarg"));
	}
	t = Ei.tuple(this.ReplyTag,iref,value);
    }
    else {
	t = Ei.tuple(this.NoReplyTag,-iref);
    }
    // this.console.debug("t = " + Ei.pp(t));
    return t;
};

WseClass.prototype.send_request = function (ref, request)
{
    if (this.state === "open") {
	this.ws.send(this.encode(request));
    }
    else {
	// safe until channel is open
	this.requests[ref] = request;
    }
}

//
// Start remote controller "program" 
//
WseClass.prototype.start = function (mod,fun,args) {
    var ref = this.iref++;
    var cmd = Ei.tuple(Ei.atom("start"),Ei.atom(mod),Ei.atom(fun),args);

    this.send_request(ref, cmd);
    return ref;
};

//
// Call remote function mod:fun(Args)
// execute onreply when reply is returned
//
WseClass.prototype.call = function (mod,fun,args,onreply) {
    var ref = this.iref++;
    var cmd = Ei.tuple(Ei.atom("call"),ref,Ei.atom(mod),Ei.atom(fun),args);

    this.console.debug("call mod="+mod+", fun="+fun+", args="+args);
    this.reply_fun[ref] = onreply;
    this.reply_obj[ref] = this;
    this.reply_ref[ref] = ref;
    this.console.debug("set reply_fun["+ref+"] = "+onreply);
    this.send_request(ref, cmd);
    return ref;
};

// Used for handle return relay
WseClass.prototype.reply = function (iref,value) {
    var reply = Ei.tuple(this.ReplyTag,iref,value);
    this.console.debug("sending reply "+reply+"id="+this.id);
    if (this.state == "open") {
	this.ws.send(this.encode(reply));
	return true;
    }
    return false;
};
//
// Execute remote function mod:fun(Args)
//
WseClass.prototype.cast = function (mod,fun,args) {
    var ref = this.iref++;
    var cmd = Ei.tuple(Ei.atom("cast"),ref,Ei.atom(mod),Ei.atom(fun),args);
    this.console.debug("cast mod="+mod+", fun="+fun+", args="+args);
    this.send_request(ref, cmd);
    return true;
};

//
// Send notification 
//
WseClass.prototype.notify = function (ref,data) {
    var cmd = Ei.tuple(Ei.atom("notify"),ref,data);
    this.console.debug("notify "+ ref + ", data="+data);
    this.send_request(ref, cmd);
    return true;
};

//
// Register a websocket (on the erlang node side)
// could nearly call erlang:register through the call
// but only nearly
//
WseClass.prototype.register = function (name) {
    var ref = this.iref++;
    var cmd = Ei.tuple(Ei.atom("register"),Ei.atom(name));
    this.console.debug("register "+ name);
    this.send_request(ref, cmd);
    return true;
}

// Unregister a websocket (on the erlang node side)
WseClass.prototype.unregister = function () {
    var ref = this.iref++;
    var cmd = Ei.tuple(Ei.atom("unregister"));
    this.console.debug("unregister");
    this.send_request(ref, cmd);
    return true;
}

var Wse = new WseClass(wse_console_debug);
