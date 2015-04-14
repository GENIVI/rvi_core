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
// Erlang term encode/decode using TypeArray interface
//

function EiClass() {
    this.MAGIC = 131;
    this.SMALL_ATOM = 115;
    this.ATOM = 100;
    this.BINARY = 109;
    this.SMALL_INTEGER = 97;
    this.INTEGER = 98;
    this.SMALL_BIG = 110;
    this.LARGE_BIG = 111;
    this.FLOAT = 99;
    this.NEW_FLOAT = 70;
    this.STRING = 107;
    this.LIST = 108;
    this.SMALL_TUPLE = 104;
    this.LARGE_TUPLE = 105;
    this.NIL = 106;
    this.MAP = 116;
    
    this.use_map = false;         // use map when available
    this.use_small_atom = true;   // use small atom when available
    
    if (this.use_small_atom) {
	this.true_atom_size  = 1+4;
	this.false_atom_size = 1+5;
    }
    else {
	this.true_atom_size  = 2+4;
	this.false_atom_size = 2+5;
    }
};

//
// [] can be encoded as:
//   <<MAGIC, NIL>>
//   <<MAGIC, STRING, 0:16>>
//   <<MAGIC, LIST, 0:32, NIL>>
// 
function EiAtom(Obj) {
    this.type = "Atom";
    this.value = Obj;
    this.toString = function () {
	return Obj;
    };
};

// Obj is typed array or string
function EiBinary(Obj) {
    this.type = "Binary";
    this.value = Obj;
    this.toString = function () {
	return "<<\"" + Obj + "\">>";
    };
};

function EiTuple(Arr) {
    var i;
    this.type   = "Tuple";
    this.length = Arr.length;
    this.value  = Arr;
    this.toString = function () {
	var i, s = "";
	for (i = 0; i < this.value.length; i++) {
	    if (s !== "") {
		s += ", ";
	    }
	    s += this.value[i].toString();
	}
	return "{" + s + "}";
    };
};

// - INTERFACE -

EiClass.prototype.isAtom = function (Obj) {
    return (Obj.type === "Atom");
};

EiClass.prototype.eqAtom = function (Obj, name) {
    return (Obj.type === "Atom") && (Obj.value === name);
};

EiClass.prototype.isArray = function(Obj) {
    return (typeof(Obj) == "object") &&
	(Obj.constructor.toString().indexOf("Array") !== -1);
};

EiClass.prototype.isTuple = function(Obj) {
    return (typeof(Obj) == "object") &&	(Obj.type == "Tuple");
};

EiClass.prototype.isTupleSize = function(Obj,n) {
    return (typeof(Obj) == "object") &&	(Obj.type == "Tuple") &&
	(Obj.length == n);
};

EiClass.prototype.isBinary = function (Obj) {
    return (Obj.type === "Binary");
};

EiClass.prototype.binary_size = function (Obj) {
    if (Obj.type === "Binary") return Obj.value.byteLength;
    return 0;
};


EiClass.prototype.atom = function (Obj) {
    return new EiAtom(Obj);
};

EiClass.prototype.binary = function (Obj) {
    return new EiBinary(Obj);
};

EiClass.prototype.tuple = function () {
    return new EiTuple([].splice.call(arguments,0));
};

// byte_size_xxx
//   calculate the size need to encode the Term 
//   (use byte_size_term to avoid extra magic byte)
//
EiClass.prototype.byte_size = function (Obj) {
    return 1+this.byte_size_term(Obj);
};

EiClass.prototype.byte_size_term = function (Obj) {
    var len, Size;
    switch(typeof(Obj)) {
    case 'string':
	len = Obj.length;
	if (len <= 65535) 
	    Size = 2+len;      // STRING encoded
	else
	    Size = 4+2*len+1;  // LIST encoded (with terminating nil)!
	break;
    case 'boolean':
	Size = Obj ? this.true_atom_size : this.false_atom_size;
	break;
    case 'number':
	if (Obj % 1 === 0) { // then integer
	    if ((Obj >= 0) && (Obj < 256))
		Size = 1;  // small_integer (uint8)
	    else if ((Obj >= -134217728) && (Obj <= 134217727))
		Size = 4;  // integer (int32)
	    else // bignum (32 bit only, can not really be used here
		Size = 1+1+4; // size,sign,byte*4
	}
	else {
	    Size = 8;  // NEW_FLOAT
	}
	break;
    case 'object':
	switch (Obj.type) {
	case 'Atom':
	    Size = this.atom_size(Obj.value);
	    break;
	case 'Binary':
	    Size = 4 + Obj.value.byteLength;
	    break;
	case 'Tuple':
	    Size = this.byte_size_tuple(Obj);
	    break;
	default:
	    if (Obj.constructor.toString().indexOf("Array") !== -1)
		Size = this.byte_size_array(Obj);
	    else
		Size = this.byte_size_associative_array(Obj);
	    break;
	}
	break;
    default: 
	throw ("bad object: " + Obj);
    }
    return 1+Size;
}

EiClass.prototype.atom_size = function (Name) {
    if ((Name.length < 256) && this.use_small_atom)
	return 1+Name.length;
    else
	return 2+Name.length;
}

EiClass.prototype.byte_size_tuple = function (Obj) {
    var i;
    var Size = (Obj.length < 256) ? 1 : 4;
    for (i = 0; i < Obj.length; i++)
	Size += this.byte_size_term(Obj.value[i]);
    return Size;
}

EiClass.prototype.byte_size_array = function (Obj) {
    var len = Obj.length;
    if (len == 0)
	return 1; // nil only
    else {
	var i;
	var Size = 4;  // length-bytes
	for (i = 0; i < len; i++)
	    Size += this.byte_size_term(Obj[i]);
	Size +=1; // nil byte
	return Size;
    }
}

EiClass.prototype.byte_size_associative_array = function (Obj) {
    var key, Size = 0, N = 0;

    for (key in Obj) {
	if (Obj.hasOwnProperty(key)) {
	    var len = key.length;
	    klen = (len < 256) ? 1+len : 2+len;
	    Size += 1+klen;  // (small)atom
	    Size += this.byte_size_term(Obj[key]);
	    N++;
	}
    }
    if (N == 0) {
	if (!this.use_map)
	    Size = 4;   // map size
	else
	    Size = 0    // nil
    }
    else {
	if (!this.use_map)
	    Size += N*2;  // add N small tuples
	Size += 4;  // list or map size
    }
    return Size;
}

// encode_xxx
//  encode the term into Array Buffer

EiClass.prototype.encode = function (Obj) {
    var sz = this.byte_size(Obj);
    var ab = new ArrayBuffer(sz);
    var dv = new DataView(ab);
    dv.setUint8(0, this.MAGIC);
    this.encode_term(Obj, dv, 1);
    return ab;
};

EiClass.prototype.encode_term = function (Obj,dv,pos) {
    switch(typeof(Obj)) {
    case 'string':
	if (Obj.length <= 65535) {
	    dv.setUint8(pos++, this.STRING);
	    dv.setUint16(pos, Obj.length);
	    pos += 2;
	    pos = this.string_to_bytes(Obj,dv,pos,Obj.length);
	}
	else {
	    dv.setUint8(pos++, this.LIST);
	    dv.setUint32(pos, Obj.length);
	    pos += 4;
	    for (i = 0; i < Obj.length; i++) {
		dv.setUint8(pos++, this.SMALL_INTEGER);
		dv.setUint8(pos++, Obj.charCodeAt(i));
	    }
	    dv.setUint8(pos++, this.NIL);
	}
	break;

    case 'boolean':
	if (Obj)
	    pos = this.encode_atom_string("true",dv,pos);
	else 
	    pos = this.encode_atom_string("false",dv,pos);
	break;

    case 'number':
	if ((Obj % 1 === 0)) {  // integer
	    if ((Obj >= 0) && (Obj < 256)) {
		dv.setUint8(pos, this.SMALL_INTEGER);
		dv.setUint8(pos+1, Obj);
		pos += 2;
	    }
	    else if ((Obj >= -134217728) && (Obj <= 134217727)) {
		dv.setUint8(pos, this.INTEGER);
		dv.setInt32(pos+1, Obj);
		pos += 5;
	    }
	    else if (Obj < 0) {
		dv.setUint8(pos, this.SMALL_BIG);
		dv.setUint8(pos+1, 4);
		dv.setUint8(pos+2, 0);  // negative
		// little endian encoded digits!
		dv.setUint32(pos+3, -Obj, true);  
		pos += 7;
	    }
	    else {
		dv.setUint8(pos, this.SMALL_BIG);
		dv.setUint8(pos+1, 4);
		dv.setUint8(pos+2, 1);
		// little endian encoded digits!
		dv.setUint32(pos+3, Obj, true);
		pos += 7;
	    }
	}
	else {
	    dv.setUint8(pos, this.NEW_FLOAT);
	    dv.setFloat64(pos+1, Obj, false);  // store float as big endian 64
	    pos += 9;
	}
	break;

    case 'object':
	switch(Obj.type) {
	case 'Atom':
	    pos = this.encode_atom_string(Obj.value,dv,pos);
	    break;

	case 'Binary': { // Obj.value MUST be an Uint8Array!
	    var i, len = Obj.value.byteLength;
	    dv.setUint8(pos, this.BINARY);
	    dv.setUint32(pos+1, len, false);
	    pos += 5;
	    for (i = 0; i < len; i++, pos++)
		dv.setUint8(pos, Obj.value[i]);
	    break;
	}

	case 'Tuple':
	    pos = this.encode_tuple(Obj,dv,pos);
	    break;
	default:
	    if (Obj.constructor.toString().indexOf("Array") !== -1)
		pos = this.encode_array(Obj,dv,pos);
	    else
		pos = this.encode_associative_array(Obj,dv,pos);
	}
	break;

    default: 
	throw ("bad object: " + Obj);
    }
    return pos;
};

EiClass.prototype.encode_atom_string = function (Name,dv,pos) {
    var len = Name.length;
    var i;
    if ((len < 256) && this.use_small_atom) {
	dv.setUint8(pos, this.SMALL_ATOM);
	dv.setUint8(pos+1, len);
	pos += 2;
    }
    else if (len < 65536) {
	dv.setUint8(pos, this.ATOM);
	dv.setUint16(pos+1, len, false);
	pos += 3;
    }
    else
	throw("bad atom: too big");
    return this.string_to_bytes(Name,dv,pos,len);
}


EiClass.prototype.encode_tuple = function (Obj,dv,pos) {
    var i;
    var N = Obj.length;
    if (N < 256) {
	dv.setUint8(pos, this.SMALL_TUPLE);
	dv.setUint8(pos+1, N);
	pos += 2;
    } else {
	dv.setUint8(pos, this.LARGE_TUPLE);
	dv.setUint32(pos+1, N, false);
	pos += 5;
    }
    for (i = 0; i < N; i++)
	pos = this.encode_term(Obj.value[i],dv,pos);
    return pos;
};

EiClass.prototype.encode_array = function (Obj,dv,pos) {
    var N = Obj.length;
    if (N > 0) {
	dv.setUint8(pos, this.LIST);
	dv.setUint32(pos+1, N, false);
	pos += 5;
	for (var i = 0; i < N; i++)
	    pos = this.encode_term(Obj[i],dv,pos);
    }
    dv.setUint8(pos,this.NIL);
    return pos+1;
};

// count number of keys
EiClass.prototype.assoc_num_keys = function (Obj) {
    if (!Object.keys) {
	var N = 0;
	var key;
	for (key in Obj) {
	    if (Obj.hasOwnProperty(key))
		N++;
	}
	return N;
    }
    return Object.keys(Obj).length;
};


EiClass.prototype.encode_associative_array = function (Obj,dv,pos) {
    var N = this.assoc_num_keys(Obj);
    if (N > 0) {
	var key;
	dv.setUint8(pos, this.use_map ? this.MAP : this.LIST);
	dv.setUint32(pos+1,N,false);
	pos += 5
	for (key in Obj) {
	    if (Obj.hasOwnProperty(key)) {
		if (!this.use_map) {
		    dv.setUint8(pos++, this.SMALL_TUPLE);
		    dv.setUint8(pos++, 2);
		}
		pos = this.encode_atom_string(key,dv,pos);
		pos = this.encode_term(Obj[key],dv,pos);
	    }
	}
    }
    if (!this.use_map)
	dv.setUint8(pos++,this.NIL);
    else if (this.use_map && (N == 0)) {  // special case for empty map
	dv.setUint8(pos, this.MAP);
	dv.setUint32(pos+1,0,false);
	pos += 5;
    }
    return pos;
};

//
// decode_size_xxx
//
//  Calculate the size of an external encoded term 
//  in the buffer
//

EiClass.prototype.decode_size = function (ab, pos) {
    var dv = new DataView(ab);
    if (dv.getUint8(pos) !== this.MAGIC) {
	throw ("badmagic");
    }
    return 1+this.decode_size_term(dv, pos+1);
};

EiClass.prototype.decode_size_term = function (dv,pos) {
    var Tag = dv.getUint8(pos++);
    var L = 0;

    switch (Tag) {
    case this.NIL: break;
    case this.SMALL_ATOM: L = 1+dv.getUint8(pos); break;
    case this.ATOM:       L = 2+dv.getUint16(pos,false); break;
    case this.BINARY:     L = 4+dv.getUint32(pos,false); break;
    case this.SMALL_INTEGER: L = 1; break;
    case this.INTEGER:	L = 4; break;
    case this.SMALL_BIG: L = 1+dv.getUint8(pos); break;
    case this.LARGE_BIG: L = 4+dv.getUint32(pos,false); break;
    case this.FLOAT:	L = 31; break;
    case this.NEW_FLOAT: L = 8; break;
    case this.STRING: L = 2+dv.getUint16(pos,false); break;
    case this.LIST:
	L = this.decode_size_seq(dv,pos+4,dv.getUint32(pos,false)+1,4);
	break;
    case this.SMALL_TUPLE:
	L = this.decode_size_seq(dv,pos+1,dv.getUint8(pos),1); 
	break;
    case this.LARGE_TUPLE: 
	L = this.decode_size_seq(dv,pos+4,dv.getUint32(pos,false),4);
	break;
    case this.MAP:
	L = this.decode_size_seq(dv,pos+4,2*dv.getUint32(pos,false),4);
	break;
    default: throw ("bad tag: " + Tag);
    }
    return L+1;
};

EiClass.prototype.decode_size_seq = function (dv,pos,len,Size) {
    var i;
    for (i = 0; i < len; i++) {
	var k = this.decode_size_term(dv,pos);
	pos += k;
	Size += k;
    }
    return Size;
}

//
//  Decode the Array Buffer
//

EiClass.prototype.decode = function (ab, pos) {
    var dv = new DataView(ab);
    if (dv.getUint8(pos) !== this.MAGIC) {
	throw ("badmagic");
    }
    return this.decode_term(dv, pos+1);
};

EiClass.prototype.decode_term = function (dv,pos) {
    var R,Tag = dv.getUint8(pos++);

    switch (Tag) {
    case this.NIL: 
	R = []; break;
    case this.SMALL_ATOM:
	R = this.decode_atom_bytes(dv,pos+1,dv.getUint8(pos)); break;
    case this.ATOM:
	R = this.decode_atom_bytes(dv,pos+2,dv.getUint16(pos,false)); break;
    case this.BINARY:
	R = this.decode_binary(dv,pos); break;
    case this.SMALL_INTEGER:
	R = dv.getUint8(pos); break;
    case this.INTEGER:
	R = dv.getInt32(pos,false); break;
    case this.SMALL_BIG:	
	R = this.decode_big_bytes(dv,pos+1,dv.getUint8(pos)); break;
    case this.LARGE_BIG:
	R = this.decode_big_bytes(dv,pos+4,dv.getUint32(pos,false)); break;
    case this.FLOAT:
	R = parseFloat(this.bytes_to_string(dv,pos,31)); break;
    case this.NEW_FLOAT:
	R = dv.getFloat64(pos, false); break;
    case this.STRING:
	R = this.bytes_to_string(dv,pos+2,dv.getUint16(pos,false)); break;
    case this.LIST:
	R = this.decode_list(dv,pos); break;
    case this.SMALL_TUPLE:
	R = this.decode_tuple(dv,pos+1,dv.getUint8(pos)); break;
    case this.LARGE_TUPLE:
	R = this.decode_tuple(dv,pos+4,dv.getUint32(pos,false)); break;
    case this.MAP:
	R = this.decode_map(dv,pos+4,dv.getUint32(pos,false)); break;
    default:
	throw ("badtag: " + Tag);
    }
    // console.debug("decode_term = " + R);
    return R;
};

EiClass.prototype.decode_atom_bytes = function (dv,pos,len) {
    var S = this.bytes_to_string(dv,pos,len);
    if (S === "true")
	return true;
    else if (S === "false")
	return false;
    return new this.atom(S);
};

EiClass.prototype.decode_binary = function (dv,pos) {
    var Size = dv.getUint32(pos, false);
    var Bin  = new Uint8Array(dv.buffer, pos+4, Size);
    return this.binary(Bin);
};

EiClass.prototype.decode_big_bytes = function (dv,pos,len) {
    var Sign = dv.getUint8(pos++);
    if (len == 4) {
	var Num = dv.getUint32(pos,false);
	if (Sign) return -Num;
	return Num;
    }
    throw ("bad number");
};

//
// Special hack for argument lists  ['array',64,65,66,67]
// to separate this in the erlang term_to_binary from a string!
// also make sure [array|T] is encoded as [array,array|T]
//
EiClass.prototype.decode_list = function (dv,pos) {
    var len, i, Arr = [];

    len = dv.getUint32(pos, false);
    pos += 4;
    // console.debug("decode_list len=" + len);
    for (i = 0; i < len; i++) {
	var Term = this.decode_term(dv,pos);
	var k   = this.decode_size_term(dv,pos);
	//console.debug("term (tag="+dv.getUint8(pos)+"["+ Term+ "] size = "+k);
	pos += k;
	if (!((i==0) && this.eqAtom(Term, "array")))
	    Arr.push(Term);
    }
    // console.debug("decode_list end");
    if (dv.getUint8(pos) != this.NIL) {  // improper list not allowed
	throw ("List does not end with NIL! [tag="+dv.getUint8(pos)+"]");
    }
    return Arr;
};

EiClass.prototype.decode_tuple = function (dv,pos,len) {
    var i, Arr = [];
    for (i = 0; i < len; i++) {
	var Term = this.decode_term(dv,pos);
	pos += this.decode_size_term(dv,pos);
	Arr.push(Term);
    }
    return new EiTuple(Arr);
};


EiClass.prototype.decode_map = function (dv,pos,len) {
    var key, value, i, Obj = new Object();
    for (i = 0; i < len; i++) {
	key = this.decode_term(dv,pos);
	pos += this.decode_size_term(dv,pos);
	value = this.decode_term(dv,pos);
	pos += this.decode_size_term(dv,pos);
	Obj[key.value] = value;  // key must be EiAtom! fixme test!
    }
    return Obj;
};


// Convert an array of bytes into a string.
EiClass.prototype.bytes_to_string = function (dv,pos,count) {
    var i, s = "";
    for (i = 0; i < count; i++,pos++)
	s += String.fromCharCode(dv.getUint8(pos));
    return s;
};

// Convert an array of bytes into a string.
EiClass.prototype.string_to_bytes = function (Obj,dv,pos,count) {
    var i;
    for (i = 0; i < count; i++, pos++)
	dv.setUint8(pos, Obj.charCodeAt(i));
    return pos;
};

// Pretty Print a byte-string in Erlang binary form.
EiClass.prototype.pp_bytes = function (Bin) {
    var i, s = "";
    for (i = 0; i < Bin.length; i++) {
	if (s !== "") {
	    s += ",";
	}
	s += "" + Bin[i];
    }
    return "<<" + s + ">>";
};

// Pretty Print a JS object in Erlang term form.
EiClass.prototype.pp = function (Obj) {
    return Obj.toString();
};

var Ei = new EiClass();
