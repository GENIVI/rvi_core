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
// UTF8 encoder/decoder
//
function UTF8Class() {
};

// method for UTF-8 encoding
UTF8Class.prototype.encode = function (string) {
    string = string.replace(/\r\n/g,"\n");
    var utftext = "";
	
    for (var n = 0; n < string.length; n++) {
	var c = string.charCodeAt(n);
	if (c < 128) {
	    utftext += String.fromCharCode(c);
	}
	else if((c > 127) && (c < 2048)) {
	    utftext += String.fromCharCode((c >> 6) | 192);
	    utftext += String.fromCharCode((c & 63) | 128);
	}
	else {
	    utftext += String.fromCharCode((c >> 12) | 224);
	    utftext += String.fromCharCode(((c >> 6) & 63) | 128);
	    utftext += String.fromCharCode((c & 63) | 128);
	}
    }
    return utftext;
};
    
// method for UTF-8 decoding
UTF8Class.prototype.decode = function(utftext) {
    var string = "";
    var i = 0;
    var c = c1 = c2 = 0;
	
    while (i < utftext.length) {
	c = utftext.charCodeAt(i);
	if (c < 128) {
	    string += String.fromCharCode(c);
	    i++;
	}
	else if((c > 191) && (c < 224)) {
	    c2 = utftext.charCodeAt(i+1);
	    string += String.fromCharCode(((c & 31) << 6) | (c2 & 63));
	    i += 2;
	}
	else {
	    c2 = utftext.charCodeAt(i+1);
	    c3 = utftext.charCodeAt(i+2);
	    string += String.fromCharCode(((c & 15) << 12) | ((c2 & 63) << 6) | (c3 & 63));
	    i += 3;
	}
    }
    return string;
};

var UTF8 = new UTF8Class();
