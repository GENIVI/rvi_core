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
// Base64 encoder/decoder (on strings)
//
function Base64Class() {
    this._keyStr = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";
};

// Base64 encoder
Base64Class.prototype.encode = function (input) {
    var output = "";
    var chr1, chr2, chr3, enc1, enc2, enc3, enc4;
    var i = 0;

    while (i < input.length) {
	chr1 = input.charCodeAt(i++);
	chr2 = input.charCodeAt(i++);
	chr3 = input.charCodeAt(i++);
	    
	enc1 = chr1 >> 2;
	enc2 = ((chr1 & 3) << 4) | (chr2 >> 4);
	enc3 = ((chr2 & 15) << 2) | (chr3 >> 6);
	enc4 = chr3 & 63;
	    
	if (isNaN(chr2)) {
	    enc3 = enc4 = 64;
	} else if (isNaN(chr3)) {
	    enc4 = 64;
	}
	output = output +
	    this._keyStr.charAt(enc1) + this._keyStr.charAt(enc2) +
	    this._keyStr.charAt(enc3) + this._keyStr.charAt(enc4);
    }
    return output;
};
    
// Base64 decoder
Base64Class.prototype.decode = function (input) {
    var output = "";
    var chr1, chr2, chr3;
    var enc1, enc2, enc3, enc4;
    var i = 0;
	
    input = input.replace(/[^A-Za-z0-9\+\/\=]/g, "");
	
    while (i < input.length) {
	enc1 = this._keyStr.indexOf(input.charAt(i++));
	enc2 = this._keyStr.indexOf(input.charAt(i++));
	enc3 = this._keyStr.indexOf(input.charAt(i++));
	enc4 = this._keyStr.indexOf(input.charAt(i++));
	    
	chr1 = (enc1 << 2) | (enc2 >> 4);
	chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
	chr3 = ((enc3 & 3) << 6) | enc4;
	    
	output = output + String.fromCharCode(chr1);
	    
	if (enc3 != 64) {
	    output = output + String.fromCharCode(chr2);
	}
	if (enc4 != 64) {
	    output = output + String.fromCharCode(chr3);
	}
    }
    return output;
};

var Base64 = new Base64Class();
