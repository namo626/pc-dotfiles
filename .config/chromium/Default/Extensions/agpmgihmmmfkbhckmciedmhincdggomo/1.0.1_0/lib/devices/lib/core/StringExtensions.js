'use strict';

// TOFIX(jracle): flawed logic

String.prototype.padLeft = function(padString, length) {
	var str = this;
    while (str.length < length)
        str = padString + str;
    return str;
}

String.prototype.padRight = function(padString, length) {
	var str = this;
    while (str.length < length)
        str = str + padString;
    return str;
}

String.prototype.capitalize = function() {
    return this.toLowerCase().replace( /\b./g, function(a) {
    	return a.toUpperCase();
    });
}
