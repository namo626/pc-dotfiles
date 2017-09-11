'use strict';

Object.prototype.getOwnPropertyNamesForValue = function(value) {
	var propNames = Object.getOwnPropertyNames(this);

	var propNamesForValue = [];
	for (var i=0; i<propNames.length; i++) {
		if ((this[propNames[i]] | value) === value) {
			propNamesForValue.push(propNames[i]);
		}
	}

	return propNamesForValue;
};

Object.prototype.getOwnPropertyNameForValue = function(value) {
	var propNames = Object.getOwnPropertyNames(this);

	for (var i=0; i<propNames.length; i++) {
		if (this[propNames[i]] === value) {
			return propNames[i];
		}
	}

	return "";
};

Object.prototype.transferOwnProperties = function(other) {
	var propNames = Object.getOwnPropertyNames(other);

	for (var i=0; i<propNames.length; i++) {
		this[propNames[i]] = other[propNames[i]];
	}
}
