'use strict';

var core = (function () {

var my = {};

// http://javascript.info/tutorial/pseudo-classical-pattern
my.inherit = function(proto) {
	function F() {};
	F.prototype = proto;
	return new F;
}

// See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/setPrototypeOf#Browser_compatibility
// See also
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/setPrototypeOf
my.cast = function(object, proto) {
	if (Object.setPrototypeOf) {
		Object.setPrototypeOf(object, proto);
	} else {
		object.__proto__ = proto;
	}
}

return my;

}());
