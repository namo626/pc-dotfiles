'use strict';

var core = (function (my) {

////////////////////////////////////////////////////////
// ContractBase

my.ContractBase = function() {}

my.ContractBase.prototype.assert = function(expression, object) {
	throw new TypeError("Abstract method");
}

////////////////////////////////////////////////////////
// ContractWithConsole

my.ContractWithConsole = function() {
	my.ContractBase.call(this);
}

my.ContractWithConsole.prototype = core.inherit(my.ContractBase.prototype);

my.ContractWithConsole.prototype.assert = function(expression, object) {
	console.assert && console.assert(expression, object);
}

////////////////////////////////////////////////////////
// Contract

my.Contract = {
	assert: function(expression, object) {
		var impl = core.Injector.resolve(function(contract) {
			if (contract) {
				contract.assert(expression, object);
			}
		});

		impl();
	}
}

return my;

}(core || {}));
