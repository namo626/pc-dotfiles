'use strict';

var core = (function (my) {

////////////////////////////////////////////////////////
// DebuggerBase

my.DebuggerBase = function() {}

my.DebuggerBase.prototype.break = function() {
	throw new TypeError("Abstract method");
}

////////////////////////////////////////////////////////
// DebuggerWithStatement

my.DebuggerWithStatement = function() {
	my.DebuggerBase.call(this);
}

my.DebuggerWithStatement.prototype = core.inherit(my.DebuggerBase.prototype);

my.DebuggerWithStatement.prototype.break = function() {
	debugger;
}

////////////////////////////////////////////////////////
// Debugger

my.Debugger = {
	break: function() {
		var impl = core.Injector.resolve(function(debuggerImpl) {
			if (debuggerImpl) {
				debuggerImpl.break();
			}			
		});

		impl();
	}
}

return my;

}(core || {}));
