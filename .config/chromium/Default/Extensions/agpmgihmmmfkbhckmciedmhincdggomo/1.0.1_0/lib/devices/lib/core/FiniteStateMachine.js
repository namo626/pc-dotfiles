'use strict';

var core = (function (my) {

// Finite State Machine

////////////////////////////////////////////////////////
// StateMachine

my.StateMachine = function() {
	core.Observable.call(this);
	this.registerProperty('state');
}

my.StateMachine.prototype = core.inherit(core.Observable.prototype);

my.StateMachine.prototype.setState = function(newState) {
	var reEnterState = this.state && this.state === newState;

	if (this.state) {
		this.state.stateExit(reEnterState);
	}

	this.state = newState;

	if (this.state) {
		var args = Array.prototype.slice.call(arguments, 1);
		this.state.stateEntry.apply(this.state, args);
	}
}

////////////////////////////////////////////////////////
// State

my.State = function(stateMachine) {
	core.Contract.assert(stateMachine instanceof core.StateMachine,
		"Expected StateMachine.");
	core.Observable.call(this);
	this.stateMachine = stateMachine;
}

my.State.prototype = core.inherit(core.Observable.prototype);

my.State.prototype.stateEntry = function() {
	// Overridable
}

my.State.prototype.stateExit = function() {
	// Overridable
}

return my;

}(core || {}));
