'use strict';

var core = (function (my) {

// Observer-Observable design pattern
// - Publish/Subscribe mechanism
// - Synchronous property/event change notifications
// See also Object.watch() in gecko (not standard)
my.Observable = function() {
	core.Closable.call(this);
	this.properties = {};
	this.propertyObservers = [];
	this.events = {};
	this.eventObservers = {};
}

my.Observable.prototype = core.inherit(core.Closable.prototype);

// Closable
my.Observable.prototype.onClosing = function(callback) {
	// TODO close 'closable' properties ??
	//this.properties = {};
	this.propertyObservers = [];
	this.events = {};
	this.eventObservers = {};
	core.Closable.prototype.onClosing.apply(this, [callback]);
}

// Observable properties :

// callback = function(name, value)
my.Observable.prototype.observePropertyChange = function(callback) {
	core.Contract.assert(typeof callback == 'function'
		&& callback, "Expected function.");
	this.propertyObservers.push(callback);
}

my.Observable.prototype.unobservePropertyChange = function(callback) {
	core.Contract.assert(typeof callback == 'function'
		&& callback, "Expected function.");
	this.propertyObservers.remove(callback);
}

my.Observable.prototype.registerProperty = function(name, getter) {
	core.Contract.assert(typeof name === "string",
		"Expected string, got " + typeof name);

	var self = this;
	var getting = false;
	Object.defineProperty(self, name, {
		get: function() {
			if (!self.properties[name] && getter && !getting) {
				getting = true;
				var synchronousResult = getter.apply(self);
				if (synchronousResult) {
					self[name] = synchronousResult;
				}
			}
			return self.properties[name];
		},
		set: function(value) {
			if (self.properties[name] != value) {
				self.properties[name] = value;
				// Notify observers
				for (var i=0; i<self.propertyObservers.length; i++) {
					self.propertyObservers[i](name, value);
				}
			}
		}
	});
}

// Parameters:
// - String name : property name
// - function(value) callback
// - (optional) integer timeoutMs: wait timeout in milliseconds
// - (optional) function timeoutCallback
my.Observable.prototype.waitProperty = function(
		name, valueCallback, timeoutMs, timeoutCallback) {
	core.Contract.assert(typeof name === "string",
		"Expected string, got " + typeof name);
	core.Contract.assert(typeof valueCallback == 'function'
		&& valueCallback, "Expected function");
	var self = this;

	var value = self[name];
	if (value) {
		valueCallback && valueCallback.apply(self, [value]);

	} else {
		var waitTimeMs = 0;
		var doGetProperty = function() {
			value = self[name];

			if (value) {
				valueCallback && valueCallback.apply(self, [value]);

			} else {
				waitTimeMs++;
				if (waitTimeMs == timeoutMs) {
					timeoutCallback && timeoutCallback.apply(self);

				} else {
					setTimeout(doGetProperty, 1);
				}
			}
		};

		doGetProperty();
	}
}

// Observable events :

// callback = function(eventArgs...)
my.Observable.prototype.observeEvent = function(name, callback) {
	core.Contract.assert(typeof callback == 'function'
		&& callback, "Expected function.");
	if (!this.eventObservers[name]) {
		this.eventObservers[name] = [];
	}
	this.eventObservers[name].push(callback);
	this.onObserveEvent(name, callback);
}

my.Observable.prototype.onObserveEvent = function(name, callback) {
	// Overridable
}

my.Observable.prototype.unobserveEvent = function(name, callback) {
	core.Contract.assert(typeof callback == 'function'
		&& callback, "Expected function.");
	if (this.eventObservers[name]) {
		this.eventObservers[name].remove(callback);
	}
}

// Parameters : name, eventArgs (implicit)
my.Observable.prototype.raiseEvent = function(name) {
	core.Contract.assert(typeof name === "string",
		"Expected string, got " + typeof name);
	var eventArgs = Array.prototype.slice.call(arguments, 1);
	// Notify observers
	if (this.eventObservers[name]) {
		for (var i=0; i<this.eventObservers[name].length; i++) {
			var eventHandler = this.eventObservers[name][i];
			eventHandler.apply(this, eventArgs);
		}
	}
}

return my;

}(core || {}));
