'use strict';

var devices = (function (my) {

my.Protocol = function(device) {
	core.Contract.assert(device instanceof devices.Device, "Expected Device.");
	core.Observable.call(this);
	this.device   = device;
	this.starting = false;
	this.started  = false;

	if (!this.isBootProtocol) {
		device.protocol = this;
	}
}

my.Protocol.prototype = core.inherit(core.Observable.prototype);

my.Protocol.prototype.start = function(callback) {
	if (callback) {
		core.Contract.assert(typeof callback === 'function'
			&& callback, "Expected function");
	}

	if (this.starting || this.started) {
		return;
	}

	this.starting = true;
	this.onStarting(callback);
	this.started = true;
}

// Overridable
my.Protocol.prototype.onStarting = function(callback) {
	this.starting = false;
	callback && callback();
}

return my;

}(devices || {}));
