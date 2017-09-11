'use strict';

var devices = (function (my) {

my.Device = function(provider, apiDevice, parent, connected, properties) {
	core.Observable.call(this);
	core.Contract.assert(provider instanceof devices.DeviceProvider,
		"Expected DeviceProvider.");
	this.provider  = provider;
	this.apiDevice = apiDevice;

	this.children = new core.ObservableArray();

	if (properties) {
		this.transferOwnProperties(properties);
	}

	if (parent) {
		core.Contract.assert(parent instanceof devices.Device, "Expected Device.");
		this.parent = parent;
		parent.children.push(this);
	} else {
		this.parent = null;
	}

	// Observable properties
	this.registerProperty('connected');
	this.connected = typeof connected === "undefined" ? false : connected;
}

my.Device.prototype = core.inherit(core.Observable.prototype);

// Closable
my.Device.prototype.onClosing = function(callback) {
	if (this.parent) {
		this.parent.children.remove(this);
	}

	core.Observable.prototype.onClosing.apply(this, [callback]);
}

////////////////////////////////////////////////////////
// DeviceFsm

my.DeviceFsm = function(device) {
	//Contract.assert(typeof device === 'Device', "Expected Device.");
	//Contract.assert(device instanceof devices.Device, "Expected Device.");
	core.StateMachine.call(this);
	this.device = device;
}

my.DeviceFsm.prototype = core.inherit(core.StateMachine.prototype);

return my;

}(devices || {}));
