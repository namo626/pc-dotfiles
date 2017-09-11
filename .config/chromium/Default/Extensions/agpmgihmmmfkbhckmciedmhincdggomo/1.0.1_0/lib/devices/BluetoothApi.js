'use strict';

var devices = (function (my) {

////////////////////////////////////////////////////////
// BluetoothManager

my.BluetoothManager = function() {
	core.Observable.call(this);
}

my.BluetoothManager.prototype = core.inherit(core.Observable.prototype);

my.BluetoothManager.prototype.getDefaultAdapter = function() {
	throw new TypeError("Abstract method");
}

////////////////////////////////////////////////////////
// BluetoothAdapterState

// TODO(jracle): implement

////////////////////////////////////////////////////////
// BluetoothAdapter

my.BluetoothAdapter = function() {
	devices.DeviceProvider.call(this);
}

my.BluetoothAdapter.prototype = core.inherit(devices.DeviceProvider.prototype);

// Returns devices which are bonded
// and the ones found in last inquiry process
my.BluetoothAdapter.prototype.getKnownDevices = function(callback) {
	throw new TypeError("Abstract method");
}

////////////////////////////////////////////////////////
// BluetoothDevice

// TODO: BluetoothGatt(Profile) class?

my.BluetoothDevice = function(provider, apiDevice) {
	devices.Device.call(this, provider, apiDevice);
	this.primaryGattServices = new core.ObservableArray();
}

my.BluetoothDevice.prototype = core.inherit(devices.Device.prototype);

my.BluetoothDevice.prototype.connectGatt = function(callback) {
	throw new TypeError("Abstract method");
}

my.BluetoothDevice.prototype.disconnectGatt = function(callback) {
	throw new TypeError("Abstract method");
}

my.BluetoothDevice.prototype.getPrimaryGattServices = function() {
	throw new TypeError("Abstract method");
}

////////////////////////////////////////////////////////
// BluetoothGattService

my.BluetoothGattService = function(device, apiGattService) {
	core.Observable.call(this);
	this.device = device;
	this.apiGattService = apiGattService;
	this.characteristics = new core.ObservableArray();
	this.includedServices = new core.ObservableArray();
}

my.BluetoothGattService.prototype = core.inherit(core.Observable.prototype);

my.BluetoothGattService.prototype.getCharacteristics = function(callback) {
	throw new TypeError("Abstract method");
}

////////////////////////////////////////////////////////
// BluetoothGattCharacteristic

my.BluetoothGattCharacteristic = function(service, apiGattCharacteristic) {
	core.Observable.call(this);
	this.service = service;
	this.apiGattCharacteristic = apiGattCharacteristic;
	this.registerProperty('value');
}

my.BluetoothGattCharacteristic.prototype = core.inherit(core.Observable.prototype);

my.BluetoothGattCharacteristic.prototype.getDescriptors = function(callback) {
	throw new TypeError("Abstract method");
}

my.BluetoothGattCharacteristic.prototype.startNotifications = function(callback) {
	throw new TypeError("Abstract method");
}

my.BluetoothGattCharacteristic.prototype.stopNotifications = function(callback) {
	throw new TypeError("Abstract method");
}

my.BluetoothGattCharacteristic.prototype.readValue = function(callback) {
	throw new TypeError("Abstract method");
}

my.BluetoothGattCharacteristic.prototype.writeValue = function(value, callback) {
	throw new TypeError("Abstract method");
}

////////////////////////////////////////////////////////
// BluetoothGattDescriptor

my.BluetoothGattDescriptor = function(characteristic, apiGattDescriptor) {
	core.Observable.call(this);
	this.characteristic = characteristic;
	this.apiGattDescriptor = apiGattDescriptor;
}

my.BluetoothGattDescriptor.prototype = core.inherit(core.Observable.prototype);

return my;

}(devices || {}));
