'use strict';

var devices = (function (my) {

////////////////////////////////////////////////////////
// BluetoothManagerChrome

my.BluetoothManagerChrome = function() {
	devices.BluetoothManager.call(this);
	this.defaultAdapter = new my.BluetoothAdapterChrome();
}

my.BluetoothManagerChrome.prototype = core.inherit(devices.BluetoothManager.prototype);

// Closable
my.BluetoothManagerChrome.prototype.onClosing = function(callback) {
	this.defaultAdapter.close(callback);
}

my.BluetoothManagerChrome.prototype.getDefaultAdapter = function() {
	return this.defaultAdapter;
}

////////////////////////////////////////////////////////
// BluetoothAdapterChrome

my.BluetoothAdapterChrome = function() {
	devices.BluetoothAdapter.call(this);
	
	this.onAdapterStateChangedHandler = this.onAdapterStateChanged.bind(this);
	chrome.bluetooth.onAdapterStateChanged.addListener(this.onAdapterStateChangedHandler);

	this.onDeviceAddedHandler = this.onDeviceAdded.bind(this);
	chrome.bluetooth.onDeviceAdded.addListener(this.onDeviceAddedHandler);
	this.onDeviceChangedHandler = this.onDeviceChanged.bind(this);
	chrome.bluetooth.onDeviceChanged.addListener(this.onDeviceChangedHandler);
	this.onDeviceRemovedHandler = this.onDeviceRemoved.bind(this);
	chrome.bluetooth.onDeviceRemoved.addListener(this.onDeviceRemovedHandler);

	// BLE methods
	this.onServiceAddedHandler = this.onServiceAdded.bind(this);
	chrome.bluetoothLowEnergy.onServiceAdded.addListener(this.onServiceAddedHandler);
	this.onServiceChangedHandler = this.onServiceChanged.bind(this);
	chrome.bluetoothLowEnergy.onServiceChanged.addListener(this.onServiceChangedHandler);
	this.onServiceRemovedHandler = this.onServiceRemoved.bind(this);
	chrome.bluetoothLowEnergy.onServiceRemoved.addListener(this.onServiceRemovedHandler);

	this.onCharacteristicValueChangedHandler = this.onCharacteristicValueChanged.bind(this);
	chrome.bluetoothLowEnergy.onCharacteristicValueChanged.addListener(this.onCharacteristicValueChangedHandler);
}

my.BluetoothAdapterChrome.prototype = core.inherit(devices.BluetoothAdapter.prototype);

// Closable
my.BluetoothAdapterChrome.prototype.onClosing = function(callback) {
	chrome.bluetooth.onAdapterStateChanged.removeListener(this.onAdapterStateChangedHandler);

	chrome.bluetooth.onDeviceAdded.removeListener(this.onDeviceAddedHandler);
	chrome.bluetooth.onDeviceChanged.removeListener(this.onDeviceChangedHandler);
	chrome.bluetooth.onDeviceRemoved.removeListener(this.onDeviceRemovedHandler);

	chrome.bluetoothLowEnergy.onServiceAdded.removeListener(this.onServiceAddedHandler);
	chrome.bluetoothLowEnergy.onServiceChanged.removeListener(this.onServiceChangedHandler);
	chrome.bluetoothLowEnergy.onServiceRemoved.removeListener(this.onServiceRemovedHandler);

	chrome.bluetoothLowEnergy.onCharacteristicValueChanged.removeListener(this.onCharacteristicValueChangedHandler);

	core.Observable.prototype.onClosing.apply(this, [callback]);
}

my.BluetoothAdapterChrome.prototype.getKnownDevices = function(callback) {
	var self = this;

	chrome.bluetooth.getDevices(function(devices) {
		var chromeDevices = [];

		devices.forEach(function(device) {
			var chromeDevice = null;
			if (!self.devices.some(function(thisDevice) {
				if (thisDevice.apiDevice.deviceId === device.deviceId) {
					chromeDevice = thisDevice;
				}					return true;

				return false;
			})) {
				chromeDevice = new my.BluetoothDeviceChrome(self, device);
				self.devices.push(chromeDevice);
			}
			
			chromeDevices.push(chromeDevice);
		});

		callback && callback.apply(self, [chromeDevices]);
	});
}

my.BluetoothAdapterChrome.prototype.onAdapterStateChanged = function(state) {
	// TODO(jracle): implement
}

my.BluetoothAdapterChrome.prototype.onDeviceAdded = function(device) {
	var chromeDevices = this.devices.filter(function(thisDevice) {
		return thisDevice.apiDevice.deviceId === device.deviceId;
	});

	if (!chromeDevices) {
		var chromeDevice = new my.BluetoothDeviceChrome(self, device);
		this.devices.push(chromeDevice);
	} else {
		core.Log.warning("An existing device was found with id %s", device.deviceId);
	}
}

my.BluetoothAdapterChrome.prototype.onDeviceChanged = function(device) {
	// TOFIX(jracle): seems that device.connected logic is reversed
	var chromeDevices = this.devices.filter(function(thisDevice) {
		return thisDevice.apiDevice.deviceId === device.deviceId;
	});

	core.Contract.assert(chromeDevices, "Device not found.");
	// TODO(jracle): implement
}

my.BluetoothAdapterChrome.prototype.onDeviceRemoved = function(device) {
	var chromeDevices = this.devices.removeFirst(function(thisDevice) {
		return thisDevice.apiDevice.deviceId === device.deviceId;
	});
	
	core.Contract.assert(chromeDevices, "Device not found.");
}

my.BluetoothAdapterChrome.prototype.onServiceAdded = function(service) {
	// TODO(jracle)
}

my.BluetoothAdapterChrome.prototype.onServiceChanged = function(service) {
	// TODO(jracle)
}

my.BluetoothAdapterChrome.prototype.onServiceRemoved = function(service) {
	// TODO(jracle)
}

my.BluetoothAdapterChrome.prototype.onCharacteristicValueChanged = function(characteristic) {
	var chromeDevices = this.devices.filter(function(thisDevice) {
		return thisDevice.apiDevice.address == characteristic.service.deviceAddress;
	});

	core.Contract.assert(chromeDevices, "Device not found");
	core.Contract.assert(chromeDevices.length === 1, "Too many devices");

	chromeDevices[0].primaryGattServices.forEach(function(thisGattService) {
		thisGattService.characteristics.forEach(function(thisGattCharacteristic) {
			if (thisGattCharacteristic.apiGattCharacteristic.instanceId === characteristic.instanceId) {
				thisGattCharacteristic.value = characteristic.value;
			}
		});
	});
}

////////////////////////////////////////////////////////
// BluetoothDeviceChrome

my.BluetoothDeviceChrome = function(provider, apiDevice) {
	devices.BluetoothDevice.call(this, provider, apiDevice);
	this.id = this.apiDevice.address;
	this.address = this.apiDevice.address;
}

my.BluetoothDeviceChrome.prototype = core.inherit(devices.BluetoothDevice.prototype);

my.BluetoothDeviceChrome.prototype.connectGatt = function(callback) {
	chrome.bluetoothLowEnergy.connect(this.apiDevice.address, null, callback);
}

my.BluetoothDeviceChrome.prototype.disconnectGatt = function(callback) {
	chrome.bluetoothLowEnergy.disconnect(this.apiDevice.address, callback);
}

my.BluetoothDeviceChrome.prototype.getPrimaryGattServices = function(callback) {
	var self = this;

	chrome.bluetoothLowEnergy.getServices(self.apiDevice.address, function(gattServices) {
		var gattServicesChrome = [];
		gattServices.forEach(function(gattService) {
			var gattServiceChrome = null;
			if (!self.primaryGattServices.some(function(thisGattService) {
				if (thisGattService.apiGattService.instanceId === gattService.instanceId) {
					gattServiceChrome = thisGattService;
					return true;
				}
				return false;
			})) {
				gattServiceChrome = new my.BluetoothGattServiceChrome(self, gattService);
				self.primaryGattServices.push(gattServiceChrome);
			}
			
			gattServicesChrome.push(gattServiceChrome);
		});
		callback && callback.apply(self, [gattServicesChrome]);
	});
}

////////////////////////////////////////////////////////
// BluetoothGattServiceChrome

my.BluetoothGattServiceChrome = function(device, apiGattService) {
	devices.BluetoothGattService.call(this, device, apiGattService);
	this.id = this.apiGattService.instanceId;
}

my.BluetoothGattServiceChrome.prototype = core.inherit(devices.BluetoothGattService.prototype);

my.BluetoothGattServiceChrome.prototype.getCharacteristics = function(callback) {
	var self = this;

	chrome.bluetoothLowEnergy.getCharacteristics(self.apiGattService.instanceId, function(gattCharacteristics) {
		var gattCharacteristicsChrome = [];
		gattCharacteristics.forEach(function(gattCharacteristic) {
			var gattCharacteristicChrome = null;
			if (!self.characteristics.some(function(thisCharacteristic) {
				if (thisCharacteristic.apiGattCharacteristic.instanceId === apiGattCharacteristic.instanceId) {
					gattCharacteristicChrome = thisCharacteristic;
					return true;
				}
				return false;
			})) {
				gattCharacteristicChrome = new my.BluetoothGattCharacteristicChrome(self, gattCharacteristic);
				self.characteristics.push(gattCharacteristicChrome);
			}
			
			gattCharacteristicsChrome.push(gattCharacteristicChrome);
		});
		callback && callback.apply(self, [gattCharacteristicsChrome]);
	});
}

////////////////////////////////////////////////////////
// BluetoothGattCharacteristicChrome

my.BluetoothGattCharacteristicChrome = function(service, apiGattCharacteristic) {
	devices.BluetoothGattCharacteristic.call(this, service, apiGattCharacteristic);
	this.id = this.apiGattCharacteristic.instanceId;
	this.uuid = this.apiGattCharacteristic.uuid;
}

my.BluetoothGattCharacteristicChrome.prototype = core.inherit(devices.BluetoothGattCharacteristic.prototype);

my.BluetoothGattCharacteristicChrome.prototype.getDescriptors = function(callback) {
	var self = this;

	chrome.bluetoothLowEnergy.getDescriptors(self.apiGattCharacteristic.instanceId, function(gattDescriptors) {
		var gattDescriptorsChrome = [];
		gattDescriptors.forEach(function(gattDescriptor) {
			gattDescriptorsChrome.push(new my.BluetoothGattDescriptorChrome(self, gattDescriptor));
		});
		callback && callback.apply(self, [gattDescriptorsChrome]);
	});
}

my.BluetoothGattCharacteristicChrome.prototype.startNotifications = function(callback) {
	chrome.bluetoothLowEnergy.startCharacteristicNotifications(this.apiGattCharacteristic.instanceId, null, callback);
}

my.BluetoothGattCharacteristicChrome.prototype.stopNotifications = function(callback) {
	chrome.bluetoothLowEnergy.stopCharacteristicNotifications(this.apiGattCharacteristic.instanceId, callback);
}

my.BluetoothGattCharacteristicChrome.prototype.readValue = function(callback) {
	var self = this;

	chrome.bluetoothLowEnergy.readCharacteristicValue(this.apiGattCharacteristic.instanceId, function(characteristic) {
		var value = characteristic.value;
		self.value = value;
		callback && callback.apply(self, [value]);
	});
}

my.BluetoothGattCharacteristicChrome.prototype.writeValue = function(value, callback) {
	chrome.bluetoothLowEnergy.writeCharacteristicValue(this.apiGattCharacteristic.instanceId, value, callback);
}

////////////////////////////////////////////////////////
// BluetoothGattDescriptorChrome

my.BluetoothGattDescriptorChrome = function() {
	devices.BluetoothGattDescriptor.call(this, characteristic, apiGattDescriptor);
	this.uuid = this.apiGattDescriptor.uuid;
}

my.BluetoothGattDescriptorChrome.prototype = core.inherit(devices.BluetoothGattDescriptor.prototype);

return my;

}(devices || {}));
