'use strict';

var devices = (function (my) {

////////////////////////////////////////////////////////
// REPORT_ID

my.REPORT_ID = {
	ALL               : 0x00, // Reserved
	KEYBOARD          : 0x01, // Standard-keyboard keys
	MOUSE             : 0x02, // Mouse
	CONSUMER_KEYS     : 0x03, // Consumer control keys
	POWER             : 0x04, // Power keys
	MEDIACENTER       : 0x08, // Microsoft MediaCenter keys (vendor specific)
	HIDPP_SHORT       : 0x10, // Short HID++ messages (7 bytes)
	HIDPP_LONG        : 0x11, // Long HID++ messages (20 bytes)
	HIDPP_VERY_LONG   : 0x12, // Very-long HID++ messages (64 bytes)
	HIDPP_EXTRA_LONG  : 0x13, // Extra-long HID++ messages (111 bytes)
	DJ_BUS_ENUM_SHORT : 0x20, // DJ bus enumerator short messages
	DJ_BUS_ENUM_LONG  : 0x21
}

////////////////////////////////////////////////////////
// HidManager

my.HidManager = function() {
	devices.DeviceProvider.call(this);
	this.worker  = null;
}

my.HidManager.prototype = core.inherit(devices.DeviceProvider.prototype);

// Closable
my.HidManager.prototype.onClosing = function(callback) {
	this.stop();
	devices.DeviceProvider.prototype.onClosing.apply(this, [callback]);
}

my.HidManager.prototype.start = function(devicesFilter) {
	core.Contract.assert(devicesFilter instanceof Array, "Expected Array.")
	var self = this;

	self.stop();

	var getDeviceIds = function(devices) {
		var deviceIds = [];
		if (devices) {
			for (var i = 0; i < devices.length; i++) {
				deviceIds.push(devices[i].deviceId);
			}
		}
		return deviceIds;
	};

	var getDevices = function(devicesSource, deviceIds) {
		var devices = [];
		if (devicesSource) {
			for (var i = 0; i < devicesSource.length; i++) {
				for (var j = 0; j < deviceIds.length; j++) {
					if (devicesSource[i].deviceId === deviceIds[j]) {
						devices.push(devicesSource[i]);
						break;
					}
				}
			}
		}
		return devices;
	};

	var onHidDeviceAdded = function(hidDevice) {
		// Legacy HID++ usages (usage page 0xFF00)
		var hidppUsagesOld = hidDevice.collections.filter(function(collection) {
			return collection.usagePage === devices.HID_USAGE_PAGE.HIDPP_OLD;
		}).map(function(collection) {
			return collection.usage;
		});

		// New HID++ usages (usage page 0xFF43)
		var hidppUsagesNew = hidDevice.collections.filter(function(collection) {
			return collection.usagePage === devices.HID_USAGE_PAGE.HIDPP_NEW;
		}).map(function(collection) {
			return collection.usage;
		})

		// Not a HID++ device?
		if (hidppUsagesOld.length == 0 && hidppUsagesNew.length == 0) {
			return;
		}

		var hidppDevices = self.devices.filter(function(device) {
			return device !== hidDevice && device.isHidpp
				&& device.vendorId  == hidDevice.vendorId
				&& device.productId == hidDevice.productId;
		});

		var hidppUsages   = null;
		var missingUsages = null;
		
		if (hidppUsagesOld.length > 0) {
			hidppUsages = hidppUsagesOld;
			// missingUsages = ? we have no clue
			// TODO: infer a few missingUsages by pid
			// (e.g. unifying receiver = HID++ short, long, DJ)

		} else if (hidppUsagesNew.length > 0) {
			var expectedHidppUsagesField = (hidppUsagesNew[0] & 0xFF00) >> 8;
			var expectedHidppUsages = [];

			var combinedHidppUsagesField =  hidppUsagesNew[0] & 0x00FF;
			var combinedHidppUsages = [];

			for (var key in devices.HID_USAGE_ID.HID_PLUS_PLUS) {
				var hidppUsage = devices.HID_USAGE_ID.HID_PLUS_PLUS[key];

				if ((expectedHidppUsagesField & hidppUsage) == hidppUsage) {
					expectedHidppUsages.push(hidppUsage);
				}

				if ((combinedHidppUsagesField & hidppUsage) == hidppUsage) {
					combinedHidppUsages.push(hidppUsage);
				}
			}

			var usagesDelta = combinedHidppUsages.compareTo(expectedHidppUsages);

			hidppUsages   = combinedHidppUsages;
			missingUsages = usagesDelta.added;
		}

		var incompleteDevices = hidppDevices.filter(function(device) {
			if (device.missingUsages) {
				var comparison = device.missingUsages.compareTo(hidppUsages);
				return comparison.same.length > 0;

			} else {
				var comparison = device.hidppUsages.compareTo(hidppUsages);
				return comparison.same.length == 0;
			}
		});

		var hidppDevice = null;

		if (incompleteDevices.length === 0) {
			hidppDevice = new devices.HidppDevice(self);
			hidppDevice.connected     = true;
			hidppDevice.vendorId      = hidDevice.vendorId;
			hidppDevice.productId     = hidDevice.productId;
			hidppDevice.index         = devices.HIDPP_DEVICE_INDEX.RECEIVER;
			hidppDevice.hidppUsages   = hidppUsages;
			hidppDevice.missingUsages = missingUsages;
			self.devices.push(hidppDevice);

			hidppDevice.pip = new devices.HidppPip(hidppDevice);
			hidppDevice.pip.getProtocol(function(protocol) {
				hidppDevice.pip.close();
				hidppDevice.pip = null;
				hidppDevice.protocol = protocol;
				protocol.start();
			});

		} else {
			// Assumption here
			hidppDevice = incompleteDevices[0];
			hidppDevice.hidppUsages = hidppDevice.hidppUsages
				.concat(hidppUsages);
		}

		hidDevice.parent = hidppDevice;
		hidppDevice.children.push(hidDevice);
	};

	var onDeviceRemoved = function(device, onRemoving) {
		if (onRemoving) {
			core.Contract.assert(typeof onRemoving === 'function'
				&& onRemoving, "Expected function");

			onRemoving();
		}

		self.devices.remove(device);

		device.close(function() {
			if (device.parent && device.parent.children.length === 0) {
				onDeviceRemoved(device.parent);
			}
		});
	};

	var onHidDeviceRemoved = function(hidDevice) {
		onDeviceRemoved(hidDevice, function() {
			if (hidDevice.parent && hidDevice.parent.isHidpp) {
				var hidppDevices = hidDevice.parent.children.removeWhere(
					function(dev) { return dev.isHidpp; }
				);

				hidppDevices.forEach(function(hidppDevice) {
					onDeviceRemoved(hidppDevice);
				});
			}
		});
	};

	var deviceCache = {};
	var updateDeviceList = function() {
		var updateDeviceListByPid = function(filterIndex) {
			var deviceFilter = devicesFilter[filterIndex];

			if (self.restarting) {
				deviceCache = {};
				self.worker = setTimeout(updateDeviceList, 100);
				return;
			}

			self.getApiDevices(deviceFilter, function(apiDevices) {
				var cachedDevices = deviceCache[filterIndex];

				var comparison = getDeviceIds(cachedDevices)
					.compareTo(getDeviceIds(apiDevices));

				getDevices(cachedDevices, comparison.removed)
						.forEach(function(apiDevice) {
					var hidDevices = self.devices.filter(function(dev) {
						return dev instanceof devices.HidDevice
							&& dev.apiDevice.deviceId === apiDevice.deviceId;
					});

					//core.Contract.assert(hidDevices.length == 1);
					if (hidDevices.length == 1) {
						onHidDeviceRemoved(hidDevices[0]);
					}
				});

				getDevices(apiDevices, comparison.added)
						.forEach(function(apiDevice) {
					var hidDevice = self.createHidDevice(apiDevice);
					self.devices.push(hidDevice);

					onHidDeviceAdded(hidDevice);
				});

				deviceCache[filterIndex] = apiDevices;

				filterIndex++;
				if (filterIndex < devicesFilter.length) {
					updateDeviceListByPid(filterIndex);
				} else {
					self.worker = setTimeout(updateDeviceList, 1000);
				}
			});
		};

		if (devicesFilter.length > 0) {
			updateDeviceListByPid(0);
		}
	};

	updateDeviceList();
}

my.HidManager.prototype.stop = function() {
	if (this.worker && !this.restarting) {
		clearTimeout(this.worker);
		this.worker = null;
	}
}

my.HidManager.prototype.restart = function(callback) {
	var self = this;

	self.restarting = true;
	core.Closable.prototype.close.apply(self, [function() {

		var restartAsync = function()  {
			self.closed = false;
			self.restarting = false;
			callback && callback.apply(self);
		}

		setTimeout(restartAsync, 1000);
	}]);
}

my.HidManager.prototype.getWirelessDevice = function(parent, deviceIndex) {
	core.Contract.assert(parent instanceof devices.Device, "Expected Device");
	var wirelessDevice = null;
	this.devices.some(function(device) {
		if (device.parent === parent && device.index === deviceIndex) {
			wirelessDevice = device;
			return true;
		}
		return false;
	});

	return wirelessDevice;
}

my.HidManager.prototype.onWirelessDeviceArrival = function(
		parent, deviceIndex, connected, properties) {
	var wirelessDevice = this.getWirelessDevice(parent, deviceIndex);

	if (wirelessDevice) {
		wirelessDevice.connected = connected;

	} else {
		properties.index = deviceIndex;
		wirelessDevice = new devices.HidppDevice(this, parent, connected, properties);
		this.devices.push(wirelessDevice);
	}

	if (connected && !wirelessDevice.protocol) {
		var pip = new devices.HidppPip(wirelessDevice, deviceIndex);
		pip.getProtocol(function(protocol) {
			pip.close();
			pip = null;
			protocol.start();
		});
	}
}

my.HidManager.prototype.onWirelessDeviceDisconnect = function(
		parent, deviceIndex, disconnectType) {
	var wirelessDevice = this.getWirelessDevice(parent, deviceIndex);

	if (wirelessDevice) {
		// TOFIX(jracle): code duplication (cf OnDeviceRemoved
		this.devices.remove(wirelessDevice);
		wirelessDevice.close();
	}
}

my.HidManager.prototype.getApiDevices = function(devicesFilter, callback) {
	throw new TypeError("Abstract method");
}

my.HidManager.prototype.createHidDevice = function(apiDevice) {
	throw new TypeError("Abstract method");
}

////////////////////////////////////////////////////////
// HidDevice

my.HidDevice = function(provider, apiDevice, parent) {
	devices.Device.call(this, provider, apiDevice, parent);
	this.onPropertyChangedHandler = this.onPropertyChanged.bind(this);
	this.observePropertyChange(this.onPropertyChangedHandler);
}

my.HidDevice.prototype = core.inherit(devices.Device.prototype);

// Closable...
my.HidDevice.prototype.onClosing = function(callback) {
	var self = this;
	self.unobservePropertyChange(self.onPropertyChangedHandler);
	self.disconnect(function() {
		devices.Device.prototype.onClosing.apply(self, [callback]);
	});
}

my.HidDevice.prototype.connect = function(callback) {
	throw new TypeError("Abstract method");
}

my.HidDevice.prototype.disconnect = function(callback) {
	throw new TypeError("Abstract method");
}

my.HidDevice.prototype.receive = function(callback) {
	throw new TypeError("Abstract method");
}

my.HidDevice.prototype.send = function(reportId, payload, callback) {
	throw new TypeError("Abstract method");
}

my.HidDevice.prototype.onPropertyChanged = function(name, value) {
	var self = this;

	var readAsync = function() {
		self.receive(function(success, reportId, data) {
		  if (success) {
			  var dataIn = new Uint8Array(data.byteLength + 1);
  			dataIn[0] = reportId;
  			var dataView = new DataView(data);
  			for (var i=0; i<data.byteLength; ++i) {
  				dataIn[i+1] = dataView.getUint8(i);
  			}
  
  			core.Log.debug("[IN ] %s", dataIn.buffer.toStringEx());
  			self.raiseEvent('newReport', dataIn.buffer);
  
  			readAsync();
		  }
		});
	};

	if (name === 'connected' && value === true) {
		readAsync();
	}
}

////////////////////////////////////////////////////////
// HidppDevice

my.HidppDevice = function(provider, parent, connected, properties) {
	devices.Device.call(this, provider, null, parent, connected, properties);
	this.isHidpp = true;

	// Observable properties
	this.registerProperty('protocol');

	this.registerProperty('isLocked');
	this.isLocked = true;

	this.registerProperty('active');
	this.active = false;

	var self = this;
	self.onChildrenChangedHandler = self.onChildrenChanged.bind(self);
	self.registerProperty('hidppChildren', function() {
		self.children.observeCollectionChanged(self.onChildrenChangedHandler);
		self['hidppChildren'] = self.children.filter(function(device) {
			return device.isHidpp;
		});
	});
}

my.HidppDevice.prototype = core.inherit(devices.Device.prototype);

// Closable
my.HidppDevice.prototype.onClosing = function(callback) {
	var self = this;

	self.children.unobserveCollectionChanged(self.onChildrenChangedHandler);

	if (self.protocol) {
		self.protocol.close(function() {
			devices.Device.prototype.onClosing.apply(self, [callback]);
		});
	} else {
		devices.Device.prototype.onClosing.apply(self, [callback]);
	}
}

my.HidppDevice.prototype.onChildrenChanged = function(change) {
	this['hidppChildren'] = this.children.filter(function(device) {
		return device.isHidpp;
	});
}

return my;

}(devices || {}));
