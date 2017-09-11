'use strict';

var devices = (function (my) {

////////////////////////////////////////////////////////
// HIDPP_REPORT_SIZE

my.HIDPP_REPORT_SIZE = {
	SHORT     : 7,
	LONG      : 20,
	VERY_LONG : 64,
	EXTRA_LONG: 111,
}

////////////////////////////////////////////////////////
// HIDPP_DEVICE_INDEX

my.HIDPP_DEVICE_INDEX = {
	CORDED_DEVICE : 0x00, // Corded device
	DEVICE1       : 0x01, // Device 1
	DEVICE2       : 0x02, // Device 2
	DEVICE3       : 0x03, // Device 3
	DEVICE4       : 0x04, // Device 4
	DEVICE5       : 0x05, // Device 5
	DEVICE6       : 0x06, // Device 6
	RECEIVER      : 0xFF, // Receiver

	toString : function(value) {
		return this.getOwnPropertyNameForValue(value)
			.replace(/_/g, " ").capitalize();
	},

	getRapHubIndex : function(pid) {
		var category = devices.LOGITECH_PRODUCT_CATEGORY
			.fromPid(pid);
		var connectionType = devices.LOGITECH_PRODUCT_CONNECTION_TYPE
			.fromCategory(category);

		if (category == devices.LOGITECH_PRODUCT_CATEGORY.Transceiver) {
			return my.HIDPP_DEVICE_INDEX.RECEIVER;

		} else {
			return my.HIDPP_DEVICE_INDEX.CORDED_DEVICE;
		}
	}
}

////////////////////////////////////////////////////////
// HidppProtocol

my.HidppProtocol = function(device) {
	devices.TransactionProtocol.call(this, device);

	this.onLinkBrokenHandler = this.onLinkBrokenHandler.bind(this);
	this.onNewReportHandler = this.onNewReport.bind(this);
	this.listen(this.device.children);

	this.onDeviceChildrenChangedHandler = this.onDeviceChildrenChanged.bind(this);
	this.device.children.observeCollectionChanged(this.onDeviceChildrenChangedHandler);
}

my.HidppProtocol.prototype = core.inherit(devices.TransactionProtocol.prototype);

// Closable
my.HidppProtocol.prototype.onClosing = function(callback) {
	this.device.children.unobserveCollectionChanged(this.onDeviceChildrenChangedHandler);
	this.stopListening(this.device.children);
	devices.TransactionProtocol.prototype.onClosing.apply(this, [callback]);
}

// TransactionProtocol
my.HidppProtocol.prototype.write = function(request, callback) {
	core.Contract.assert(request instanceof devices.HidppRequest,
		"Expected HidppRequest.");
	core.Contract.assert(request.data.byteLength > 0,
		"Request is empty.");
	var self = this;

	if (!request.prepareForWrite()) {
		core.Log.warning("Write fails : request.prepareForWrite()",
				targetDeviceIndex);
		callback && callback.apply(self, [false]);
		return;
	}

	var targetDeviceIndex = request.deviceIndex;

	if (targetDeviceIndex != my.HIDPP_DEVICE_INDEX.RECEIVER
	 && targetDeviceIndex != my.HIDPP_DEVICE_INDEX.CORDED_DEVICE) {
		var targetDevices = self.device.children.filter(function(device) {
			return device.index === targetDeviceIndex;
		});

		var targetDevice = targetDevices.shift();
		if (targetDevice && !targetDevice.connected) {
			core.Log.warning("Write fails : device %d is not connected",
				targetDeviceIndex);
			callback && callback.apply(self, [false]);
			return;
		}
	}

	var dataView = new DataView(request.data);

	var canWrite = self.device.children.filter(function(hidDevice) {
		return request.data.byteLength <= (hidDevice.maxOutputReportSize + 1)
			&& hidDevice.collections.some(function(collection) {
				return collection.reportIds.some(function(reportId) {
					return reportId == dataView.getUint8(0);
				});
			});

	}).sort(function(p1, p2) {
		return p1.maxOutputReportSize - p2.maxOutputReportSize;
	});

	if (canWrite.length > 0) {
		core.Log.debug("[OUT] %s", request.data.toStringEx());

		var dataView = new DataView(request.data);
		var reportId = dataView.getUint8(0);
		var  dataOut = request.data.slice(1);
		canWrite[0].send(reportId, dataOut, function(success) {
			if (success) {
				devices.TransactionProtocol.prototype.write.apply(self, [request]);
			}
			callback && callback.apply(self, [success]);
		});
	} else {
		// Cannot write
		core.Log.warning("Failed to write");
		callback && callback.apply(self, [false]);

		// TODO:
		// check if device is complete or not (this.device.missingUsages)
	}
}

my.HidppProtocol.prototype.onDeviceChildrenChanged = function(change) {
	this.listen(change.add);
	this.stopListening(change.remove);
}

my.HidppProtocol.prototype.onLinkBrokenHandler = function() {
	core.Log.warning("Device link broken");
	this.removeDevices();
}

my.HidppProtocol.prototype.onNewReport = function(dataIn) {
	this.onInputMessage(dataIn);
}

my.HidppProtocol.prototype.listen = function(hidDevices) {
	var self = this;
	hidDevices.forEach(function(hidDevice) {
		if (hidDevice instanceof devices.HidDevice) {
			hidDevice.connect(function(connected) {
				if (connected) {
					hidDevice.observeEvent('linkBroken', self.onLinkBrokenHandler);
					hidDevice.observeEvent('newReport', self.onNewReportHandler);
				} else {
					core.Log.warning("Device connect failed");
					self.removeDevices();
				}
			});
		}
	});
}

my.HidppProtocol.prototype.stopListening = function(hidDevices) {
	var self = this;
	hidDevices.forEach(function(hidDevice) {
		if (hidDevice instanceof devices.HidDevice) {
			hidDevice.unobserveEvent('linkBroken', self.onLinkBrokenHandler);
			hidDevice.unobserveEvent('newReport', self.onNewReportHandler);
		}
	});
}

my.HidppProtocol.prototype.removeDevices = function() {
	var self = this;

	var removeDevice = function(dev) {
		self.device.provider.devices.remove(dev);
		dev.close();
	}

	var deviceChildren = [];
		self.device.children.forEach(function(child) {
		deviceChildren.push(child);
	});

	deviceChildren.forEach(function(child) {
		removeDevice(child);
	});

	removeDevice(self.device);
}

return my;

}(devices || {}));
