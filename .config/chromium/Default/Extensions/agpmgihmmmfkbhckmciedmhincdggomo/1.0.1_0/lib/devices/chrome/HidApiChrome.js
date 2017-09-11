'use strict';

var devices = (function (my) {

////////////////////////////////////////////////////////
// HidManagerChrome

my.HidManagerChrome = function() {
	devices.HidManager.call(this);
}

my.HidManagerChrome.prototype = core.inherit(devices.HidManager.prototype);

my.HidManagerChrome.prototype.getApiDevices = function(deviceFilter, callback) {
	chrome.hid.getDevices(deviceFilter, callback);
}

my.HidManagerChrome.prototype.createHidDevice = function(apiDevice) {
	return new my.HidDeviceChrome(this, apiDevice);
}

////////////////////////////////////////////////////////
// HidDeviceChrome

my.HidDeviceChrome = function(provider, apiDevice) {
	devices.HidDevice.call(this, provider, apiDevice);
	this.vendorId            = apiDevice.vendorId;
	this.productId           = apiDevice.productId;
	this.collections         = apiDevice.collections;
	this.maxInputReportSize  = apiDevice.maxInputReportSize;
	this.maxOutputReportSize = apiDevice.maxOutputReportSize;
	this.connectionId        = -1;
}

my.HidDeviceChrome.prototype = core.inherit(devices.HidDevice.prototype);

my.HidDeviceChrome.prototype.connect = function(callback) {
	var self = this;
	if (self.connectionId !== -1) {
		callback && callback.apply(self, [self.connected]);

	} else {
		chrome.hid.connect(self.apiDevice.deviceId, function(hidConnectInfo) {
			var lastError = chrome.runtime.lastError;
			if (lastError) {
				core.Log.error(lastError.message);
				self.connectionId = -1;
			} else {
				self.connectionId = hidConnectInfo.connectionId;
			}

			self.connected = self.connectionId !== -1;
			if (self.connected) {
				core.Log.info("HID connection %d established.", self.connectionId);
			}

			callback && callback.apply(self, [self.connected]);
		});
	}
}

my.HidDeviceChrome.prototype.disconnect = function(callback) {
	var self = this;
	if (self.connectionId === -1) {
		callback && callback.apply(self);
	} else {
		chrome.hid.disconnect(self.connectionId, function() {
			if (self.connectionId !== -1) { // else connection broken
				core.Log.info("HID connection %d closed.", self.connectionId);
				self.connectionId = -1;
				self.connected = false;
			}
			callback && callback.apply(self);
		});
	}
}

my.HidDeviceChrome.prototype.receive = function(callback) {
	var self = this;
	chrome.hid.receive(self.connectionId, function(reportId, data) {
		var lastError = chrome.runtime.lastError;
		if (lastError) {
			core.Log.error(lastError.message);
			core.Log.warning("HID connection %d broken.", self.connectionId);
			self.connectionId = -1;
			self.connected = false;
			self.raiseEvent('linkBroken');
		}

		callback && callback.apply(self, [!lastError, reportId, data]);
	});
}

my.HidDeviceChrome.prototype.send = function(reportId, data, callback) {
	var self = this;
	self.waitProperty('connected', function(connected) {
		if (connected) {
			chrome.hid.send(self.connectionId, reportId, data, function() {
				var lastError = chrome.runtime.lastError;
				if (lastError) {
					core.Log.error(lastError.message);
				}
				callback && callback.apply(self, [!lastError]);
			});
		} else {
			callback && callback.apply(self, false);
		}
	});
}

return my;

}(devices || {}));
