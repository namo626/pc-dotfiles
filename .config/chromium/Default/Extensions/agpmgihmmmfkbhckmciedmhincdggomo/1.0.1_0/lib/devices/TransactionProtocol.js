'use strict';

var devices = (function (my) {

my.TransactionProtocol = function(device) {
	devices.Protocol.call(this, device);

	var self = this;
	if (self.device.parent) {
		self.device.parent.waitProperty('protocol', function(protocol) {
			self.requests = protocol.requests;
		});
	} else {
		self.requests = new devices.RequestPipe(self);
	}
}

my.TransactionProtocol.prototype = core.inherit(devices.Protocol.prototype);

// Protocol
my.TransactionProtocol.prototype.onClosing = function(callback) {
	var self = this;
	if (self.device.parent) {
		self.requests.clean(self.device.index, function() {
			devices.Protocol.prototype.onClosing.apply(self, [callback]);
		});
	} else {
		self.requests.close(function() {
			devices.Protocol.prototype.onClosing.apply(self, [callback]);
		});
	}
}

my.TransactionProtocol.prototype.write = function(request) {
	this.raiseEvent('writtenRequest', request);
}

my.TransactionProtocol.prototype.onCriticalError = function(callback) {
	this.device.provider.restart(callback);
}

// To be called by sub-classes
my.TransactionProtocol.prototype.onInputMessage = function(dataIn) {
	var currentRequest = this.requests.currentRequest;

	if (currentRequest != null) {
		// A pending request is not yet completed
		// (See RequestPipe.js)
		if (currentRequest.response) {
			// Pending request matched incoming data
			if (currentRequest.tryMatch(dataIn)) {
				core.Log.warning("Request %O (%s) is already completed.",
					currentRequest, currentRequest.toString());
				return;
			}
			// Else this is a notification, proceed
		}
		// Attempt to match request to incoming data
		else if (currentRequest.tryMatch(dataIn)) {
			this.raiseEvent('requestCompleted', currentRequest);
			return;
		}
		// Else this is a notification, proceed
	}

	// Incoming data is a notification
	var notification = new devices.Notification(dataIn);
	this.raiseEvent('newNotification', notification);
	this.onNotification(notification);

	this.device.children.forEach(function(child) {
		if (child.protocol) {
			child.protocol.onNotification(notification);
		}
	});
}

my.TransactionProtocol.prototype.onNotification = function(notification) {
	// Overridable
}

return my;

}(devices || {}));
