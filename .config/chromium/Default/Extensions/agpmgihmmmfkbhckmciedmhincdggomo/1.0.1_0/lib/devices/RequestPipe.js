'use strict';

var devices = (function (my) {

my.RequestPipe = function(writer) {
	core.Closable.call(this);
	this.writer          = writer;
	this.currentRequest  = null;
	this.requestsToWrite = [];
	this.worker          = null;
	this.exiting         = false;

	this.workerFunc();
}

my.RequestPipe.prototype = core.inherit(core.Closable.prototype);

// Closable
my.RequestPipe.prototype.onClosing = function(callback) {
	this.exiting = true;
	this.requestsToWrite.clear();

	if (this.worker) {
		clearTimeout(this.worker);
		this.worker = null;
	}

	core.Closable.prototype.onClosing.apply(this, [callback]);
}

my.RequestPipe.prototype.enqueue = function(request, front) {
	var self = this;
	var targetDeviceIndex = request.deviceIndex;

	if (targetDeviceIndex != my.HIDPP_DEVICE_INDEX.RECEIVER
	 && targetDeviceIndex != my.HIDPP_DEVICE_INDEX.CORDED_DEVICE) {
		var targetDevices = self.writer.device.children.filter(
				function(device) {
			return device.index === targetDeviceIndex;
		});

		var targetDevice = targetDevices.shift();
		targetDevice && targetDevice.waitProperty('connected',
				function(connected) {
			if (connected) {
				self.requestsToWrite.push(request);
			} else {
				core.Log.warning("Enqueue: wait device %d reconnection",
					targetDeviceIndex);
				self.enqueue(request, front);
			}
		});

	} else {
		if (front) {
			self.requestsToWrite.unshift(request);
		} else {
			self.requestsToWrite.push(request);
		}		
	}
}

my.RequestPipe.prototype.clean = function(deviceIndex, callback) {
	var self = this;

	var waitRequestCompletion = function(request, callback) {
		request.waitResponse(
			// Success
			function() {
				callback && callback.apply(self);
			// Error
			}, function() {
				callback && callback.apply(self);
			});
	}

	self.requestsToWrite.removeWhere(function(request) {
		if (request.targetDeviceIndex) {
			return request.targetDeviceIndex == deviceIndex;
		} else {
			return request.deviceIndex == deviceIndex;
		}
	});

	var currentRequest = self.currentRequest;
	if (currentRequest) {
		if (currentRequest.targetDeviceIndex) {
			if (currentRequest.targetDeviceIndex == deviceIndex) {
				waitRequestCompletion(currentRequest, callback);
				return;
			}
		} else {
			if (currentRequest.deviceIndex == deviceIndex) {
				waitRequestCompletion(currentRequest, callback);
				return;
			}
		}
	}

	callback && callback.apply(self);
}

my.RequestPipe.prototype.workerFunc = function() {
	var self = this;

	var writeAsync = function() {
		if (self.exiting) {
			return;
		}

		var onWriteFailure = function(request) {
			if (request.isCritical) {
				self.writer.onCriticalError(function() {
					core.Log.error("A critical error occurred");
					clearTimeout(self.worker);
					self.worker = setTimeout(writeAsync, 10);
				});
			} else {
				clearTimeout(self.worker);
				self.worker = setTimeout(writeAsync, 10);
			}
		};

		var retryWrite = function(request) {
			if (!request.retryCount) {
				request.retryCount = 0;
			}
			request.retryCount++;

			if (request.retryCount <= request.maxRetryCount) {
				request['response'] = null;
				self.enqueue(request, true);
				clearTimeout(self.worker);
				self.worker = setTimeout(writeAsync, request.retryCount * 100);

			} else {
				onWriteFailure(request);
			}
		}

		self.currentRequest = self.requestsToWrite.shift();
		if (self.exiting) {
			return;
		}

		if (self.currentRequest) {
			self.writer.write(self.currentRequest, function(success) {
				if (self.exiting) {
					return;
				}

				if (success) {
					self.currentRequest.waitProperty('response',
						function(response) {
							if (!self.exiting) {
								if (response.success) {
									clearTimeout(self.worker);
									self.worker = setTimeout(writeAsync, 1);
								} else {
									core.Log.warning(
										"Error response %O (%s) for request %O (%s)",
										response, response.toString(),
										self.currentRequest,
										self.currentRequest.toString());

									retryWrite(self.currentRequest);
								}
							}
						}, self.currentRequest.responseTimeoutMs, function() {
							core.Log.warning("Request %O (%s) timed out",
								self.currentRequest,
								self.currentRequest.toString());

							if (!self.exiting) {
								retryWrite(self.currentRequest);
							}
					});

				} else {
					core.Log.warning("Failed to write request %O (%s)",
						self.currentRequest,
						self.currentRequest.toString());

					retryWrite(self.currentRequest);
				}
			});

		} else {
			clearTimeout(self.worker);
			self.worker = setTimeout(writeAsync, 1);
		}
	};

	writeAsync();
}

return my;

}(devices || {}));
