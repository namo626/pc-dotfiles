'use strict';

var devices = (function (my) {

my.DeviceProvider = function() {
	core.Closable.call(this);
	this.devices = new core.ObservableArray();
}

my.DeviceProvider.prototype = core.inherit(core.Closable.prototype);

// Closable
my.DeviceProvider.prototype.onClosing = function(callback) {
	var self = this;

	if (self.devices.length > 0) {
		var closeDevice = function(index) {
			var device = self.devices[index];
			device.close(function() {
				index++;
				if (index < self.devices.length) {
					closeDevice(index);
				} else {
					self.devices.clear();
					core.Closable.prototype.onClosing.apply(self, [callback]);
				}
			});
		};

		closeDevice(0);
	} else {
		core.Closable.prototype.onClosing.apply(self, [callback]);
	}
}

return my;

}(devices || {}));
