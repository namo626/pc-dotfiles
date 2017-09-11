'use strict';

var devices = (function (my) {

my.UsbApiChrome = function() {
	devices.HidApi.call(this);
}

my.UsbApiChrome.prototype = core.inherit(devices.HidApi.prototype);

// DeviceApi
// TODO(jracle)

// USB
// TODO(jracle): wrap chrome.usb API

return my;

}(devices || {}));
