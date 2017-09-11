'use strict';

var core = (function (my) {

my.PermissionsChrome = function() {
	core.PermissionsBase.call(this);
}

my.PermissionsChrome.prototype = core.inherit(core.PermissionsBase.prototype);

my.PermissionsChrome.prototype.isAuthorized = function(callback) {
	var manifest = chrome.runtime.getManifest();
	if (manifest.optional_permissions) {
		var permissionObj = {
			permissions: manifest.optional_permissions
		};
		chrome.permissions.contains(permissionObj, callback);
	} else {
		callback(true);
	}
}

my.PermissionsChrome.prototype.request = function(callback) {
	var manifest = chrome.runtime.getManifest();
	if (manifest.optional_permissions) {
		var permissionObj = {
			permissions: manifest.optional_permissions
		};
		chrome.permissions.request(permissionObj, callback);
	} else {
		callback(true);
	}
}

my.PermissionsChrome.prototype.getDevicePermissions = function(callback) {
	core.Contract.assert(typeof callback == 'function'
		&& callback, "Expected function.");

	var devicePermissions = {};
	var manifest = chrome.runtime.getManifest();

	// Bluetooth / BLE
	if (manifest.bluetooth) {
		devicePermissions.bluetooth = {};
		devicePermissions.bluetooth.transferOwnProperties(manifest.bluetooth);
	}

	// HID (HID over Bluetooth, HID over BLE)
	if (manifest.permissions.contains('hid')) {
		devicePermissions.hid = {};
	}

	// USB
	if (manifest.permissions.contains('usb')) {
		devicePermissions.usb = {};
	}

	if (manifest.optional_permissions) {
		for (var i=0; i<manifest.optional_permissions.length; i++) {
			var optional_permission = manifest.optional_permissions[i];
				if (optional_permission.usbDevices) {
					if (devicePermissions.hid) {
						devicePermissions.hid.devices
							= optional_permission.usbDevices;
					}
					if (devicePermissions.usb) {
						devicePermissions.usb.devices
							= optional_permission.usbDevices;
				}
			}
		}
	}

	callback(devicePermissions);
}

return my;

}(core || {}));
