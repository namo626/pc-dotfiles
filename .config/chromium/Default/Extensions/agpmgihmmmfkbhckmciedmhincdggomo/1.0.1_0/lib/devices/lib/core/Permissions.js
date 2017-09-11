'use strict';

var core = (function (my) {

////////////////////////////////////////////////////////
// PermissionsBase

my.PermissionsBase = function() {}

my.PermissionsBase.prototype.isAuthorized = function(callback) {
	throw new TypeError("Abstract method");
}

my.PermissionsBase.prototype.request = function(callback) {
	throw new TypeError("Abstract method");
}

my.PermissionsBase.prototype.getDevicePermissions = function(callback) {
	throw new TypeError("Abstract method");
}

////////////////////////////////////////////////////////
// PermissionsMock

my.PermissionsMock = function() {
	my.PermissionsBase.call(this);
	this.isAuthorizedSuccess = true;
	this.requestSuccess = true;
	this.devicePermissions = {};
}

my.PermissionsMock.prototype = core.inherit(my.PermissionsBase.prototype);

my.PermissionsMock.prototype.isAuthorized = function(callback) {
	callback && callback(this.isAuthorizedSuccess);
}

my.PermissionsMock.prototype.request = function(callback) {
	callback && callback(this.requestSuccess);
}

my.PermissionsMock.prototype.getDevicePermissions = function(callback) {
	callback && callback(this.devicePermissions);
}

////////////////////////////////////////////////////////
// Permissions

my.Permissions = {
	isAuthorized: function(callback) {
		var impl = core.Injector.resolve(function(permissions) {
			if (permissions) {
				permissions.isAuthorized(callback);
			}
		});

		impl();
	},
	request: function(callback) {
		var impl = core.Injector.resolve(function(permissions) {
			if (permissions) {
				permissions.request(callback);
			}
		});

		impl();
	},
	getDevicePermissions: function(callback) {
		var impl = core.Injector.resolve(function(permissions) {
			if (permissions) {
				permissions.getDevicePermissions(callback);
			}
		});

		impl();
	}
}

return my;

}(core || {}));
