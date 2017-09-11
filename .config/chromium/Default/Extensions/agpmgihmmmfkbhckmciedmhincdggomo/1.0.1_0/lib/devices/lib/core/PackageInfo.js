'use strict';

var core = (function (my) {

////////////////////////////////////////////////////////
// PackageInfoBase

my.PackageInfoBase = function() {}

my.PackageInfoBase.prototype.getAppVersion = function() {
	throw new TypeError("Abstract method");
}

////////////////////////////////////////////////////////
// PackageInfoMock

my.PackageInfoMock = function() {
	my.PackageInfoBase.call(this);
}

my.PackageInfoMock.prototype = core.inherit(my.PackageInfoBase.prototype);

my.PackageInfoMock.prototype.getAppName = function() {
	return "";
}

my.PackageInfoMock.prototype.getAppVersion = function() {
	return '0.0.0';
}

////////////////////////////////////////////////////////
// PackageInfo

my.PackageInfo = {
	getAppName: function(callback) {
		var name = null;

		var impl = core.Injector.resolve(function(packageInfo) {
			if (packageInfo) {	
				name = packageInfo.getAppName();
			}
		});

		impl();

		return name;
	},
	getAppVersion: function(callback) {
		var version = null;

		var impl = core.Injector.resolve(function(packageInfo) {
			if (packageInfo) {	
				version = packageInfo.getAppVersion();
			}
		});

		impl();

		return version;
	}
}

return my;

}(core || {}));
