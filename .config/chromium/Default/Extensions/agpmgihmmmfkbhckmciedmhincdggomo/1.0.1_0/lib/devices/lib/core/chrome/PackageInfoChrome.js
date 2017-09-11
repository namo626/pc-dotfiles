'use strict';

var core = (function (my) {

my.PackageInfoChrome = function() {
	core.PackageInfoBase.call(this);
}

my.PackageInfoChrome.prototype = core.inherit(core.PackageInfoBase.prototype);

my.PackageInfoChrome.prototype.getAppName = function() {
	var manifest = chrome.runtime.getManifest();	
	return manifest.name;
}

my.PackageInfoChrome.prototype.getAppVersion = function() {
	var manifest = chrome.runtime.getManifest();	
	return manifest.version;
}

return my;

}(core || {}));
