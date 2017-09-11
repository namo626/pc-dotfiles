'use strict';

/* Services */

var services = angular.module('unifyingApp.services', []);

// Localization
services.factory('l10n', function($resource) {

	var singleInstance = {
		getTranslation: function($scope, language, fallbackLanguage, callback) {
			var self = this;
			// Attempt to retrieve localized strings file content.
			var languageFilePath = 'l10n/' + language + '.json';
			$resource(languageFilePath).get(function (data) {
				$scope.l10n = data;
				$scope.l10n.format = function() {
					if (arguments.length > 0) {
						if (arguments.length == 1) {
							return arguments[0];
						} else if (arguments.length == 2) {
							var formatString = arguments[0];
							var argMatch = formatString.match(/\{[^\}]+\}/);
							if (argMatch) {
								var argName  = argMatch[0].slice(1, argMatch[0].length-1);
								var argValue = arguments[1][argName];
								formatString = formatString.replace(argMatch[0], argValue);
							}
							return formatString;
						}
					} else {
						return "";
					}
				}
				callback && callback.apply(self);
			}, function() {
				// An error occurred.
				if (language != fallbackLanguage) {
					// E.g. 'zh-CN', 'fr-CA'
					var localeRegex = /^[a-z]{2}-[a-z]{2}$/i;
					if (language.match(localeRegex)) {
						// Try short locale (e.g. 'en', 'fr', or 'de').
						language = language.slice(0,2);
						self.getTranslation($scope, language, fallbackLanguage, callback);
					} else {
						// Fall-back to fallbackLanguage.
						self.getTranslation($scope, fallbackLanguage, fallbackLanguage, callback);
					}
				}
			});

			// TODO(jracle): use session/local storage?
		}
	};

	return singleInstance;
});

// IoC container
services.factory('ioc', function($location, $rootScope, platformInfo) {
	var permissions = null;
	var hidManager = null;

	var lastSearchUrl = null;

	var onUrlChanged = function() {
		var pathArray = $location.path().split('/')
				.filter(function(elt) { return elt != ''});

		if (pathArray.length > 0 && pathArray[0] == 'test') {
			var searchUrl = $location.search();
			if (lastSearchUrl == searchUrl) {
				return;
			}

			lastSearchUrl = searchUrl;
			var searchUrlPropNames = Object.getOwnPropertyNames(searchUrl);

			if (pathArray.length > 1) {
				var api = pathArray[1];
				if (api == 'hid' && hidManager) {
					if (pathArray.length > 2) {
						var action = pathArray[2];

						// A HID device has been plugged
						// ex: 0xc52b=0x1017;0x4002&0xc52b&0xc52b=0x1018
						if (action == 'plug') {
							for (var i=0; i<searchUrlPropNames.length; ++i) {
								var devicePid = searchUrlPropNames[i];

								var searchUrlProps = searchUrl[devicePid];
								var searchUrlPropsArray = null;
								if (Array.isArray(searchUrlProps)) {
									searchUrlPropsArray = searchUrlProps;
								} else {
									searchUrlPropsArray = [ searchUrlProps ];
								}

								searchUrlPropsArray.forEach(function(childPidsRaw) {
									var childPids    = [];
									childPidsRaw.split && childPidsRaw.split(';')
											.forEach(function(item) {
										var childPid = parseInt(item, 16);
										if (!isNaN(childPid)) {
											childPids.push(childPid);
										}
									});

									hidManager.deviceInfos.push({
										productId: devicePid,
										childPids: childPids
									});
								});
							}

						// A HID device has been un-plugged
						// ex: 0xc52b, 0xc52b=1, 0xc52b=1;2, 0xc52b=1;2&0xc52b
						} else if (action == 'unplug') {
							for (var i=0; i<searchUrlPropNames.length; ++i) {
								var devicePid = searchUrlPropNames[i];

								var searchUrlProps = searchUrl[devicePid];
								var searchUrlPropsArray = null;
								if (Array.isArray(searchUrlProps)) {
									searchUrlPropsArray = searchUrlProps;
								} else {
									searchUrlPropsArray = [ searchUrlProps ];
								}

								searchUrlPropsArray.forEach(function(indicesRaw) {
									var indices = [];
									if (indicesRaw.split) {
										indicesRaw.split(';').forEach(function(item) {
											var index = parseInt(item);
											if (!isNaN(index)) {
												indices.push(index);
											}
										});
									} else {
										indices.push(1);
									}

									var deviceInfos = hidManager.deviceInfos.filter(
											function(deviceInfo) {
										return deviceInfo.productId == devicePid;
									});

									indices.forEach(function(index) {
										var deviceInfo = hidManager.deviceInfos[index-1];
										if (deviceInfo) {
											deviceInfo.removed = true;
										}
									});
								});
							}

						// A HID device was turned OFF then ON
						}  else if (action == 'powercycle') {
							// TODO(jracle): implement
						}
					}
				}
			}
		}
	}

	$rootScope.$on('$locationChangeSuccess', onUrlChanged);

	var singleInstance = {
		load: function() {
			if (typeof console !== "undefined") {
				core.Injector.register('contract', new core.ContractWithConsole());
			}

			var pathArray = $location.path().split('/')
				.filter(function(elt) { return elt != ''});

			// Test mode
			if (pathArray.length > 0 && pathArray[0] == 'test') {				
				var   searchUrl = $location.search();
				var permissions = new core.PermissionsMock();

				if (pathArray.length > 1) {
					var api = pathArray[1];
					if (api == 'hid') {
						permissions.devicePermissions.hid = {};
						permissions.devicePermissions.hid.devices = [];

						if (pathArray.length > 2) {
							var action = pathArray[2];

							// ex: pid=0xc52b, pid=0xc52b&pid=0xc07d
							if (action == 'allow') {
								var pids = [];
								if (searchUrl.pid) {
									if (Array.isArray(searchUrl.pid)) {
										pids = searchUrl.pid;
									} else {
										pids = [ searchUrl.pid ];
									}
								}

								pids.forEach(function(pid) {
									permissions.devicePermissions.hid.devices.push({
										vendorId: devices.VENDOR_ID.Logitech,
										productId: pid
									});
								});
							}
						}

						hidManager = new devices.HidManagerMock();
						core.Injector.register('hidManager', hidManager);
					}
				}

				core.Injector.register('packageInfo', new core.PackageInfoMock());
				core.Injector.register('permissions', permissions);

			// Normal mode
			} else {
				if (platformInfo.isChromeApp()) {
					core.Injector.register('packageInfo', new core.PackageInfoChrome());
					core.Injector.register('permissions', new core.PermissionsChrome());
					core.Injector.register('hidManager', new devices.HidManagerChrome());
				}
			}
		},
		close: function(callback) {
			core.Injector.close(callback);
		}
	};

	return singleInstance;
});

// Platform info
services.factory('platformInfo', function() {

	var singleInstance = {
		isChromeApp: function() {
			return typeof chrome !== "undefined"
				&& chrome.app != null
				&& chrome.app.window != null;
		},
		getAppVersion: function() {
			return core.PackageInfo.getAppVersion();
		}
	};

	return singleInstance;
});
