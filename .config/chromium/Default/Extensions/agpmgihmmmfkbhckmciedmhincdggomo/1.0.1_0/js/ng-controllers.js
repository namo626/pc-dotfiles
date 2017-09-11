'use strict';

/* Controllers */

var controllers = angular.module('unifyingApp.controllers', ['unifyingApp.services']);

// Main controller (shared)
controllers.controller("UnifyingController",
		function($resource, $scope, l10n, ioc, platformInfo) {

	var doClose = function() {
		ioc.close(function() {
			core.Log.close(function() {
				if (!platformInfo.isChromeApp()) {
					window.removeEventListener('beforeunload', closeAll);
				}
			});
		});
	};

	var closeAll = function() {
		var windows = chrome.app.window.getAll();
		windows.forEach(function(wnd) {
			wnd.close();
		});

		if ($scope.unifying) {
			$scope.unifying.close(doClose);
		} else {
			doClose();
		}
	};

	if (platformInfo.isChromeApp()) {
		chrome.app.window.current().onClosed.addListener(closeAll);
	} else {
		window.addEventListener('beforeunload', closeAll);
	}

	var getLogOptions = function(callback) {
		var self = this;
		$resource('logOptions.json').get(function(options) {
			core.Log.options = options;
			callback && callback.apply(self);
		}, function() {
			callback && callback.apply(self);
		});
	};

	$scope.confirm_exit = function() {
		if (platformInfo.isChromeApp()) {
			window.$windowScope = $scope;

			chrome.app.window.create('confirm_exit.html', {
				bounds: {
					width: 340,
					height: 200,
				},
				maxWidth: 340,
				minWidth: 340,
				minHeight: 200,
				maxHeight: 200,
				id: "confirm_exit"
			});
		}
	};

	$scope.learnMore = function() {
		if (platformInfo.isChromeApp()) {
			window.$windowScope = $scope;
			chrome.app.window.create('learn_more.html', {
				bounds: {
					width: 640,
					height: 680,
				},
				maxWidth: 640,
				minWidth: 640,
				minHeight: 680,
				maxHeight: 680,
				id: "learn_more"
			});
		}
	};

	$scope.troubleshoot = function() {
		if (platformInfo.isChromeApp()) {
			window.$windowScope = $scope;

			chrome.app.window.create('troubleshoot.html', {
				bounds: {
					width: 640,
					height: 600,
				},
				maxWidth: 640,
				minWidth: 640,
				minHeight: 600,
				maxHeight: 600,
				id: "troubleshoot"
			});
		}
	};

	$scope.correct_device = function() {
		if (platformInfo.isChromeApp()) {
			window.$windowScope = $scope;

			chrome.app.window.create('correct_device.html', {
				bounds: {
					width: 640,
					height: 320,
				},
				maxWidth: 640,
				minWidth: 640,
				minHeight: 320,
				maxHeight: 320,
				id: "correct_device"
			});
		}
	};

	$scope.replacing_device = function() {
		if (platformInfo.isChromeApp()) {
			window.$windowScope = $scope;

			chrome.app.window.create('replacing_device.html', {
				bounds: {
					width: 320,
					height: 240,
				},
				maxWidth: 320,
				minWidth: 320,
				minHeight: 240,
				maxHeight: 240,
				id: "replacing_device"
			});
		}
	};

	$scope.canExit = platformInfo.isChromeApp();
	$scope.exit = closeAll;

	ioc.load();

	getLogOptions(function() {
		var fallbackLanguage = 'en';

		// For most navigators
		var language = window.navigator.language;

		if (!language) {
			// For old IE versions
			language = window.navigator.userLanguage;
		}

		if (!language) {
			// Fall-back
			language = fallbackLanguage;
		}

		l10n.getTranslation($scope, language, fallbackLanguage, function() {
			var deviceApi = core.Injector.resolve(function(hidManager) {
				$scope.unifying = new unifying.Unifying(hidManager, $scope);
			});

			deviceApi();
		});
	});
});

// Popup Panels
controllers.controller("PopupController",
		function($scope) {
	var parentWindow = window.opener;
	var parentScope  = parentWindow.$windowScope;
	$scope.l10n      = parentScope.l10n;	
	$scope.unifying = parentScope.unifying;

	$scope.close = function() {
		window.close();
	}

	$scope.exit = function() {
		$scope.unifying.exit();
	}
});
