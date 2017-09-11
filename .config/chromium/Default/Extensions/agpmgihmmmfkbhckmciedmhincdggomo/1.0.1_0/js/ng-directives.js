'use strict';

/* Directives */

var directives = angular.module('unifyingApp.directives', ['unifyingApp.services']);
 
directives.directive('appName', ['platformInfo', function(platformInfo) {
	return function(scope, elm, attrs) {
		elm.text(platformInfo.getAppName());
	};
}]);

directives.directive('appVersion', ['platformInfo', function(platformInfo) {
	return function(scope, elm, attrs) {
		elm.text(platformInfo.getAppVersion());
	};
}]);
