'use strict';

/* Background script */

chrome.app.runtime.onLaunched.addListener(function() {
	chrome.app.window.create('index.html', {
		bounds: {
			width: 640,
			height: 480,
		},
		maxWidth: 640,
		minWidth: 640,
		minHeight: 480,
		maxHeight: 480,
		singleton: true,
		id: "unifyingApp"
	});
});
