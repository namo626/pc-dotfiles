'use strict';

var devices = (function (my) {

// Parameters:
// - HidppFap fap
my.RootFeature = function(fap) {
	// Root feature is always at index 0 (without options)
	devices.Feature.call(this,
		fap, new devices.FeatureIndex(devices.FEATURE_ID.ROOT, 0, 0));
}

my.RootFeature.prototype = core.inherit(devices.Feature.prototype);

core.Injector.register('Feature0000', my.RootFeature);

// Parameters:
// - integer featureId
// - function callback = function(FeatureIndex featureIndex)
my.RootFeature.prototype.getFeatureIndex = function(featureId, callback) {
	core.Contract.assert(typeof callback == 'function'
		&& callback, "Expected function");

	var request = this.createRequest(0, (featureId & 0xFF00) >> 8, featureId & 0x00FF);
	this.fap.requests.enqueue(request);

	request.waitResponse(function(response) {
		var     dataView = new DataView(response.data);
		var featureIndex = new my.FeatureIndex(
			featureId,
			dataView.getUint8(4),
			dataView.getUint8(5));
		callback && callback(featureIndex);
	});
}

return my;

}(devices || {}));
