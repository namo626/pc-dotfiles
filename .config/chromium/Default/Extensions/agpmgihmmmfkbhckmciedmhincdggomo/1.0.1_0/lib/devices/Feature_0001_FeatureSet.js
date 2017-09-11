'use strict';

var devices = (function (my) {

// Parameters:
// - HidppFap fap
// - FeatureIndex featureIndex
my.FeatureSet = function(fap, featureIndex) {
	devices.Feature.call(this, fap, featureIndex);
}

my.FeatureSet.prototype = core.inherit(devices.Feature.prototype);

core.Injector.register('Feature0001', my.FeatureSet);

// Parameters:
// - function callback = function(integer featureCount)
my.FeatureSet.prototype.getFeatureCount = function(callback) {
	core.Contract.assert(typeof callback == 'function'
		&& callback, "Expected function");

	var request = this.createRequest(0);
	this.fap.requests.enqueue(request);

	request.waitResponse(function(response) {
		var     dataView = new DataView(response.data);
		var featureCount = dataView.getUint8(4);
		callback && callback(featureCount);
	});
}

// Parameters:
// - integer featureIndex
// - function callback = function(FeatureIndex featureIndex)
my.FeatureSet.prototype.getFeatureId = function(featureIndex, callback) {
	core.Contract.assert(typeof callback == 'function'
		&& callback, "Expected function");

	var request = this.createRequest(1, featureIndex);
	this.fap.requests.enqueue(request);

	request.waitResponse(function(response) {
		var     dataView = new DataView(response.data);
		var    featureId = dataView.getUint8(4) << 8 | dataView.getUint8(5);
		var      options = dataView.getUint8(6);
		callback && callback(new my.FeatureIndex(featureId, featureIndex, options));
	});
}

return my;

}(devices || {}));
