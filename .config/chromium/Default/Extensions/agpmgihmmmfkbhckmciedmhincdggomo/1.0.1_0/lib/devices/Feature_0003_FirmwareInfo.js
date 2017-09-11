'use strict';

var devices = (function (my) {

my.FEATURE_ID.FW_VERSION = 0x0003;

////////////////////////////////////////////////////////
// FAP_FW_TYPE

my.FAP_FW_TYPE = {
	MAIN_APPLICATION : 0,
	BOOT_LOADER      : 1,
	HARDWARE         : 2,
	TOUCHPAD         : 3,

	toString : function(value) {
		return this.getOwnPropertyNameForValue(value)
			.replace(/_/g, " ").capitalize();
	}
};

////////////////////////////////////////////////////////
// FirmwareVersion

// Parameters:
// - HidppFap fap
// - FeatureIndex featureIndex
my.FirmwareVersion = function(fap, featureIndex) {
	devices.Feature.call(this, fap, featureIndex);
}

my.FirmwareVersion.prototype = core.inherit(devices.Feature.prototype);

core.Injector.register('Feature0003', my.FirmwareVersion);

// Parameters:
// - function callback = function(integer count)
my.FirmwareVersion.prototype.getEntityCount = function(callback) {
	core.Contract.assert(typeof callback == 'function'
		&& callback, "Expected function");

	var request = this.createRequest(0);
	this.fap.requests.enqueue(request);
	
	request.waitResponse(function(response) {
		var dataView = new DataView(response.data);
		var    count = dataView.getUint8(4);
		callback && callback(count);
	});
}

// Parameters:
// - integer entity
// - function callback = function({'fwType' : FAP_FW_TYPE, 'prefix' : String, 'version' : String, 'productId' : integer } fwInfo)
my.FirmwareVersion.prototype.getFWInfo = function(entity, callback) {
	core.Contract.assert(typeof callback == 'function'
		&& callback, "Expected function");

	var request = this.createRequest(1, entity);
	this.fap.requests.enqueue(request);

	request.waitResponse(function(response) {
		var dataView = new DataView(response.data);

		var maj = dataView.getUint8(8).toString(16);
		var min = dataView.getUint8(9).toString(16);
		var bui = (dataView.getUint8(10) << 8 | dataView.getUint8(11)).toString(16);
		var rev = dataView.getUint8(12).toString(16);

		var prefix = "";
		for (var i=0;i<3;i++) {
			prefix += String.fromCharCode(dataView.getUint8(i + 5));
		}

		var fwInfo = {
			'fwType'    : dataView.getUint8(4) & 0x0F,
			'prefix'    : prefix,
			'version'   : (maj + "." + min + "." + bui + "." + rev),
			'productId' : (dataView.getUint8(13) << 8 | dataView.getUint8(14))
		};

		callback && callback(fwInfo);
	});
}

return my;

}(devices || {}));
