'use strict';

var devices = (function (my) {

my.FEATURE_ID.BATTERY = 0x1000;

////////////////////////////////////////////////////////
// FAP_BATTERY_STATUS

my.FAP_BATTERY_STATUS = {
	DISCHARGING                : 0,
	RECHARGING                 : 1,
	CHARGE_IN_FINAL_STATE      : 2,
	CHARGE_COMPLETE            : 3,
	RECHARGING_BELOW_OPTIMAL   : 4,
	ERROR_INVALID_BATTERY_TYPE : 5,
	ERROR_THERMAL              : 6,
	ERROR_OTHER                : 7,
	START_CHARGING             : 8,

	toString : function(value) {
		return this.getOwnPropertyNameForValue(value)
			.replace(/_/g, " ").capitalize();
	}
};

////////////////////////////////////////////////////////
// Battery

// Parameters:
// - HidppFap fap
// - FeatureIndex featureIndex
my.Battery = function(fap, featureIndex) {
	devices.Feature.call(this, fap, featureIndex);
}

my.Battery.prototype = core.inherit(devices.Feature.prototype);

core.Injector.register('Feature1000', my.Battery);

// Feature
my.Battery.prototype.onEvent = function(eventId, data) {
	my.Feature.prototype.onEvent.call(this, eventId, data);

	var dataView = new DataView(data);

	switch (eventId) {
		case 0:
			{
				var status = {
					level: dataView.getUint8(0),
					nextLevel: dataView.getUint8(1),
					status: dataView.getUint8(2)
				};

				this.raiseEvent('batteryStatusChanged', status);
			}
			break;

		default:
			break;
	}
}

my.Battery.prototype.getBatteryStatus = function(callback) {
	core.Contract.assert(typeof callback == 'function'
		&& callback, "Expected function");

	var request = this.createRequest(0);
	this.fap.requests.enqueue(request);

	request.waitResponse(function(response) {
		var dataView = new DataView(response.data);

		var status = {
			level:     dataView.getUint8(4),
			nextLevel: dataView.getUint8(5),
			status:    dataView.getUint8(6)
		};

		this.status = status;

		callback && callback(status);
	});
}

my.Battery.prototype.getBatteryCapability = function(callback) {
	core.Contract.assert(typeof callback == 'function'
		&& callback, "Expected function");

	var request = this.createRequest(1);
	this.fap.requests.enqueue(request);

	request.waitResponse(function(response) {
		var dataView = new DataView(response.data);

		var capability = {
			nbLevels: dataView.getUint8(4) & 0xF,
			flags: (dataView.getUint8(4) & 0xF0) >> 4,
			//nominalBatteryLife: dataView.getUint8(5) | dataView.getUint8(6) << 8,
			//batteryCriticalLevel : dataView.getUint8(6)
		};

		callback && callback(capability);
	});
}

return my;

}(devices || {}));
