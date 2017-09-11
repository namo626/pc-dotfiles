'use strict';

var devices = (function (my) {

////////////////////////////////////////////////////////
// FAP_ERROR

my.FAP_ERROR = {
	SUCCESS               : 0x00,
	UNKNOWN               : 0x01,
	INVALID_ARGUMENT      : 0x02,
	OUT_OF_RANGE          : 0x03,
	HW_ERROR              : 0x04,
	NOT_ALLOWED           : 0x05,
	INVALID_FEATURE_INDEX : 0x06,
	INVALID_FUNCTION_ID   : 0x07,
	BUSY                  : 0x08,

	toString : function(value) {
		return this.getOwnPropertyNameForValue(value)
			.replace(/_/g, " ").capitalize();
	}
};

////////////////////////////////////////////////////////
// HidppFapRequest

// Arguments : deviceIndex, featureIndex, functionId, softwareId, payload (implicit)
my.HidppFapRequest = function(deviceIndex, featureIndex, functionId, softwareId) {
	devices.HidppRequest.call(this, deviceIndex);

	var array = new Uint8Array(this.data);
	array[2] = featureIndex;
	array[3] = functionId << 4 | softwareId;

	var payload = Array.prototype.slice.call(arguments, 4);
	for (var i=0; i<Math.min(array.length-4, payload.length); i++) {
		array[4+i] = payload[i];
	}
}

my.HidppFapRequest.prototype = core.inherit(devices.HidppRequest.prototype);

// dataIn : ArrayBuffer
// returns { "matchResult" : MATCH_RESULT, "errCode" : RAP_ERROR };
my.HidppFapRequest.prototype.matches = function(dataIn) {
	core.Contract.assert(dataIn instanceof ArrayBuffer, "Expected ArrayBuffer.");

	var result = {
		"matchResult" : devices.MATCH_RESULT.NO_MATCH,
		"errCode" : my.FAP_ERROR.SUCCESS
	};

	var reqView = new DataView(this.data);
	core.Contract.assert(reqView.byteLength >= 4);

	var rspView = new DataView(dataIn);
	core.Contract.assert(rspView.byteLength >= 4);

	if ((reqView.getUint8(1) == rspView.getUint8(1))  //  device index
	 && (reqView.getUint8(2) == rspView.getUint8(2))  // feature index
	 && (reqView.getUint8(3) == rspView.getUint8(3))) // function/event ID + software ID
	{
		result.matchResult = devices.MATCH_RESULT.SUCCESS;
	}
	else if ((reqView.getUint8(1) == rspView.getUint8(1))  //  device index
						 && (0xFF == rspView.getUint8(2))  // Hid++ 2.0 error
		  && (reqView.getUint8(2) == rspView.getUint8(3))  // feature index
		  && (reqView.getUint8(3) == rspView.getUint8(4))) // function/event ID + software ID
	{
		result.errCode     = rspView.getUint8(5); // FAP_ERROR
		result.matchResult = devices.MATCH_RESULT.ERROR;
	}

	return result;
}

////////////////////////////////////////////////////////
// HidppFap

my.HidppFap = function(device, version) {
	devices.HidppProtocol.call(this, device);

	this.version              = version;
	this.deviceIndexForEvents = this.device.index; // TOFIX(jracle)
	this.softwareId           = devices.SoftwareId.value;
	this.rootFeature          = new  my.RootFeature(this);
	this.featureCache         = new my.FeatureCache(this);
}

my.HidppFap.prototype = core.inherit(devices.HidppProtocol.prototype);

// Object
my.HidppFap.prototype.toString = function() {
	return "HidppFap";
}

// TransactionProtocol
my.HidppFap.prototype.onNotification = function(notification) {
	core.Contract.assert(notification instanceof devices.Notification, "Expected Notification");
	var dataView = new DataView(notification.data);

	// Dynamic Software ID
	if (dataView.byteLength == 20 
	 && dataView.getUint8(0) == devices.REPORT_ID.HIDPP_LONG 
	 && dataView.getUint8(1) == this.device.index
	 && dataView.getUint8(2) == this.rootFeature.index.index
	 && (dataView.getUint8(3) & 0xF0) == 0x10) { // 'hidden' function 1
		var softwareId = dataView.getUint8(3) & 0x0F;

		if (softwareId === devices.SoftwareId.BroadcastQueryId) {
			// Another app requests Software ID
			var request = new devices.GetProtocolVersionRequest(this.device.index);
			this.requests.enqueue(request);
		} else {
			// Another app communicates with this Software ID
			devices.SoftwareId.setBusy(softwareId);
		}

	// Feature Event
	} else if (dataView.byteLength == 20
		&& dataView.getUint8(1) == this.device.index) {
		var featureIndex = dataView.getUint8(2);
		var     eventId = (dataView.getUint8(3) & 0xF0) >> 4;

		var feature = this.getFeatureByIndex(featureIndex);
		if (feature) {
			feature.onEvent(eventId, notification.data.slice(4));
		}
	}
}

// Parameters:
// - integer featureId
// - function callback = function(Boolean hasFeature)
my.HidppFap.prototype.hasFeature = function(featureId, callback) {
	core.Contract.assert(typeof callback == 'function' && callback, "Expected function");
	var self = this;

	if (featureId == my.FEATURE_ID.ROOT) {
		callback && callback(true);
	} else {	
		self.rootFeature.getFeatureIndex(featureId, function(featureIndex) {
			var hasFeature = featureIndex.isValid();
			callback && callback.apply(self, [hasFeature]);
		});
	}
}

my.HidppFap.prototype.getFeatureByIndex = function(featureIndex) {
	return this.featureCache.features[featureIndex];
}

// Parameters:
// - integer featureId
// - function callback = function(Feature/Feature_subclass feature)
my.HidppFap.prototype.getFeature = function(featureId, callback) {
	core.Contract.assert(typeof callback == 'function' && callback, "Expected function");
	var self = this;

	this.featureCache.getFeature(featureId, function(feature) {
		callback && callback.apply(self, [feature]);
	});
}

// Parameters:
// - function callback = function(Array of Feature/Feature_subclass features)
my.HidppFap.prototype.getFeatures = function(callback) {
	core.Contract.assert(typeof callback == 'function' && callback, "Expected function");
	var self = this;

	this.featureCache.getFeatures(function(features) {
		callback && callback.apply(self, [features]);
	});
}

////////////////////////////////////////////////////////
// FEATURE_ID

my.FEATURE_ID = {
	ROOT		: 0x0000,
	FEATURE_SET : 0x0001,

	toString : function(value) {
		return this.getOwnPropertyNameForValue(value)
			.replace(/_/g, " ").capitalize();
	}
};

////////////////////////////////////////////////////////
// FEATURE_OPTIONS

my.FEATURE_OPTIONS = {
	NONE				: 0x00,
	IGNORE				: 0X01, // FOR RAP AND ERRORS
	ENGINEERING_HIDDEN	: 0X20,
	SOFTWARE_HIDDEN		: 0X40,
	OBSOLETE			: 0x80,

	toString : function(value) {
		var names = this.getOwnPropertyNamesForValue(value);
		return names.slice(1, names.length - 1).join(", ")
			.replace(/_/g, " ").capitalize();
	}
};

////////////////////////////////////////////////////////
// FeatureIndex

// Parameters:
// - FEATURE_ID      featureId
// - integer         index
// - FEATURE_OPTIONS options
my.FeatureIndex = function(featureId, index, options) {
	this.featureId = featureId;
	this.index     = index;
	this.options   = options;

	this.isValid = function() {		
		if (this.featureId == my.FEATURE_ID.ROOT) {
			return this.index === 0;
		} else {
			return this.index !== 0;
		}
	}
}

my.FeatureIndex.prototype.toString = function() {
	var strId = my.FEATURE_ID.toString(this.featureId);
	if (strId.length > 0) {
		strId = " - " + strId;
	}

	var strOptions = my.FEATURE_OPTIONS.toString(this.options);
	if (strOptions.length > 0) {
		strOptions = "(" + strOptions + ")";
	}

	return "[" + this.index + "] 0x" 
		+ this.featureId.toString(16).padLeft("0", 4)
		+ " " + strId
		+ " " + strOptions;
}

////////////////////////////////////////////////////////
// Feature

// Parameters:
// - HidppFap     fap
// - FeatureIndex featureIndex
my.Feature = function(fap, featureIndex) {
	core.Observable.call(this);
	core.Contract.assert(fap instanceof my.HidppFap,
		"Expected HidppFap.");
	core.Contract.assert(featureIndex instanceof my.FeatureIndex,
		"Expected FeatureIndex.");
	this.fap   = fap;
	this.index = featureIndex;
}

my.Feature.prototype = core.inherit(core.Observable.prototype);

// Object
my.Feature.prototype.toString = function() {
	return this.index.toString();
}

// Overridable
my.Feature.prototype.onEvent = function(eventId, data) {
	this.raiseEvent('newEvent', eventId, data);
}

// Factory method
// Arguments : functionId, payload (implicit)
my.Feature.prototype.createRequest = function(functionId) {
	// Allocate memory
	var request = Object.create(my.HidppFapRequest.prototype);

	// Compute arguments to be passed to HidppFapRequest constructor
	var createArgs = [
		this.fap.device.index,
		this.index.index,
		functionId,
		this.fap.softwareId
	];

	var payload = Array.prototype.slice.call(arguments, 1);
	createArgs = createArgs.concat(payload);

	// Call HidppFapRequest constructor, or default to created object if failed
	request = (my.HidppFapRequest.apply(request, createArgs)) || request;

	return request;
}

////////////////////////////////////////////////////////
// FeatureCache

// Parameters:
// - HidppFap fap
my.FeatureCache = function(fap) {
	core.Contract.assert(fap instanceof my.HidppFap, "Expected HidppFap.");
	this.fap      = fap;
	this.features = { 0 : fap.rootFeature };
}

// Parameters:
// - integer featureId
// - function callback = function(Feature/Feature_subclass feature)
my.FeatureCache.prototype.getFeature = function(featureId, callback) {
	core.Contract.assert(typeof callback == 'function' && callback, "Expected function");
	var self = this;

	// TODO(jracle): optimize - avoid getFeatureIndex and lookup directly in self.features
	self.fap.rootFeature.getFeatureIndex(featureId, function(featureIndex) {
		if (featureIndex.isValid()) {
			var feature = self.features[featureIndex.index];
			if (!feature) {
				feature = self.createFeature(featureIndex);
				self.features[featureIndex.index] = feature;
			}

			callback && callback.apply(self, [feature]);
		} else {

			// Feature not found
			callback && callback.apply(self);
		}
	});
}

// Parameters:
// - function callback = function(Array of Feature/Feature_subclass features)
my.FeatureCache.prototype.getFeatures = function(callback) {
	core.Contract.assert(typeof callback == 'function' && callback, "Expected function");
	var self = this;

	self.getFeature(my.FEATURE_ID.FEATURE_SET, function(featureSet) {

		featureSet.getFeatureCount(function(featureCount) {

			var getFeatureId = function(index) {
				featureSet.getFeatureId(index, function(featureIndex) {
					var feature = self.features[featureIndex.index];
					if (!feature) {
						feature = self.createFeature(featureIndex);
						self.features[featureIndex.index] = feature;
					}

					index++;
					if (index <= featureCount) {
						getFeatureId(index);

					} else {
						callback && callback.apply(self, [self.features]);
					}
				});
			};

			getFeatureId(2);
		});
	});
}

// Parameters:
// - HidppFap     fap
// - FeatureIndex featureIndex
// Returns:
// - Feature (or a sub-class) feature
my.FeatureCache.prototype.createFeature = function(featureIndex) {
	core.Contract.assert(featureIndex instanceof my.FeatureIndex, "Expected FeatureIndex.");
	var className = 'Feature' + featureIndex.featureId.toString(16).padLeft('0', 4);
	var   feature = core.Injector.resolveFromProto(className, this.fap, featureIndex);

	if (!feature) {
		feature = new my.Feature(this.fap, featureIndex);
	}

	return feature;
}

return my;

}(devices || {}));
