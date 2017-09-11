'use strict';

var devices = (function (my) {

////////////////////////////////////////////////////////
// SoftwareId

my.SoftwareIds = {
	0x0 : { busy : true }, // Events
	0x1 : { busy : true }, // Setpoint
	0x2 : { busy : true }, // Unifying
	0x3 : { busy : true }, // Solar App
	0x4 : { busy : true }, // Dell + Gaming SW
	0x5 : { busy : true }, // ?
	0x6 : { busy : false },
	0x7 : { busy : false },
	0x8 : { busy : false },
	0x9 : { busy : false },
	0xA : { busy : false },
	0xB : { busy : false },
	0xC : { busy : false },
	0xD : { busy : false },
	0xE : { busy : true },
	0xF : { busy : true },
}

my.SoftwareId = {
	SharedId         : 0xE,
	BroadcastQueryId : 0xF,

	getFreeId: function() {
		var softwareIds = Object.keys(my.SoftwareIds).reverse();
		for (var i=0; i<softwareIds.length; i++) {
			var softwareId = softwareIds[i];
			if (!my.SoftwareIds[softwareId].busy)
				return softwareId;
		}
		return null;
	},

	setBusy : function(id) {
		my.SoftwareIds[id].busy = true;

		if (my.SoftwareId.value == id) {
			var freeId = my.SoftwareId.getFreeId();
			if (freeId) {
				my.SoftwareId.value = freeId;
			} else {
				my.SoftwareId.value = my.SoftwareId.SharedId;
			}
		}
	},
};

var currentSoftwareId = my.SoftwareId.BroadcastQueryId;

Object.defineProperty(my.SoftwareId, 'value', {
	get: function() {
		return currentSoftwareId;
	},
	set: function(value) {
		currentSoftwareId = value;
	}
});

////////////////////////////////////////////////////////
// HidppPip

my.HidppPip = function(device) {
	this.isBootProtocol = true;
	devices.HidppProtocol.call(this, device);
}

my.HidppPip.prototype = core.inherit(devices.HidppProtocol.prototype);

// Object
my.HidppPip.prototype.toString = function() {
	return "HidppPip";
}

my.HidppPip.prototype.getProtocol = function(callback) {
	core.Contract.assert(typeof callback == 'function'
		&& callback, "Expected callback");
	var self = this;

	self.getSoftwareId(function() {
		var request = new devices.GetProtocolVersionRequest(self.device.index);
		self.requests.enqueue(request);

		request.waitResponse(function(response) {
			var protocol = null;
			if (request.version == 1.0) {
				protocol = new devices.HidppRap(self.device);
			} else {
				protocol = new devices.HidppFap(self.device, request.version);
			}

			core.Contract.assert(protocol instanceof devices.HidppProtocol,
				"Failed to create protocol.");

			callback && callback.apply(self, [protocol]);
		});
	});
}

my.HidppPip.prototype.getSoftwareId = function(callback) {
	core.Contract.assert(typeof callback == 'function'
		&& callback, "Expected callback");
	var self = this;

	var doGetSoftwareId = function() {
		var request = new devices.GetSoftwareIdRequest(self.device.index, my.SoftwareId.value);

		self.requests.enqueue(request);

		request.waitResponse(function(response) {
			if (request.isHidppRap) {
				callback && callback.apply(self);
			} else {
				if (request.rspSoftwareId === my.SoftwareId.BroadcastQueryId) {
					my.SoftwareId.setBusy(request.rspSoftwareId);
					doGetSoftwareId();
				} else {
					callback && callback.apply(self);
				}
			}

		}, function(error) {
			if (request.isHidppRap && error.errCode == devices.RAP_ERROR.ERR_UNKNOWN_DEVICE) {
				self.device.index = devices.HIDPP_DEVICE_INDEX.getRapHubIndex(
					self.device.productId);
				doGetSoftwareId();
			}
		});
	};

	if (my.SoftwareId.value === my.SoftwareId.BroadcastQueryId) {
		doGetSoftwareId();
	} else {
		callback && callback.apply(self);
	}
}

return my;

}(devices || {}));
