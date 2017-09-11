'use strict';

var devices = (function (my) {

////////////////////////////////////////////////////////
// HidppRequest

my.HidppRequest = function(deviceIndex) {
	// We allocate 64 bytes, that we'll shrink later on
	// (while calling prepareForWrite method)
	var data = new ArrayBuffer(devices.HIDPP_REPORT_SIZE.VERY_LONG);

	var array = new Uint8Array(data);
	array[1] = deviceIndex;

	devices.Request.call(this, data);
	this.deviceIndex = deviceIndex;

	this.isCritical = true;
	this.forcedSize = 0;
}

my.HidppRequest.prototype = core.inherit(devices.Request.prototype);

my.HidppRequest.prototype.prepareForWrite = function() {
	var view = new DataView(this.data);

	var i;
	for (i=this.data.byteLength-1; i >= 0; --i) {
		if (view.getUint8(i) != 0x00) {
			break;
		}
	}

	if (this.forcedSize === devices.HIDPP_REPORT_SIZE.SHORT
    	|| (!this.forcedSize && i < devices.HIDPP_REPORT_SIZE.SHORT)) {
		view.setUint8(0, devices.REPORT_ID.HIDPP_SHORT);
		this.data = this.data.slice(0, devices.HIDPP_REPORT_SIZE.SHORT);

	} else if (this.forcedSize === devices.HIDPP_REPORT_SIZE.LONG
		|| (!this.forcedSize && i < devices.HIDPP_REPORT_SIZE.LONG)) {
		view.setUint8(0, devices.REPORT_ID.HIDPP_LONG);
		this.data = this.data.slice(0, devices.HIDPP_REPORT_SIZE.LONG);

	} else if (this.forcedSize === devices.HIDPP_REPORT_SIZE.VERY_LONG
		|| (!this.forcedSize && i < devices.HIDPP_REPORT_SIZE.VERY_LONG)) {
		view.setUint8(0, devices.REPORT_ID.HIDPP_VERY_LONG);
		core.Contract.assert(this.data.byteLength == devices.HIDPP_REPORT_SIZE.VERY_LONG);

	} else {
		core.Contract.assert(false, "Buffer is too long");
		return false;
	}

	return true;
}

////////////////////////////////////////////////////////
// GetSoftwareIdRequest

my.GetSoftwareIdRequest = function(deviceIndex) {
	my.HidppRequest.call(this, deviceIndex);

	this.functionId = 1;
	this.softwareId = devices.SoftwareId.value;

	var array = new Uint8Array(this.data);
	array[3] = this.functionId << 4 | this.softwareId;
}

my.GetSoftwareIdRequest.prototype = core.inherit(my.HidppRequest.prototype);

// dataIn : ArrayBuffer
// returns { "matchResult" : MATCH_RESULT, "errCode" : RAP_ERROR };
my.GetSoftwareIdRequest.prototype.matches = function(dataIn) {
	core.Contract.assert(dataIn instanceof ArrayBuffer, "Expected ArrayBuffer.");

	var result = devices.HidppRapRequest.prototype.matches.call(this, dataIn);

	if (result.matchResult == devices.MATCH_RESULT.ERROR) {
		this.isHidppRap = true;
		if (result.errCode == devices.RAP_ERROR.ERR_INVALID_SUBID) {
			result.matchResult = devices.MATCH_RESULT.SUCCESS;
		} else {
			this.errCode = result.errCode;
		}
	} else {
		var reqView = new DataView(this.data);
		var rspView = new DataView(dataIn);

		if (rspView.getUint8(1) == reqView.getUint8(1)
		 && rspView.getUint8(2) == 0x00				// Root feature
		 && (rspView.getUint8(3) & 0xF0) == 0x10) {	// 'hidden' function 1 : Ping
			result.matchResult = devices.MATCH_RESULT.SUCCESS;
			this.rspSoftwareId = rspView.getUint8(3) & 0x0F;
		}
	}

	return result;
}

////////////////////////////////////////////////////////
// GetProtocolVersionRequest

my.GetProtocolVersionRequest = function(deviceIndex) {
	my.HidppRequest.call(this, deviceIndex);

	this.functionId = 1;
	this.softwareId = devices.SoftwareId.value;

	var array = new Uint8Array(this.data);
	array[3] = this.functionId << 4 | this.softwareId;
}

my.GetProtocolVersionRequest.prototype = core.inherit(my.HidppRequest.prototype);

// dataIn : ArrayBuffer
// returns { "matchResult" : MATCH_RESULT, "errCode" : RAP_ERROR };
my.GetProtocolVersionRequest.prototype.matches = function(dataIn) {
	core.Contract.assert(dataIn instanceof ArrayBuffer, "Expected ArrayBuffer.");

	var result = devices.HidppRapRequest.prototype.matches.call(this, dataIn);
	
	if (result.matchResult == devices.MATCH_RESULT.ERROR) {
		if (result.errCode == devices.RAP_ERROR.ERR_INVALID_SUBID) {
			result.matchResult = devices.MATCH_RESULT.SUCCESS;
			this.version = 1.0;
		} else {
			this.errCode = result.errCode;
		}
	} else {
		var reqView = new DataView(this.data);
		var rspView = new DataView(dataIn);
		
		if (rspView.getUint8(1) == reqView.getUint8(1)
		 && rspView.getUint8(2) == 0x00			 // Root feature
		 && (rspView.getUint8(3) & 0xF0) == 0x10 // 'hidden' function 1
		 && (rspView.getUint8(3) & 0x0F) == this.softwareId) {
			result.matchResult = devices.MATCH_RESULT.SUCCESS;
			var major =  rspView.getUint8(4) & 0x0F;
			var minor = (rspView.getUint8(4) & 0xF0) >> 4;
			this.version = parseFloat(major + "." + minor);
		}
	}

	return result;
}

return my;

}(devices || {}));
