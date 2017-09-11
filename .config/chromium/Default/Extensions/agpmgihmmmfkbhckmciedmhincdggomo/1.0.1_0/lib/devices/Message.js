'use strict';

var devices = (function (my) {

////////////////////////////////////////////////////////
// MSG_DIRECTION

my.MSG_DIRECTION = {
	IN: 0, // Device -> Host
	OUT: 1 // Host   -> Device
};

////////////////////////////////////////////////////////
// Message

my.Message = function(direction, data) {
	core.Observable.call(this);
	core.Contract.assert(data instanceof ArrayBuffer, "Expected ArrayBuffer.");
	core.Contract.assert(
		direction == my.MSG_DIRECTION.IN ||
		direction == my.MSG_DIRECTION.OUT, "Out of range.");

	this.direction = direction;
	this.data      = data;
}

my.Message.prototype = core.inherit(core.Observable.prototype);

// Object
my.Message.prototype.toString = function() {
	var str = "";
	switch(this.direction) {
		case my.MSG_DIRECTION.IN:
			str += "[IN] ";
			break;
		case my.MSG_DIRECTION.OUT:
			str += "[OUT] ";
			break;
		default:
			throw new RangeError();
	}
	str += this.data.toStringEx();
	return str;
}

////////////////////////////////////////////////////////
// OutputMessage

my.OutputMessage = function(data) {
	my.Message.call(this, my.MSG_DIRECTION.OUT, data);
}

my.OutputMessage.prototype = core.inherit(my.Message.prototype);

////////////////////////////////////////////////////////
// InputMessage

my.InputMessage = function(data) {
	my.Message.call(this, my.MSG_DIRECTION.IN, data);
}

my.InputMessage.prototype = core.inherit(my.Message.prototype);

////////////////////////////////////////////////////////
// Notification

my.Notification = function(data) {
	my.InputMessage.call(this, data);
}

my.Notification.prototype = core.inherit(my.InputMessage.prototype);

////////////////////////////////////////////////////////
// Response

my.Response = function(data) {
	my.InputMessage.call(this, data);
	this.errCode = 0;

	Object.defineProperty(this, 'success', {
		get: function() {
			return this.errCode === 0;
		}
	});

	Object.defineProperty(this, 'isError', {
		get: function() {
			return this.errCode !== 0;
		}
	});
}

my.Response.prototype = core.inherit(my.InputMessage.prototype);

////////////////////////////////////////////////////////
// MATCH_RESULT

my.MATCH_RESULT = {
	SUCCESS  : 0,
	ERROR    : 1,
	NO_MATCH : 2,
};

////////////////////////////////////////////////////////
// Request

my.Request = function(data) {
	my.OutputMessage.call(this, data);
	this.registerProperty('response');
	this.maxRetryCount = 10;
	this.responseTimeoutMs = 2000;
}

my.Request.prototype = core.inherit(my.OutputMessage.prototype);

// returns true if matched (response or error)
my.Request.prototype.tryMatch = function(dataIn) {
	var result = this.matches(dataIn);

	switch(result.matchResult) {
		case my.MATCH_RESULT.SUCCESS:
			var rsp = new my.Response(dataIn);
			this.onMatched(rsp);
			this.response = rsp;
			return true;

		case my.MATCH_RESULT.ERROR:
			var rspErr = new my.Response(dataIn);
			rspErr.errCode = result.errCode;
			this.response = rspErr;
			return true;

		case my.MATCH_RESULT.NO_MATCH:
			return false;

		default:
			throw new RangeError();
	}
}

my.Request.prototype.waitResponse = function(
		successCallback, errorCallback) {
	core.Contract.assert(typeof successCallback == 'function'
		&& successCallback, "Expected function.");
	var self = this;

	self.waitProperty('response', function(response) {
		if (response.success) {
			successCallback && successCallback.apply(self, [response]);
		} else {
			errorCallback && errorCallback.apply(self, [response]);
		}
	});
}

my.Request.prototype.onMatched = function(response) {
	// Overridable
}

// abstract
// dataIn : ArrayBuffer
// returns { "matchResult" : MATCH_RESULT, "errCode" : RAP_ERROR };
my.Request.prototype.matches = function(dataIn) {
	throw new TypeError("Abstract method");
}

return my;

}(devices || {}));
