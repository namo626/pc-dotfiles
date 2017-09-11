'use strict';

var core = (function (my) {

// Dispose pattern
my.Closable = function() {
	this.closing = false;
	this.closed = false;
}

my.Closable.prototype.close = function(callback) {
	var self = this;

	if (callback) {
		core.Contract.assert(typeof callback === 'function'
			&& callback, "Expected function");
	}

	if (self.closing || self.closed) {
		callback && callback.apply(self);
	} else {
		self.closing = true;
		self.onClosing(function() {
			self.closing = false;
			self.closed = true;
			callback && callback.apply(self);
		});
	}
}

// Overridable
my.Closable.prototype.onClosing = function(callback) {
	callback && callback.apply(this);
}

return my;

}(core || {}));
