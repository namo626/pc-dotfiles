'use strict';

var core = (function (my) {

////////////////////////////////////////////////////////
// Filter

// predicate: function(object item, function callback)
// callback returns Boolean
my.Filter = function(predicate) {
	core.Contract.assert(typeof predicate == 'function'
		&& predicate, "Expected function");
	this.predicate = predicate;
}

// Parameters
// item: any object
// callback: returns Boolean when matched (asynchronous)
my.Filter.prototype.match = function(item, callback) {
	this.predicate(item, callback);
}

////////////////////////////////////////////////////////
// CompositeFilter

my.CompositeFilter = function() {
	my.Filter.call(this);
	this.includedFilters = [];
	this.excludedFilters = [];
}

my.CompositeFilter.prototype = core.inherit(my.Filter.prototype);

// Filter
my.CompositeFilter.prototype.match = function(item, callback) {
	var self = this;

	self.isIncluded(item, function(result) {
		if (!result) {
			callback.apply(self, [false]);
		} else {
			self.isExcluded(item, function(result) {
				if (result) {
					callback.apply(self, [false]);
				} else {
					callback.apply(self, [true]);
				}
			});
		}
	});
}

// predicate: function(item, callback)
my.CompositeFilter.prototype.include = function(predicate) {
	this.includedFilters.push(new my.Filter(predicate));
	return this;
}

// predicate: function(item, callback)
my.CompositeFilter.prototype.exclude = function(predicate) {
	this.excludedFilters.push(new my.Filter(predicate));
	return this;
}

my.CompositeFilter.prototype.isIncluded = function(item, callback) {
	var self = this;

	if (self.includedFilters.length > 0) {
		var matchIncludedItem = function(index) {
			var filter = self.includedFilters[index];
			filter.match(item, function(result) {
				if (!result) {
					callback.apply(self, [false]);
				} else {
					index++;
					if (index < self.includedFilters.length) {
						matchIncludedItem(index);
					} else {
						callback.apply(self, [result]);
					}
				}
			});
		}

		matchIncludedItem(0);

	} else {
		callback(false);
	}
}

my.CompositeFilter.prototype.isExcluded = function(item, callback) {
	var self = this;

	if (self.excludedFilters.length > 0) {
		var matchExcludedItem = function(index) {
			var filter = self.excludedFilters[index];
			filter.match(item, function(result) {
				if (result) {
					callback.apply(self, [true]);
				} else {
					index++;
					if (index < self.excludedFilters.length) {
						matchExcludedItem(index);
					} else {
						callback.apply(self, [result]);
					}
				}
			});
		}

		matchExcludedItem(0);

	} else {
		callback(false);
	}
}

return my;

}(core || {}));
