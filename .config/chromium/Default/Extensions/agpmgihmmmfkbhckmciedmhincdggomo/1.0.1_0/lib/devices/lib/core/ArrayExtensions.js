'use strict';

Array.prototype.clear = function() {
  while (this.length > 0) {
    this.pop();
  }
};

Array.prototype.contains = function(value) {
	var index = this.indexOf(value);
	return index != -1;
}

Array.prototype.removeAt = function(index) {
	var removedItems = this.splice(index, 1);
	if (removedItems.length > 0) {
		return removedItems[0];
	} else {
		return null;
	}
}

Array.prototype.remove = function(value) {
	var index = this.indexOf(value);
	if (index != -1) {
		return this.removeAt(index);
	} else {
		return null;
	}
}

Array.prototype.removeFirst = function(predicate) {
	var removedItems = this.filter(predicate);

	var removedItem = removedItems.shift();
	if (removedItem) {
		this.remove(removedItem);
	}

	return removedItem;
}

Array.prototype.removeWhere = function(predicate) {
	var self = this;

	var removedItems = this.filter(predicate);

	removedItems.forEach(function(item) {
		self.remove(item);
	});

	return removedItems;
}

Array.prototype.compareTo = function(other) {
	core.Contract.assert(other instanceof Array, "Expected Array.")

	var added = [];
	var removed = [];
	var same = [];

	for (var i = 0; i < this.length; i++) {
		if (other.indexOf(this[i]) == -1) {
			removed.push(this[i]);
		} else {
			same.push(this[i]);
		}
	}

	for (var i = 0; i < other.length; i++) {
		if (this.indexOf(other[i]) == -1) {
			added.push(other[i]);
		}
	}

	return { 
		'added' : added.sort(),
		'removed' : removed.sort(),
		'same' : same.sort()
	};
}

