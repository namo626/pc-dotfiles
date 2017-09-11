'use strict';

var core = (function (my) {

// Observable collection
// - To be used in repository patterns
my.ObservableArray = function() {
	Array.call(this);
	this.observers = [];
}

my.ObservableArray.prototype = core.inherit(Array.prototype);

// Array
my.ObservableArray.prototype.pop = function() {
	var item = Array.prototype.pop.apply(this);
	if (item) {
		this.raiseCollectionChanged([], [item], []);
	}
	return item;
}

// Array
my.ObservableArray.prototype.push = function() {
	Array.prototype.push.apply(this, arguments);
	var add = Array.prototype.slice.call(arguments, 0);
	this.raiseCollectionChanged(add, [], []);
	return this.length;
}

// Array
my.ObservableArray.prototype.reduce = function() {
	throw new Error("Not implemented");
}

// Array
my.ObservableArray.prototype.reduceRight = function() {
	throw new Error("Not implemented");
}

// Array
my.ObservableArray.prototype.reverse = function() {
	throw new Error("Not implemented");
}

// Array
my.ObservableArray.prototype.shift = function() {
	throw new Error("Not implemented");
}

// Array
my.ObservableArray.prototype.slice = function() {
	throw new Error("Not implemented");
}

// Array
my.ObservableArray.prototype.sort = function() {
	throw new Error("Not implemented");
}

// Array
my.ObservableArray.prototype.splice = function() {
	var    add = Array.prototype.slice.call(arguments, 2);
	var remove = Array.prototype.splice.apply(this, arguments);
	this.raiseCollectionChanged(add, remove, []);
	return remove;
}

// Array
my.ObservableArray.prototype.unshift = function() {
	throw new Error("Not implemented");
}

// Array
my.ObservableArray.prototype.reverse = function() {
	throw new Error("Not implemented");
}

// Array
my.ObservableArray.prototype.reverse = function() {
	throw new Error("Not implemented");
}

my.ObservableArray.prototype.observeCollectionChanged = function(callback) {
	core.Contract.assert(typeof callback == 'function'
		&& callback, "Expected function.");
	this.observers.push(callback);
}

my.ObservableArray.prototype.unobserveCollectionChanged = function(callback) {
	core.Contract.assert(typeof callback == 'function'
		&& callback, "Expected function.");
	this.observers.remove(callback);
}

my.ObservableArray.prototype.raiseCollectionChanged = function(add, remove, update) {
	core.Contract.assert(add    instanceof Array, "Expected Array.")
	core.Contract.assert(remove instanceof Array, "Expected Array.")
	core.Contract.assert(update instanceof Array, "Expected Array.")

	// Notify observers
	for (var i=0; i<this.observers.length; i++) {
		this.observers[i]({
			add    : add,
			remove : remove,
			update : update
		});
	}
}

return my;

}(core || {}));
