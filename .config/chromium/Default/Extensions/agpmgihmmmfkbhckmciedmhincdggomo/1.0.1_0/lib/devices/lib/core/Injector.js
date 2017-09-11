'use strict';

var core = (function (my) {

// TODO(jracle): refactor resolve methods.

// http://krasimirtsonev.com/blog/article/Dependency-injection-in-JavaScript

// Dependency Injector/Resolver
my.Injector = {
	dependencies: {},
	register: function(key, value) {
		this.dependencies[key] = value;
	},
	resolve: function() {
		var func, deps, scope, args = [], self = this;

		if(typeof arguments[0] === 'string') {
			func = arguments[1];
			deps = arguments[0].replace(/ /g, '').split(',');
			scope = arguments[2] || {};
		} else {
			func = arguments[0];
			deps = func.toString()
				.match(/^function\s*[^\(]*\(\s*([^\)]*)\)/m)[1]
				.replace(/ /g, '')
				.split(',');
			scope = arguments[1] || {};
		}
		return function() {
			var a = Array.prototype.slice.call(arguments, 0);
			for(var i=0; i<deps.length; i++) {
				var d = deps[i];
				args.push(self.dependencies[d] && d != ''
					? self.dependencies[d] 
					: a.shift());
			}
			func.apply(scope || {}, args);
		}
	},
	resolveFromProto: function(key) {
		var value = null;
		var classObj = this.dependencies[key];
		if (classObj) {
			value = Object.create(classObj.prototype);
			var createArgs = Array.prototype.slice.call(arguments, 1);
			classObj.apply(value, createArgs);
		}
		return value;
	},
	close: function(callback) {
		var self = this;
		var dependencyNames = Object.getOwnPropertyNames(self.dependencies);

		if (dependencyNames.length > 0) {
			var closeDependency = function(index) {
				var closeNextDependency = function() {
					index++;
					if (index < dependencyNames.length) {
						closeDependency(index);
					} else {
						callback && callback.apply(self);
					}
				}
				var dependency = self.dependencies[dependencyNames[index]];
				if (dependency instanceof core.Closable) {
					dependency.close(closeNextDependency);
				} else {
					closeNextDependency();
				}
			};

			closeDependency(0);
		}
	}
}

return my;

}(core || {}));
