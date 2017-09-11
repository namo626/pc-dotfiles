'use strict';

var core = (function (my) {

////////////////////////////////////////////////////////
// LOG_LEVEL

var LOG_LEVEL = {
	OFF     : 0,
	ERROR   : 1,
	WARNING : 2,
	INFO    : 3,
	DEBUG	: 4,

	toString : function(value) {
		return this.getOwnPropertyNameForValue(value);
	}
};

////////////////////////////////////////////////////////
// LogItem

var LogItem = function(stack, level, format) {
	this.stack   = stack;
	this.level   = level;
	this.format  = format;
}

////////////////////////////////////////////////////////
// Logger

var Logger = function() {
	core.Closable.call(this);
	this.logItems = [];
	this.worker   = null;
}

Logger.prototype = core.inherit(core.Closable.prototype);

// Closable
Logger.prototype.onClosing = function(callback) {
	if (this.worker) {
		this.logItems.clear();
		clearTimeout(this.worker);
		this.worker = null;
	}

	core.Closable.prototype.onClosing(callback);
}

Logger.prototype.log = function(level) {
	var e = new Error('dummy');
	if (!e || !e.stack) {
		return;
	}

	var stack = e.stack
		.replace(/^[^\(]+?[\n$]/gm, '')	// remove lines without '('
		.replace(/^\s+at\s+/gm, '') 	// remove prefix text ' at '
		.split('\n')					// explode as Array
		.slice(3);						// remove this function call

	// TODO(jracle): fix file:///<path> URLs -> extracted path is not correct
	var filePathRegex = /^[^\(]*\([^:]*:[\/]{1,3}[^:]*[:]{0,1}[^\/]*\/([^:]*)/;
	var result = stack.length > 0 && stack[0].match(filePathRegex);
	if (!result || !result[1]) {
		return;
	}

	var filePath = result[1];
	var logLevel = this.getLogLevel(filePath);
	if (level > logLevel) {
		return;
	}

	if (!this.worker) {
		this.workerFunc(); 
	}

	var format = Array.prototype.slice.call(arguments, 1);
	this.logItems.push(new LogItem(stack, level, format));
}

Logger.prototype.getLogLevel = function(filePath) {
	var getParentDirectory = function(path) {
		var tokens = path.split('/');
		tokens.pop();
		return tokens.join('/');
	}

	var   logLevel = LOG_LEVEL.OFF;
	var lookupPath = filePath;

	do {
		if (core.Log.options[lookupPath]) {
			logLevel = LOG_LEVEL[core.Log.options[lookupPath]];
			break;
		}

		lookupPath = getParentDirectory(lookupPath);
	} while (lookupPath != "");

	if (logLevel == LOG_LEVEL.OFF && lookupPath == ""
			&& core.Log.options[lookupPath]) {
		logLevel = LOG_LEVEL[core.Log.options[lookupPath]];
	}

	return logLevel;
}

Logger.prototype.workerFunc = function() {
	var self = this;

	var doOutput = function(logItem) {
		var args = [ ];
		for (var i=0; i<logItem.format.length; ++i) {
			args.push(logItem.format[i]);
		}

		// Append calling line information.
		args.push("[" + logItem.stack[0] + "]");

		switch (logItem.level) {
			case LOG_LEVEL.ERROR:
				console.error && console.error.apply(console, args);
				break;

			case LOG_LEVEL.WARNING:
				console.warn && console.warn.apply(console, args);
				break;

			case LOG_LEVEL.INFO:
				console.log && console.log.apply(console, args);
				break;

			case LOG_LEVEL.DEBUG:
				console.debug && console.debug.apply(console, args);
				break;

			default:
				break;
		}
	}

	var doLog = function() {
		// Log max 10 items in a raw.
		var maxItemsToLog = 10;
		var       logItem = null;

		do {
			logItem = self.logItems.shift();

			if (logItem) {
				doOutput(logItem);
				maxItemsToLog--;
			}
		} while (logItem && maxItemsToLog > 0);

		// Sleep 50ms and log again.
		self.worker = setTimeout(doLog, 50);
	};

	doLog();
}

////////////////////////////////////////////////////////
// Log

var logger = null;

var getLogger = function() {
	if (!logger) {
		logger = new Logger();
	}
	return logger;
}

my.Log = {
	options: null
	, error: function() {
		this._log(LOG_LEVEL.ERROR, arguments);
	},
	debug: function() {
		this._log(LOG_LEVEL.DEBUG, arguments);
	},
	info: function() {
		this._log(LOG_LEVEL.INFO, arguments);
	},
	warning: function() {
		this._log(LOG_LEVEL.WARNING, arguments);
	},
	close: function(callback) {
		var self = this;
		if (logger) {
			logger.close(function() {
				logger = null;
				callback && callback.apply(self);
			})
		} else {
			callback && callback.apply(self);
		}
	},
	_log: function(level, logArgs) {
		if (!this.options) {
			return;
		}
		if (!logArgs) {
			logArgs = arguments;
		}
		var args = [ level ];
		for (var i=0; i<logArgs.length; ++i) {
			args.push(logArgs[i]);
		};
		
		var loggerInstance = getLogger();
		loggerInstance.log.apply(loggerInstance, args);
	},
}

return my;

}(core || {}));
