'use strict';

var unifying = (function (my) {

var BATTERY_LEVEL = {
	UNKNOWN           : 0,
	FULL              : 1,
	GOOD              : 2,
	FAIR              : 3,
	LOW               : 4,
	CRITICAL          : 5,
	CHARGING          : 6,
	CHARGING_COMPLETE : 7,
	CHARGING_ERROR    : 8,

	toString : function(value, l10n) {
		var propertyName = this.getOwnPropertyNameForValue(value);
		return l10n["BATTERY_LEVEL_" + propertyName];
	}
};

/* Unifying *******************************************/

my.Unifying = function(deviceProvider, controller_scope) {
	core.StateMachine.call(this);

	core.Contract.assert(deviceProvider instanceof devices.DeviceProvider,
		"Expected DeviceProvider.");
	this.deviceProvider   = deviceProvider;
	this.controller_scope = controller_scope;

	this.canExit = controller_scope.canExit;
	this.l10n    = controller_scope.l10n;

	this.receivers = [];
	this.removedDevices = [];

	this.onDevicesChangedHandle = this.onDevicesChanged.bind(this);
	this.deviceProvider.devices
		.observeCollectionChanged(this.onDevicesChangedHandle);

	// Build states
	this.request_permissions  = new my.RequestPermissionsState(this);
	this.welcome              = new            my.WelcomeState(this);
	this.advanced             = new           my.AdvancedState(this);
	this.restart              = new            my.RestartState(this);
	this.confirm_pairing      = new     my.ConfirmPairingState(this);
	this.device_paired        = new       my.DevicePairedState(this);
	this.pairing_failed       = new      my.PairingFailedState(this);
	this.paired_devices       = new      my.PairedDevicesState(this);
	this.plug_receiver        = new       my.PlugReceiverState(this);
	this.unplug_receiver      = new     my.UnplugReceiverState(this);
	this.unplug_last_receiver = new my.UnplugLastReceiverState(this);
	this.receiver_full        = new       my.ReceiverFullState(this);
	this.too_many_devices     = new     my.TooManyDevicesState(this);
	this.move_devices         = new        my.MoveDevicesState(this);
	this.wrong_receiver       = new      my.WrongReceiverState(this);

	// Enter 'request_permissions' state
	this.setState(this.request_permissions);
}

my.Unifying.prototype = core.inherit(core.StateMachine.prototype);

// Closable
my.Unifying.prototype.onClosing = function(callback) {
	this.deviceProvider.devices
		.unobserveCollectionChanged(this.onDevicesChangedHandle);
	core.Closable.prototype.onClosing(callback);
}

my.Unifying.prototype.confirm_exit = function() {
	this.controller_scope.confirm_exit();
}

my.Unifying.prototype.exit = function() {
	this.controller_scope.exit();
}

my.Unifying.prototype.learnMore = function() {
	this.controller_scope.learnMore();
}

my.Unifying.prototype.troubleshoot = function() {
	this.controller_scope.troubleshoot();
}

my.Unifying.prototype.correct_device = function() {
	this.controller_scope.correct_device();
}

my.Unifying.prototype.replacing_device = function() {
	this.controller_scope.replacing_device();
}

my.Unifying.prototype.updateController = function(applyFunction) {
	if (!this.closing) {
		if (applyFunction) {
			core.Contract.assert(typeof applyFunction == 'function'
				&& applyFunction, "Expected function.");
			this.controller_scope.$evalAsync(applyFunction);
		} else {
			this.controller_scope.$evalAsync();
		}
	}
}

my.Unifying.prototype.onDevicesChanged = function(change) {
	var self = this;

	var updateBatteryLevel = function(device) {
		core.Contract.assert(device instanceof devices.Device,
			"Expected Device.");

		device.waitProperty('protocol', function(protocol) {
			// HID++ 1.0
			if (protocol instanceof devices.HidppRap) {
				var request = new devices.GetBatteryMileage(device.index);
				request.isCritical = false;
				request.maxRetryCount = 0;
				protocol.requests.enqueue(request);

				request.waitResponse(function(response) {
					var updateRapBatteryMileage = function(mileage) {
						var level = BATTERY_LEVEL.UNKNOWN;
						if (mileage.chargeState !== 0) { // Charging
							switch (mileage.chargeState) {
								case 0x01:
									level = BATTERY_LEVEL.CHARGING;
									break;
								case 0x02:
									level = BATTERY_LEVEL.CHARGING_COMPLETE;
									break;
								case 0x03:
									level = BATTERY_LEVEL.CHARGING_ERROR;
									break;
								default:
									level = BATTERY_LEVEL.CHARGING;
									break;
							}
						} else { // Discharging
							if (mileage.charge > 0 && mileage.charge <= 30 )
								level = BATTERY_LEVEL.LOW;
							else if (mileage.charge > 30 && mileage.charge <= 100 )
								level = BATTERY_LEVEL.GOOD;
						}
						device['battery'] = BATTERY_LEVEL.toString(level, self.l10n)
							.capitalize();
					}

					updateRapBatteryMileage(response.mileage);
					protocol.observeEvent('batteryMileageChanged',
						updateRapBatteryMileage);

				}, function (errorResponse) {
					// If mileage not supported, fallback to GetBatteryStatus
					if (errorResponse.errCode === devices.RAP_ERROR.ERR_INVALID_ADDRESS) {
						var request = new devices.GetBatteryStatus(device.index);
						protocol.requests.enqueue(request);

						request.waitResponse(function(response) {
							var updateRapBatteryStatus = function(status) {
								var level = BATTERY_LEVEL.UNKNOWN;
								if (status.charge_level !== 0) { // Charging
									switch (status.charge_level) {
										case 0x23:
											level = BATTERY_LEVEL.CHARGING_ERROR;
											break;
										case 0x22:
											level = BATTERY_LEVEL.CHARGING_COMPLETE;
											break;
										case 0x21:
											level = BATTERY_LEVEL.CHARGING;
											break;
										default:
											level = BATTERY_LEVEL.CHARGING;
											break;
									}
								} else { // Discharging
									switch (status.level) {
										case 0:
											level = BATTERY_LEVEL.UNKNOWN;
											break;
										case 1:
											level = BATTERY_LEVEL.LOW;
											break;
										default:
											level = BATTERY_LEVEL.GOOD;
											break;
									}
								}

								device['battery'] = BATTERY_LEVEL.toString(level, self.l10n)
									.capitalize();
							}

							updateRapBatteryStatus(response.status);
							protocol.observeEvent('batteryStatusChanged',
								updateRapBatteryStatus);
						});
					}
				});

			// HID++ >= 2.0
			} else if (protocol instanceof devices.HidppFap) {
				protocol.getFeature(devices.FEATURE_ID.BATTERY, function(feature) {
					if (feature) {
						var getFapBatteryLevel = function(currentLevel, nextLevel, nbLevels) {
							var level = BATTERY_LEVEL.UNKNOWN;
							switch(nbLevels) {
								case 2: {
									// [100..11] Good
									// [10..1]   Critical
									if (currentLevel > 10)
										level = BATTERY_LEVEL.GOOD;
									else if (currentLevel >= 0)
										level = BATTERY_LEVEL.CRITICAL;
								}
								break;

								case 3: {
									// [100..31] Full
									// [30..11]  Low
									// [10..1]   Critical
									if (currentLevel > 30)
										level = BATTERY_LEVEL.FULL;
									else if (currentLevel > 10)
										level = BATTERY_LEVEL.LOW;
									else if (currentLevel >= 0)
										level = BATTERY_LEVEL.CRITICAL;
								}
								break;

								case 4:
								{
									// [100..81] Full
									// [80..31]  Good
									// [30..11]  Low
									// [10..1]   Critical
									if (currentLevel > 80)
										level = BATTERY_LEVEL.FULL;
									else if (currentLevel > 30)
										level = BATTERY_LEVEL.GOOD;
									else if (currentLevel > 10)
										level = BATTERY_LEVEL.LOW;
									else if (currentLevel >= 0)
										level = BATTERY_LEVEL.CRITICAL;
								}
								break;

								case 5: {
									// [100..81] Full
									// [80..61]  Good
									// [60..41]  Fair
									// [40..21]  Low
									// [20..1]   Critical
									if (currentLevel > 80)
										level = BATTERY_LEVEL.FULL;
									else if (currentLevel > 60)
										level = BATTERY_LEVEL.GOOD;
									else if (currentLevel > 40)
										level = BATTERY_LEVEL.FAIR;
									else if (currentLevel > 20)
										level = BATTERY_LEVEL.LOW;
									else if (currentLevel >= 0)
										level = BATTERY_LEVEL.CRITICAL;
								}
								break;

								default:
								break;
							}

							return level;
						}

						feature.getBatteryCapability(function(capability) {
							var updateFapBatteryLevel = function(status) {
								var level = BATTERY_LEVEL.UNKNOWN;
								
								switch (status.status) {
									case devices.FAP_BATTERY_STATUS.DISCHARGING: {
										level = getFapBatteryLevel(
											status.level,
											status.nextLevel,
											capability.nbLevels);
										}
										break;

									case devices.FAP_BATTERY_STATUS.START_CHARGING:
									case devices.FAP_BATTERY_STATUS.RECHARGING:
									case devices.FAP_BATTERY_STATUS.RECHARGING_BELOW_OPTIMAL:
										level = BATTERY_LEVEL.CHARGING;
										break;

									case devices.FAP_BATTERY_STATUS.CHARGE_IN_FINAL_STATE:
									case devices.FAP_BATTERY_STATUS.CHARGE_COMPLETE:
										level = BATTERY_LEVEL.CHARGING_COMPLETE;
										break;

									case devices.FAP_BATTERY_STATUS.ERROR_INVALID_BATTERY_TYPE:
									case devices.FAP_BATTERY_STATUS.ERROR_THERMAL:
									case devices.FAP_BATTERY_STATUS.ERROR_OTHER:
										level = BATTERY_LEVEL.CHARGING_ERROR;
										break;

									default:
										break;
								}

								device['battery'] = BATTERY_LEVEL.toString(level, self.l10n)
									.capitalize();
							}

							feature.getBatteryStatus(updateFapBatteryLevel);
							feature.observeEvent('batteryStatusChanged', updateFapBatteryLevel);
						});
					}
				});
			}
		});
	}

	var findSameDevices = function(device, callback) {
		core.Contract.assert(device instanceof devices.Device, "Expected Device.");
		core.Contract.assert(device.isHidpp, "Expected Hid++ device.");
		core.Contract.assert(device.index != devices.HIDPP_DEVICE_INDEX.RECEIVER,
			"Expected wireless device.");
		var sameDevices = [];

		// We check parent's serial number in case same device is found twice.
		// This is a bug on some chromebooks (running Linux 3.4).
		var otherHidppDevices = self.deviceProvider.devices.filter(
			function(dev) {
				return dev.isHidpp
					&& dev.index != devices.HIDPP_DEVICE_INDEX.RECEIVER
					&& dev.parent !== device.parent
					&& dev.parent.serialNumber != device.parent.serialNumber;
			}
		);

		if (otherHidppDevices.length > 0) {
			device.waitProperty('serialNumber', function(serialNumber) {
				var doFindSameDevices = function(index) {
					var dev = otherHidppDevices[index];
					dev.waitProperty('serialNumber', function(devSN) {
						if (serialNumber == devSN) {
							sameDevices.push(dev);
						}
						index++;
						if (index < otherHidppDevices.length) {
							doFindSameDevices(index);
						} else {
							callback && callback.apply(self, [sameDevices]);
						}
					});
				};
				doFindSameDevices(0);
			});
		} else {
			callback && callback.apply(self, [sameDevices]);
		}
	}

	var unpairGhostDevices = function(device) {
		core.Contract.assert(device instanceof devices.Device, "Expected Device.");
		core.Contract.assert(device.isHidpp, "Expected Hid++ device.");
		core.Contract.assert(device.index != devices.HIDPP_DEVICE_INDEX.RECEIVER,
			"Expected wireless device.");

		if (device.connected) {

			findSameDevices(device, function(devs) {
				devs.forEach(function(dev) {
					core.Log.info("Will unpair ghost");
					dev.unpair(function() {
						self.removedDevices.remove(dev);
					});
				});
			});

		} else {

			findSameDevices(device, function(devs) {
				if (devs.some(function(dev) {
					return dev.connected;
				})) {
					devs.push(device);
					devs.forEach(function(dev) {
						if (!dev.connected) {
							core.Log.info("Will unpair (un-connected) ghost");
							dev.unpair(function() {
								self.removedDevices.remove(dev);
							});
						}
					});
				}
			});
		}
	}

	var storeRemovedDevice = function(device) {
		// Look for HID++ wireless devices with same serial number
		// attached to this computer.
		findSameDevices(device, function(devices) {
			if (devices.length == 0 && !self.removedDevices.contains(device)) {
				self.removedDevices.push(device);
			}
		});
	}

	var onDevicePropertyChanged = function(name, value) {
		var device = this;

		if (!device.isHidpp) {
			return;
		}

		if (device.index != devices.HIDPP_DEVICE_INDEX.RECEIVER
				&& name == 'connected') {
			// Wireless device connection status changed.
			unpairGhostDevices(device);

			if (value) { // Device is connected.
				updateBatteryLevel(device);
			}
		}

		// Refresh device property in GUI.
		self.updateController(function() {
			device[name];
		});
	}

	var onDeviceChildrenChanged = function() {
		var device = this;
		var hidppChildren = device.children.filter(function(dev) {
			return dev.isHidpp;
		})

		device.isFull = hidppChildren.length == 6;

		var devicesPaired = "";

		if (hidppChildren.length > 0) {
			var nbKbd=0;  var nbMse=0; var nbNum=0;  var nbPres=0;
			var nbHead=0; var nbSpk=0; var nbRem=0;  var nbTrk=0;
			var nbTpad=0; var nbTab=0; var nbGpad=0; var nbJoy=0;
			hidppChildren.forEach(function(dev) {
				switch (dev['rapDeviceType']) {
					case devices.RAP_DEVICE_TYPE.KEYBOARD:       nbKbd++;  break;
					case devices.RAP_DEVICE_TYPE.MOUSE:          nbMse++;  break;
					case devices.RAP_DEVICE_TYPE.NUMPAD:         nbNum++;  break;
					case devices.RAP_DEVICE_TYPE.PRESENTER:      nbPres++; break;
					case devices.RAP_DEVICE_TYPE.HEADSET:        nbHead++; break;
					case devices.RAP_DEVICE_TYPE.SPEAKER_PHONE:  nbSpk++;  break;
					case devices.RAP_DEVICE_TYPE.REMOTE_CONTROL: nbRem++;  break;
					case devices.RAP_DEVICE_TYPE.TRACKBALL:      nbTrk++;  break;
					case devices.RAP_DEVICE_TYPE.TOUCHPAD:       nbTpad++; break;
					case devices.RAP_DEVICE_TYPE.TABLET:         nbTab++;  break;
					case devices.RAP_DEVICE_TYPE.GAMEPAD:        nbGpad++; break;
					case devices.RAP_DEVICE_TYPE.JOYSTICK:       nbJoy++;  break;
					default: break;
				}
			});

			if (nbKbd > 0) {
				devicesPaired += (nbKbd == 1 ?
					self.l10n.format(self.l10n.ADVANCED_KEYBOARD,  { NB_KEYBOARDS : nbKbd }) :
					self.l10n.format(self.l10n.ADVANCED_KEYBOARDS, { NB_KEYBOARDS : nbKbd }));
			}
			if (nbMse > 0) {
				devicesPaired += ", " + (nbMse == 1 ?
					self.l10n.format(self.l10n.ADVANCED_MOUSE, { NB_MICE : nbMse }) :
					self.l10n.format(self.l10n.ADVANCED_MICE,  { NB_MICE : nbMse }));
			}
			if (nbNum > 0) {
				devicesPaired += ", " + (nbNum == 1 ?
					self.l10n.format(self.l10n.ADVANCED_NUMPAD,  { NB_NUMPADS : nbNum }) :
					self.l10n.format(self.l10n.ADVANCED_NUMPADS, { NB_NUMPADS : nbNum }));
			}
			if (nbPres > 0) {
				devicesPaired += ", " + (nbPres == 1 ?
					self.l10n.format(self.l10n.ADVANCED_PRESENTER,  { NB_PRESENTERS : nbPres }) :
					self.l10n.format(self.l10n.ADVANCED_PRESENTERS, { NB_PRESENTERS : nbPres }));
			}
			if (nbHead > 0) {
				devicesPaired += ", " + (nbHead == 1 ?
					self.l10n.format(self.l10n.ADVANCED_HEADSET,  { NB_HEADSETS : nbHead }) :
					self.l10n.format(self.l10n.ADVANCED_HEADSETS, { NB_HEADSETS : nbHead }));
			}
			if (nbSpk > 0) {
				devicesPaired += ", " + (nbSpk == 1 ?
					self.l10n.format(self.l10n.ADVANCED_SPEAKER,  { NB_SPEAKERS : nbSpk }) :
					self.l10n.format(self.l10n.ADVANCED_SPEAKERS, { NB_SPEAKERS : nbSpk }));
			}
			if (nbRem > 0) {
				devicesPaired += ", " + (nbRem == 1 ?
					self.l10n.format(self.l10n.ADVANCED_REMOTE,  { NB_REMOTES : nbRem }) :
					self.l10n.format(self.l10n.ADVANCED_REMOTES, { NB_REMOTES : nbRem }));
			}
			if (nbTrk > 0) {
				devicesPaired += ", " + (nbTrk == 1 ?
					self.l10n.format(self.l10n.ADVANCED_TRACKBALL,  { NB_TRACKBALLS : nbTrk }) :
					self.l10n.format(self.l10n.ADVANCED_TRACKBALLS, { NB_TRACKBALLS : nbTrk }));
			}
			if (nbTpad > 0) {
				devicesPaired += ", " + (nbTpad == 1 ?
					self.l10n.format(self.l10n.ADVANCED_TOUCHPAD,  { NB_TOUCHPADS : nbTpad }) :
					self.l10n.format(self.l10n.ADVANCED_TOUCHPADS, { NB_TOUCHPADS : nbTpad }));
			}
			if (nbTab > 0) {
				devicesPaired += ", " + (nbTab == 1 ?
					self.l10n.format(self.l10n.ADVANCED_KEYBOARD,  { NB_TABLETS : nbTab }) :
					self.l10n.format(self.l10n.ADVANCED_KEYBOARDS, { NB_TABLETS : nbTab }));
			}
			if (nbGpad > 0) {
				devicesPaired += ", " + (nbGpad == 1 ?
					self.l10n.format(self.l10n.ADVANCED_GAMEPAD,  { NB_GAMEPADS : nbGpad }) :
					self.l10n.format(self.l10n.ADVANCED_GAMEPADS, { NB_GAMEPADS : nbGpad }));
			}
			if (nbJoy > 0) {
				devicesPaired += ", " + (nbJoy == 1 ?
					self.l10n.format(self.l10n.ADVANCED_JOYSTICK,  { NB_JOYSTICKS : nbJoy }) :
					self.l10n.format(self.l10n.ADVANCED_JOYSTICKS, { NB_JOYSTICKS : nbJoy }));
			}
			devicesPaired = devicesPaired.replace(/^,\s/,'');

		} else {
			devicesPaired = self.l10n.ADVANCED_NO_DEVICES;
		}

		device.devicesPaired = devicesPaired;
	}

	// Device Removal.
	change.remove.forEach(function(device) {
		if (!device.isHidpp) {
			return;
		}

		// Receiver.
		if (device.index != devices.HIDPP_DEVICE_INDEX.RECEIVER) {
			storeRemovedDevice(device);

		// Wireless device.
		} else {
			device.hidppChildren.forEach(function (child) {
				storeRemovedDevice(child);
			});
		}

		self.raiseEvent('deviceRemoved', device);

		self.updateController();
	});

	// Device Arrival.
	change.add.forEach(function(device) {
		if (!device.isHidpp) {
			return;
		}

		// Receiver.
		if (device.index == devices.HIDPP_DEVICE_INDEX.RECEIVER) {
			// Clear removed wireless device cache.
			self.removedDevices.clear();

		// Wireless device.
		} else {
			// Unpair ghost devices if any.
			unpairGhostDevices(device);
		}

		// Serial Number (non-volatile).
		device.registerProperty('serialNumber', function() {
			if (device.parent) {
				device.parent.waitProperty('protocol', function(protocol) {
					if (protocol.getChildDeviceSerialNumber) {
						protocol.getChildDeviceSerialNumber(device, function(devSN) {
							device['serialNumber'] =  devSN;
						});
					}
				});

			} else if (device.index == devices.HIDPP_DEVICE_INDEX.RECEIVER) {
				device.waitProperty('protocol', function(protocol) {
					var request = new devices.GetTransceiverEQuadInformation();
					protocol.requests.enqueue(request);

					request.waitResponse(function(response) {
						device['serialNumber'] = device.productId.toString(16)
							+ "-" + response.eQuadInfo.baseAddress;
					});
				});
			}
		});

		// Force device serial number retrieval
		// (used for ghost device removal).
		device.serialNumber;

		// Device Name (non-volatile).
		device.registerProperty('name', function() {
			if (device.parent) {
				device.parent.waitProperty('protocol', function(protocol) {
					if (protocol.getChildDeviceName) {
						protocol.getChildDeviceName(device, function(value) {
							var type = devices.RAP_DEVICE_TYPE
								.toString(device['rapDeviceType']);
							device['name'] =  type + ' ' + value;
						});
					}
				});
			}
		});

		// Force device name retrieval
		// (used in removed device cache).
		device.name;

		// Firmware Version.
		device.registerProperty('fwVersion', function() {
			device.waitProperty('protocol', function(protocol) {
				// HID++ 1.0
				if (protocol instanceof devices.HidppRap) {
					var fwVersion = "";
					var request = new devices.GetFirmwareVersionItem(device.index,
						devices.RAP_FW_VERSION_ITEM.FW_VERSION);
					protocol.requests.enqueue(request);

					request.waitResponse(function(response) {
						fwVersion += response.fwVersion;

						request = new devices.GetFirmwareVersionItem(device.index,
							devices.RAP_FW_VERSION_ITEM.FW_BUILD);
						protocol.requests.enqueue(request);

						request.waitResponse(function(response) {
							fwVersion += "." + response.fwBuild;
							device['fwVersion'] = fwVersion;
						});
					});
				// HID++ >= 2.0
				} else if (protocol instanceof devices.HidppFap) {
					protocol.getFeature(devices.FEATURE_ID.FW_VERSION, function(feature) {
						if (feature) {
							feature.getEntityCount(function(nbEntities) {
								for (var i=0; i<nbEntities; ++i) {
									feature.getFWInfo(i, function(fwInfo) {
										if (fwInfo.fwType == devices.FAP_FW_TYPE.MAIN_APPLICATION) {
											device['fwVersion'] = fwInfo.version;
										}
									});
								}
							});
						}
					});
				}
			});
		});

		// Battery Level.
		if (device.index != devices.HIDPP_DEVICE_INDEX.RECEIVER) {
			device.registerProperty('battery', function() {
				updateBatteryLevel(device);
			});
		}

		device.registerProperty('devicesPaired');
		device.registerProperty('isFull');

		device.observePropertyChange(onDevicePropertyChanged.bind(device));
		device.children
			.observeCollectionChanged(onDeviceChildrenChanged.bind(device));

		self.raiseEvent('deviceAdded', device);
	});

	self.receivers = self.deviceProvider.devices.filter(function(device) {
		return device.isHidpp && device.index == devices.HIDPP_DEVICE_INDEX.RECEIVER;
	});

	if (self.state != null) {
		switch (self.receivers.length) {
			case 0:
				self.state.onNoReceiver();
				break;

			case 1:
				self.state.onOneReceiver(self.receivers[0]);
				break;

			default:
				self.state.onTooManyReceivers(self.receivers);
				break;
		}
	}
}

/* UnifyingState **************************************/

////////////////////////////////////////////////////////
// UnifyingState

// Parameters:
// - Unifying unifying
my.UnifyingState = function(unifying) {
	core.State.call(this, unifying);
	this.canGoHome        = true;
	this.canGoAdvanced    = false;
	this.canGoPrevious    = false;
	this.canGoNext        = false;
	this.NextButtonText   = unifying.l10n.NEXT;
	this.FinishButtonText = unifying.l10n.FINISH;
	this.canFinish        = false;
	this.confirmExit      = false;

	var self = this;
	Object.defineProperty(this, 'unifying', { get: function() {
		return self.stateMachine;
	}});
}

my.UnifyingState.prototype = core.inherit(core.State.prototype);

my.UnifyingState.prototype.home = function() {
	this.unifying.setState(this.unifying.welcome);
}

my.UnifyingState.prototype.advanced = function() {
	this.unifying.setState(this.unifying.advanced);
}

my.UnifyingState.prototype.previous = function() {}

my.UnifyingState.prototype.next = function() {
	switch (this.unifying.receivers.length) {
		case 0: {
				this.unifying.setState(this.unifying.plug_receiver);
			}
			break;

		case 1: {
				if (this.unifying.removedDevices.length > 1) {
					this.unifying.setState(this.unifying.move_devices);
				} else {
					this.unifying.setState(this.unifying.restart);
				}
			}
			break;

		default: {
				var nbPairedDevices = 0;
				this.unifying.receivers.forEach(function(receiver) {
					var hidppChildren = receiver.children.filter(function(dev) {
						return dev.isHidpp;
					})
					nbPairedDevices += hidppChildren.length;
				});

				if (this.unifying.state === this.unifying.too_many_devices
						|| nbPairedDevices <= 6) {
					this.unifying.setState(this.unifying.unplug_receiver);
				} else {
					this.unifying.setState(this.unifying.too_many_devices,
						nbPairedDevices);
				}
			}
			break;
	}
}

my.UnifyingState.prototype.finish = function() {
	if (this.confirmExit) {
		this.unifying.confirm_exit();
	} else {
		this.unifying.exit();
	}
}

my.UnifyingState.prototype.onNoReceiver = function() {}

my.UnifyingState.prototype.onOneReceiver = function() {}

my.UnifyingState.prototype.onTooManyReceivers = function() {}

////////////////////////////////////////////////////////
// RequestPermissionsState

my.RequestPermissionsState = function(unifying) {
	my.UnifyingState.call(this, unifying);
	this.canGoHome = false;
}

my.RequestPermissionsState.prototype = core.inherit(my.UnifyingState.prototype);

my.RequestPermissionsState.prototype.toString = function() {
	return "request_permissions";
}

my.RequestPermissionsState.prototype.stateEntry = function() {
	var self = this;

	core.Permissions.isAuthorized(function(success) {
		if (success) {
			core.Log.info("App was already granted permissions.");
			self.unifying.setState(self.unifying.welcome);
		}
	});
}

my.RequestPermissionsState.prototype.requestPermissions = function() {
	var self = this;

	core.Permissions.request(function(success) {
		if (success) {
			core.Log.info("App was granted permissions.");
			self.unifying.updateController(function() {
				self.unifying.setState(self.unifying.welcome);
			});
		} else {
			core.Log.warning("App was denied permissions.");
		}
	});
}

////////////////////////////////////////////////////////
// WelcomeState

my.WelcomeState = function(unifying) {
	my.UnifyingState.call(this, unifying);
	this.canGoHome       = false;
	this.canGoAdvanced   = true;
	this.canGoNext       = true;
	this.firstStateEntry = true;
}

my.WelcomeState.prototype = core.inherit(my.UnifyingState.prototype);

// Object
my.WelcomeState.prototype.toString = function() {
	return "welcome";
}

my.WelcomeState.prototype.stateEntry = function() {
	if (this.firstStateEntry) {
		this.firstStateEntry = false;

		var self = this;
		core.Permissions.getDevicePermissions(function(devicePermissions) {
			var options = { devicePermissions : devicePermissions};
			self.unifying.deviceProvider.start(options.devicePermissions.hid.devices);
		});
	}
}

////////////////////////////////////////////////////////
// AdvancedState

my.AdvancedState = function(unifying) {
	my.UnifyingState.call(this, unifying);
	this.canGoHome               = true;
	this.currentDevice           = null;
	this.onDeviceAddedHandle     = this.onDeviceAdded.bind(this);
	this.onDeviceRemovedHandle   = this.onDeviceRemoved.bind(this);
	this.onPropertyChangedHandle = this.onPropertyChanged.bind(this);
}

my.AdvancedState.prototype = core.inherit(my.UnifyingState.prototype);

// Object
my.AdvancedState.prototype.toString = function() {
	return "advanced";
}

my.AdvancedState.prototype.stateEntry = function() {
	this.currentDevice = null;
	this.unifying.removedDevices.clear();
	this.unifying.observeEvent('deviceAdded',   this.onDeviceAddedHandle);
	this.unifying.observeEvent('deviceRemoved', this.onDeviceRemovedHandle);
}

my.AdvancedState.prototype.stateExit = function() {
	this.unifying.removedDevices.clear();
	this.unifying.unobserveEvent('deviceAdded',   this.onDeviceAddedHandle);
	this.unifying.unobserveEvent('deviceRemoved', this.onDeviceRemovedHandle);
}

my.AdvancedState.prototype.pairNewDevice = function() {
	var self = this;
	var currentDevice = self.currentDevice;
	if (currentDevice) {
		currentDevice.observePropertyChange(self.onPropertyChangedHandle);
		currentDevice.openOrCloseLock(60, function() {
			self.pairing = true;
			self.unifying.updateController(function() {
				self.canGoHome = false;
			});
		});
	}
}

my.AdvancedState.prototype.cancelPairing = function() {
	var self = this;
	var currentDevice = self.currentDevice;
	if (currentDevice) {
		currentDevice.unobservePropertyChange(self.onPropertyChangedHandle);
		currentDevice.openOrCloseLock(0, function() {
			self.pairing = false;
			self.unifying.updateController(function() {
				self.canGoHome = true;
			});
		});
	}
}

my.AdvancedState.prototype.selectDevice = function(device) {
	this.currentDevice = device;
}

my.AdvancedState.prototype.onDeviceAdded = function(device) {
	var self = this;
	if (device.isHidpp) {
		self.unifying.updateController(function() {
			self.currentDevice = device;
		});
	}
}

my.AdvancedState.prototype.onDeviceRemoved = function(device) {
	var self = this;
	if (device.isHidpp) {
		self.unifying.updateController(function() {
			self.currentDevice = null;
		});
	}
}

my.AdvancedState.prototype.onPropertyChanged = function(name, value) {
	var self = this;
	if (name == 'isLocked') {
		var isLocked = value;
		if (self.pairing && isLocked) {
			self.unifying.updateController(function() {
				self.canGoHome = true;
			});
		} else if (!this.pairing && !isLocked) {
			self.pairing = true;
		}
	}
}

////////////////////////////////////////////////////////
// RestartState

my.RestartState = function(unifying) {
	my.UnifyingState.call(this, unifying);
	this.onDevicesChangedHandle  = this.onDevicesChanged.bind(this);
	this.onPropertyChangedHandle = this.onPropertyChanged.bind(this);
	this.deviceToPair            = null;
	this.pairing                 = false;
}

my.RestartState.prototype = core.inherit(my.UnifyingState.prototype);

// Object
my.RestartState.prototype.toString = function() {
	return "restart";
}

my.RestartState.prototype.stateEntry = function() {
	var receiver = this.unifying.receivers[0];
	core.Contract.assert(receiver != null, "Failed to find receiver.");

	if (receiver.isFull) {
		this.unifying.setState(this.unifying.receiver_full);

	} else {
		if (this.unifying.removedDevices.length > 0) {
			this.deviceToPair = this.unifying.removedDevices[0];
		}

		this.unifying.deviceProvider.devices
			.observeCollectionChanged(this.onDevicesChangedHandle);

		receiver.observePropertyChange(this.onPropertyChangedHandle);
		if (receiver.isLocked) {
			receiver.openOrCloseLock(60);
		}
	}
}

my.RestartState.prototype.stateExit = function(reEnterState) {
	this.deviceToPair = null;

	var receiver = this.unifying.receivers[0];
	if (receiver) {
		receiver.unobservePropertyChange(this.onPropertyChangedHandle);
		if (!reEnterState) {
		  receiver.openOrCloseLock(0);
		}
	}
	
	this.unifying.deviceProvider.devices
		.unobserveCollectionChanged(this.onDevicesChangedHandle);
}

my.RestartState.prototype.onNoReceiver = function() {
	this.unifying.setState(this.unifying.plug_receiver);
}

my.RestartState.prototype.onTooManyReceivers = function() {
	this.unifying.setState(this.unifying.unplug_last_receiver);
}

my.RestartState.prototype.skipDevice = function() {
	this.pairing = false;
	this.unifying.removedDevices.remove(this.deviceToPair);

	if (this.unifying.removedDevices.length > 0) {
		this.unifying.setState(this.unifying.restart);
	} else {
		this.unifying.setState(this.unifying.paired_devices);
	}
}

my.RestartState.prototype.onDevicesChanged = function(change) {
	var self = this;

	var receiver = this.unifying.receivers[0];
	if (receiver) {
		self.pairing = !receiver.isLocked;
	}

	if (!self.pairing) {
		return;
	}

	change.add.some(function(device) {

		// Look for a wireless device.
		if (device.isHidpp && device.index != devices.HIDPP_DEVICE_INDEX.RECEIVER
			&& device.parent === receiver) {
			self.pairing = false;

			device.waitProperty('serialNumber', function(serialNumber) {
				// Look for a matching device in list.
				var removedDevices = self.unifying.removedDevices.removeWhere(
					function(dev) { return dev.serialNumber == serialNumber; }
				);

				// We are looking for a specific device.
				if (self.deviceToPair) {
					// Device found in list (can be another device than 'deviceToPair').
					if (removedDevices.length > 0) {
						self.unifying.setState(self.unifying.device_paired, device, true);

					// Device NOT found, cause we paired un-expected device.
					} else {
						core.Log.warning("Wrong device paired.. unpairing it!");

						var unpairDevice = function() {
							core.Log.info("Will unpair device");

							// Let's unpair it.
							device.unpair(function() {
								self.unifying.removedDevices.remove(device);

								core.Log.info("Set pairing failed state");
								// Pairing specific device failed.
								self.unifying.setState(self.unifying.pairing_failed,
									self.deviceToPair);

							});
						};

						setTimeout(unpairDevice, 500);
					}
			
				// We are NOT looking for a particular device -> need user confirmation.
				} else {
					self.unifying.setState(self.unifying.confirm_pairing, device);
				}
			});

			return true;
		}

		return false;
	});
}

my.RestartState.prototype.onPropertyChanged = function(name, value) {
	if (name == 'isLocked') {
		var self = this;
		var isLocked = value;
		if (self.pairing && isLocked) {
			// No device has been added (timeout or error).
			self.unifying.setState(self.unifying.pairing_failed,
				self.deviceToPair);
		} else if (!self.pairing && !isLocked) {
			self.pairing = true;
		}
	}
}

////////////////////////////////////////////////////////
// ConfirmPairingState

my.ConfirmPairingState = function(unifying) {
	my.UnifyingState.call(this, unifying);
	this.device = null;
}

my.ConfirmPairingState.prototype = core.inherit(my.UnifyingState.prototype);

// Object
my.ConfirmPairingState.prototype.toString = function() {
	return "confirm_pairing";
}

my.ConfirmPairingState.prototype.stateEntry = function(device) {
	this.device = device;
}

my.ConfirmPairingState.prototype.stateExit = function() {
	this.device = null;
}

my.ConfirmPairingState.prototype.yes = function() {
	this.unifying.setState(this.unifying.device_paired, this.device);
}

my.ConfirmPairingState.prototype.no = function() {
	var self = this;
	self.device.unpair(function() {
		// We need to wait a little so receiver.isFull gets updated
		var nextState = function() {
			self.unifying.removedDevices.remove(self.device);
			my.UnifyingState.prototype.next.call(self);
		}

		setTimeout(nextState, 500);
	});
}

my.ConfirmPairingState.prototype.onNoReceiver = function() {
	this.unifying.setState(this.unifying.plug_receiver);
}

my.ConfirmPairingState.prototype.onTooManyReceivers = function() {
	this.unifying.setState(this.unifying.unplug_last_receiver);
}

////////////////////////////////////////////////////////
// DevicePairedState

my.DevicePairedState = function(unifying) {
	my.UnifyingState.call(this, unifying);
	this.canGoHome  = false;
	this.canGoNext  = true;
	this.device     = null;
	this.isSpecific = false;
}

my.DevicePairedState.prototype = core.inherit(my.UnifyingState.prototype);

// Object
my.DevicePairedState.prototype.toString = function() {
	return "device_paired";
}

my.DevicePairedState.prototype.stateEntry = function(device, isSpecific) {
	this.device     = device;
	this.isSpecific = isSpecific;

	if (this.isSpecific) {
		this.canGoHome      = false;
		this.canFinish      = false;
		this.NextButtonText = this.unifying.l10n.NEXT;

	} else {
		this.canGoHome      = true;
		this.canFinish      = this.unifying.canExit;
		this.NextButtonText = this.unifying.l10n.PAIR_DEVICE;
	}
}

my.DevicePairedState.prototype.stateExit = function() {
	this.device     = null;
	this.isSpecific = false;
}

my.DevicePairedState.prototype.next = function() {
	if (this.isSpecific) {
		// There are remaining devices to be moved.
		if (this.unifying.removedDevices.length > 0) {
			this.unifying.setState(this.unifying.restart);

		// All devices have been moved.
		} else {
			this.unifying.setState(this.unifying.paired_devices);
		}
	} else {
		my.UnifyingState.prototype.next.call(this);
	}
}

my.DevicePairedState.prototype.onNoReceiver = function() {
	this.unifying.setState(this.unifying.plug_receiver);
}

my.DevicePairedState.prototype.onTooManyReceivers = function() {
	this.unifying.setState(this.unifying.unplug_receiver);
}

////////////////////////////////////////////////////////
// PairingFailedState

my.PairingFailedState = function(unifying) {
	my.UnifyingState.call(this, unifying);
	this.canFinish        = this.unifying.canExit;
	this.confirmExit      = true;
	this.FinishButtonText = this.unifying.l10n.CLOSE;
	this.canGoNext        = true;
	this.NextButtonText   = this.unifying.l10n.RETRY;
	this.deviceToPair     = null;
}

my.PairingFailedState.prototype = core.inherit(my.UnifyingState.prototype);

// Object
my.PairingFailedState.prototype.toString = function() {
	return "pairing_failed";
}

my.PairingFailedState.prototype.stateEntry = function(deviceToPair) {
	this.deviceToPair = deviceToPair;
}

my.PairingFailedState.prototype.stateExit = function() {
	this.deviceToPair = null;
}

my.PairingFailedState.prototype.onNoReceiver = function() {
	this.unifying.setState(this.unifying.plug_receiver);
}

my.PairingFailedState.prototype.onTooManyReceivers = function() {
	this.unifying.setState(this.unifying.unplug_last_receiver);
}

my.PairingFailedState.prototype.skipDevice = function() {
	this.unifying.removedDevices.remove(this.deviceToPair);

	if (this.unifying.removedDevices.length > 0) {
		this.unifying.setState(this.unifying.restart);
	} else {
		this.unifying.setState(this.unifying.paired_devices);
	}
}

////////////////////////////////////////////////////////
// PairedDevicesState

my.PairedDevicesState = function(unifying) {
	my.UnifyingState.call(this, unifying);
	this.canFinish      = this.unifying.canExit;
	this.canGoNext      = true;
	this.NextButtonText = this.unifying.l10n.PAIR_DEVICE;
	this.freeSlots      = 0;
}

my.PairedDevicesState.prototype = core.inherit(my.UnifyingState.prototype);

// Object
my.PairedDevicesState.prototype.toString = function() {
	return "paired_devices";
}

my.PairedDevicesState.prototype.stateEntry = function() {
	var receiver = this.unifying.receivers[0];
	var hidppChildren = receiver.children.filter(function(dev) {
		return dev.isHidpp;
	})

	this.freeSlots = 6 - hidppChildren.length;
}

my.PairedDevicesState.prototype.onNoReceiver = function() {
	this.unifying.setState(this.unifying.plug_receiver);
}

my.PairedDevicesState.prototype.onTooManyReceivers = function() {
	this.unifying.setState(this.unifying.unplug_receiver);
}

////////////////////////////////////////////////////////
// PlugReceiverState

my.PlugReceiverState = function(unifying) {
	my.UnifyingState.call(this, unifying);
	this.canFinish        = this.unifying.canExit;
	this.confirmExit      = true;
	this.FinishButtonText = this.unifying.l10n.CLOSE;
}

my.PlugReceiverState.prototype = core.inherit(my.UnifyingState.prototype);

// Object
my.PlugReceiverState.prototype.toString = function() {
	return "plug_receiver";
}

my.PlugReceiverState.prototype.stateEntry = function() {
	this.canGoNext = false;
}

my.PlugReceiverState.prototype.onNoReceiver = function() {
	this.canGoNext = false;
}

my.PlugReceiverState.prototype.onOneReceiver = function() {
	this.canGoNext = true;
}

my.PlugReceiverState.prototype.onTooManyReceivers = function() {
	this.unifying.setState(this.unifying.unplug_receiver);
}

////////////////////////////////////////////////////////
// UnplugReceiverState

my.UnplugReceiverState = function(unifying) {
	my.UnifyingState.call(this, unifying);
	this.canFinish        = this.unifying.canExit;
	this.confirmExit      = true;
	this.FinishButtonText = this.unifying.l10n.CLOSE;
}

my.UnplugReceiverState.prototype = core.inherit(my.UnifyingState.prototype);

// Object
my.UnplugReceiverState.prototype.toString = function() {
	return "unplug_receiver";
}

my.UnplugReceiverState.prototype.stateEntry = function(receiver) {
	this.canGoNext = false;
}

my.UnplugReceiverState.prototype.onNoReceiver = function() {
	this.unifying.setState(this.unifying.plug_receiver);
}

my.UnplugReceiverState.prototype.onOneReceiver = function() {
	this.canGoNext = true;
}

my.UnplugReceiverState.prototype.onTooManyReceivers = function() {
	this.canGoNext = false;
}

////////////////////////////////////////////////////////
// UnplugLastReceiverState

my.UnplugLastReceiverState = function(unifying) {
	my.UnifyingState.call(this, unifying);
	this.canFinish        = this.unifying.canExit;
	this.confirmExit      = true;
	this.FinishButtonText = this.unifying.l10n.CLOSE;
}

my.UnplugLastReceiverState.prototype = core.inherit(my.UnifyingState.prototype);

// Object
my.UnplugLastReceiverState.prototype.toString = function() {
	return "unplug_last_receiver";
}

my.UnplugLastReceiverState.prototype.stateEntry = function(receiver) {
	this.canGoNext = false;
}

my.UnplugLastReceiverState.prototype.onNoReceiver = function() {
	this.unifying.setState(this.unifying.plug_receiver);
}

my.UnplugLastReceiverState.prototype.onOneReceiver = function() {
	this.canGoNext = true;
}

my.UnplugLastReceiverState.prototype.onTooManyReceivers = function() {
	this.canGoNext = false;
}

////////////////////////////////////////////////////////
// ReceiverFullState

my.ReceiverFullState = function(unifying) {
	my.UnifyingState.call(this, unifying);
	this.canFinish        = this.unifying.canExit;
	this.confirmExit      = true;
	this.FinishButtonText = this.unifying.l10n.CLOSE;
	this.selectedDevice   = null;
}

my.ReceiverFullState.prototype = core.inherit(my.UnifyingState.prototype);

// Object
my.ReceiverFullState.prototype.toString = function() {
	return "receiver_full";
}

my.ReceiverFullState.prototype.stateEntry = function() {
	this.selectedDevice = null;
	this.canGoNext      = false;
}

my.ReceiverFullState.prototype.next = function() {
	var self = this;
	core.Contract.assert(self.selectedDevice != null, "Device not found");
	if (self.selectedDevice) {
		self.selectedDevice.unpair(function() {
			// We need to wait a little so receiver.isFull gets updated
			var restartState = function() {
				self.unifying.removedDevices.remove(self.selectedDevice);
				self.unifying.setState(self.unifying.restart);
			}

			setTimeout(restartState, 500);
		});
	}
}

my.ReceiverFullState.prototype.onNoReceiver = function() {
	this.unifying.setState(this.unifying.plug_receiver);
}

my.ReceiverFullState.prototype.onTooManyReceivers = function() {
	this.unifying.setState(this.unifying.unplug_last_receiver);
}

my.ReceiverFullState.prototype.onSelectDevice = function(device) {
	if (device && device.isHidpp) {
		this.selectedDevice = device;
		this.canGoNext      = true;
	} else {
		this.selectedDevice = null;
		this.canGoNext      = false;
	}
}

////////////////////////////////////////////////////////
// TooManyDevicesState

my.TooManyDevicesState = function(unifying) {
	my.UnifyingState.call(this, unifying);
	this.canGoNext       = true;
	this.nbPairedDevices = 0;
}

my.TooManyDevicesState.prototype = core.inherit(my.UnifyingState.prototype);

// Object
my.TooManyDevicesState.prototype.toString = function() {
	return "too_many_devices";
}

my.TooManyDevicesState.prototype.stateEntry = function(nbPairedDevices) {
	this.nbPairedDevices = nbPairedDevices;
}

////////////////////////////////////////////////////////
// MoveDevicesState

my.MoveDevicesState = function(unifying) {
	my.UnifyingState.call(this, unifying);
	this.canGoPrevious = true;
	this.canGoNext     = true;
}

my.MoveDevicesState.prototype = core.inherit(my.UnifyingState.prototype);

// Object
my.MoveDevicesState.prototype.toString = function() {
	return "move_devices";
}

my.MoveDevicesState.prototype.previous = function() {
	this.unifying.setState(this.unifying.wrong_receiver);
}

my.MoveDevicesState.prototype.next = function() {
	this.unifying.setState(this.unifying.restart);
}

my.MoveDevicesState.prototype.onNoReceiver = function() {
	this.unifying.setState(this.unifying.plug_receiver);
}

my.MoveDevicesState.prototype.onTooManyReceivers = function() {
	this.unifying.setState(this.unifying.unplug_last_receiver);
}

////////////////////////////////////////////////////////
// WrongReceiverState

my.WrongReceiverState = function(unifying) {
	my.UnifyingState.call(this, unifying);
	this.canGoNext = true;
}

my.WrongReceiverState.prototype = core.inherit(my.UnifyingState.prototype);

// Object
my.WrongReceiverState.prototype.toString = function() {
	return "wrong_receiver";
}

my.WrongReceiverState.prototype.onNoReceiver = function() {
	this.canGoNext = false;
}

my.WrongReceiverState.prototype.onOneReceiver = function() {
	this.canGoNext = true;
}

my.WrongReceiverState.prototype.onTooManyReceivers = function() {
	this.canGoNext = false;
}

return my;

}(unifying || {}));
