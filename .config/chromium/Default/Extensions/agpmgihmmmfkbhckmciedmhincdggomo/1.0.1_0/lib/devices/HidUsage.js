'use strict';

var devices = (function (my) {

////////////////////////////////////////////////////////
// HID_USAGE_PAGE

my.HID_USAGE_PAGE = {
	GENERIC_DESKTOP        : 0x01,
	SIMULATION             : 0x02,
	VIRTUAL_REALITY        : 0x03,
	SPORT                  : 0x04,
	GAME                   : 0x05,
	KEYBOARD               : 0x07,
	LED                    : 0x08,
	BUTTON                 : 0x09,
	ORDINAL                : 0x0A,
	TELEPHONY              : 0x0B,
	CONSUMER               : 0x0C,
	DIGITIZER              : 0x0D,
	PIDPAGE                : 0x0F,
	UNICODE                : 0x10,
	ALPHANUMERIC_DISPLAY   : 0x14,
	MEDICAL_INSTRUMENTS    : 0x40,
	MONITOR0               : 0x80,
	MONITOR1               : 0x81,
	MONITOR2               : 0x82,
	MONITOR3               : 0x83,
	POWER0                 : 0x84,
	POWER1                 : 0x85,
	POWER2                 : 0x86,
	POWER3                 : 0x87,
	BARCODESCANNER         : 0x8C,
	SCALE                  : 0x8D,
	MAGNETIC_STRIP_EREADER : 0x8E,
	RESERVED_POINT_OF_SALE : 0x8F,
	CAMERA_CONTROL         : 0x90,
	ARCADE                 : 0x91,
	HIDPP_OLD              : 0xFF00,
	HIDPP_NEW              : 0xFF43,
	MEDIA_CENTER           : 0xFFBC,

	toString : function(value) {
		var name = this.getOwnPropertyNameForValue(value)
			.replace(/_/g, " ");
		var hexValue = '0x' + value.toString(16).padLeft("0", 4);
		return name.length > 0 ? hexValue + " (" + name + ")" : hexValue;
    }
};

////////////////////////////////////////////////////////
// HID_USAGE_ID

my.HID_USAGE_ID = {

	GENERIC_DESKTOP: {
		POINTER: 0X01,
		MOUSE: 0X02,
		JOYSTICK: 0X04,
		GAMEPAD: 0X05,
		KEYBOARD: 0X06,
		KEYPAD: 0X07,
		MULTI_AXIS_CONTROLLER: 0X08,
		X: 0X30,
		Y: 0X31,
		Z: 0X32,
		RX: 0X33,
		RY: 0X34,
		RZ: 0X35,
		SLIDER: 0X36,
		DIAL: 0X37,
		WHEEL: 0X38,
		HAT_SWITCH: 0X39,
		COUNTED_BUFFER: 0X3A,
		BYTE_COUNT: 0X3B,
		MOTION_WAKEUP: 0X3C,
		START: 0X3D,
		SELECT: 0X3E,
		VX: 0X40,
		VY: 0X41,
		VZ: 0X42,
		VBR_X: 0X43,
		VBR_Y: 0X44,
		VBR_Z: 0X45,
		VNO: 0X46,
		SYSTEM_CONTROL: 0X80,
		SYSTEM_POWER_DOWN: 0X81,
		SYSTEM_SLEEP: 0X82,
		SYSTEM_WAKEUP: 0X83,
		SYSTEM_CONTEXT_MENU: 0X84,
		SYSTEM_MAIN_MENU: 0X85,
		SYSTEM_APP_MENU: 0X86,
		SYSTEM_HELP_MENU: 0X87,
		SYSTEM_MENU_EXIT: 0X88,
		SYSTEM_MENU_SELECT: 0X89,
		SYSTEM_MENU_RIGHT: 0X8A,
		SYSTEM_MENU_LEFT: 0X8B,
		SYSTEM_MENU_UP: 0X8C,
		SYSTEM_MENU_DOWN: 0X8D,
		DPAD_UP: 0X90,
		DPAD_DOWN: 0X91,
		DPAD_RIGHT: 0X92,
		DPAD_LEFT: 0X93
	},

	// TODO(jracle): complete with other pages

	CONSUMER: {
		CONSUMER_CONTROL: 0X01,
		// TODO(jracle): complete with other usages
	},

	// TODO(jracle): complete with other pages

	HID_PLUS_PLUS: {
		SHORT: 0x01,
		LONG: 0x02,
		VERY_LONG: 0x04,
		DJ: 0x04
	},

	DJ: {
		COLLECTION: 0x04,
		SHORT: 0x41,
		LONG: 0x42
	},

	MEDIA_CENTER: {
		// CONTROLS
		DVD_MENU: 0X24,
		TV_JUMP: 0X25,
		MY_TV: 0X46,
		MY_MUSIC: 0X47,
		RECORDED_TV: 0X48,
		MY_PICTURES: 0X49,
		MY_VIDEOS: 0X4A,
		DVD_ANGLE: 0X4B,
		DVD_AUDIO: 0X4C,
		DVD_SUBTITLE: 0X4D,
		OEM_1: 0X80,
		OEM_2: 0X81,
		// COLLECTIONS
		REMOTE_CONTROL: 0X88
	}
}

return my;

}(devices || {}));
