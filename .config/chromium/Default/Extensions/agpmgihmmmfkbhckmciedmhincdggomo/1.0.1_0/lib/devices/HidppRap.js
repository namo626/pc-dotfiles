'use strict';

var devices = (function (my) {

////////////////////////////////////////////////////////
// RAP_ERROR

my.RAP_ERROR = {
	SUCCESS              : 0x00, // No error / undefined
	ERR_INVALID_SUBID    : 0x01, // Invalid sub-identifier / command
	ERR_INVALID_ADDRESS  : 0x02, // Invalid address
	ERR_INVALID_VALUE    : 0x03, // Invalid value
	ERR_CONNECT_FAIL     : 0x04, // Connection request failed (receiver)
	ERR_TOO_MANY_DEVICES : 0x05, // Too many devices connected (receiver)
	ERR_ALREADY_EXISTS   : 0x06, // Already exists (receiver)
	ERR_BUSY             : 0x07, // Busy (receiver)
	ERR_UNKNOWN_DEVICE   : 0x08, // Unknown device (receiver)
	ERR_RESOURCE_ERROR   : 0x09, // Resource error (receiver)
	ERR_INVALID_STATE    : 0x0A, // Request not valid in current state
	ERR_INVALID_PARAM    : 0x0B, // Invalid parameter(s)
	ERR_WRONG_PIN_CODE   : 0x0C,

	toString : function(value) {
		return this.getOwnPropertyNameForValue(value)
			.replace(/_/g, " ").capitalize();
	}
};

////////////////////////////////////////////////////////
// RAP_SUBID

my.RAP_SUBID = {
	// HID++ notifications (0x00 .. 0x7F)
	ALL                         : 0x00, // Reserved
	KEYBOARD                    : 0x01, // Reserved for standard-keyboard keys
	MOUSE                       : 0x02, // Reserved for mouse
	CONSUMER_KEYS               : 0x03, // Consumer control keys
	POWER                       : 0x04, // Power keys
	ROLLER                      : 0x05, // Roller
	MOUSE_EXTRA_BT              : 0x06, // Mouse extra buttons
	BATTERY_STATUS              : 0x07, // Battery status
	UI_EVENT                    : 0x08, // User interface events
	F_LOCK                      : 0x09, // F-Lock status
	CALC_RESULT                 : 0x0A, // Calculator result {long}
	MENU_ENTER                  : 0x0B, // Navigation menu enter
	FN_PLUS_ALPHANUMKEY         : 0x0C, // Fn + alpha-numeric key
	BATTERY_MILEAGE             : 0x0D, // Battery mileage
	UART_RX                     : 0x0E, // UART received string {very-long}
	BACKLIGHT_DURATION          : 0x17, // Backlight duration update
	DEVICE_DISCONNECT           : 0x40, // Device disconnection
	DEVICE_CONNECT              : 0x41, // Device connection
	DEVICE_DISCOVERY            : 0x42, // Device discovery (BT) {long}
	PIN_CODE_REQ                : 0x43, // PIN code request
	RCV_WORKING_MODE            : 0x44, // Receiver working mode (BT)
	NOTIFICATION_ERROR          : 0x45, // Error notification
	RFLINK_CHANGE               : 0x46, // RF link change (27 MHz)
	ENCRYPTION_KEY              : 0x47, // Encryption key (27 MHz, Quad) {long}
	HCI_NOTIFICATION            : 0x48, // HCI notification (BT) {long, very-long}
	LINK_QUALITY_INFO           : 0x49, // Link quality information (eQuad) {short, long}
	QUAD_LOCKING_INFO           : 0x4A, // Quad locking change information (Quad, eQuad)
	WL_DEV_CHANGE_INFO          : 0x4B, // Wireless device change information
	HOT_NOTIFICATION            : 0x50, // HOT/SYNERGY notification
	ACL_NOTIFICATION            : 0x51, // ACL notification {very-long}
	VOIP_TELEPHONY_EVENT        : 0x5B, // VoIP telephony event
	LED_NOTIFICATION            : 0x60, // LED notification
	GEST_NOTIFICATION           : 0x65, // Gesture and air feature notification
	MULTI_TOUCH_NOTIFICATION    : 0x66, // Touchpad multi-touch notification {long}
	TRACEABILITY_NOTIFICATION   : 0x78, // Traceability notification
	// HID++ register access (0x80 .. 0x8F)
	SET_REGISTER                : 0x80, // Set short register (short request/short response)
	GET_REGISTER                : 0x81, // Get short register (short request/short response)
	SET_LONG_REGISTER           : 0x82, // Set long register (long request/short response) {long}
	GET_LONG_REGISTER           : 0x83, // Get long register (short request/long response) {long}
	SET_VERY_LONG_REGISTER      : 0x84, // Set very-long register (very-long request/short response) {very-long}
	GET_VERY_LONG_REGISTER      : 0x85, // Get very-long register (short request/very-long response) {very-long}
	ERROR_MSG_REGISTER          : 0x8F, // Error message
	// HOT (0x90 .. 0xFF)
	TRANS_INIT_ASYN             : 0x90, // First asynchronous payload packet in a HOT stream {long, very-long}
	TRANS_CONT_ASYN             : 0x91, // Next asynchronous payload packet in a HOT stream {long, very-long}
	TRANS_INIT_SYN              : 0x92, // First synchronous payload packet in a HOT stream {long, very-long}
	TRANS_CONT_SYN              : 0x93, // Next synchronous payload packet in a HOT stream {long, very-long}
	RETRY_INIT_ASYN				: 0x94, // First asynchronous payload packet in a HOT stream re-sent {long, very-long}
	RETRY_CONT_ASYN				: 0x95, // Next asynchronous payload packet in a HOT stream re-sent {long, very-long}
	RETRY_INIT_SYN				: 0x96, // First synchronous payload packet in a HOT stream re-sent {long, very-long}
	RETRY_CONT_SYN				: 0x97, // Next synchronous payload packet in a HOT stream re-sent {long, very-long}
	SYNC						: 0xFF
};

////////////////////////////////////////////////////////
// RAP_REGISTER

my.RAP_REGISTER = {
	HIDPP_REPORTING          : 0x00, // Enable HID++ reporting
	FEATURES                 : 0x01, // Enable individual features
	CONNECTION_STATE         : 0x02, // Connection state
	SW_UPDATE_PENDING        : 0x03, // Software update pending
	BATTERY_STATUS           : 0x07, // Battery status
	UI_EVENT                 : 0x08, // User interface event and status {read-only}
	F_LOCK                   : 0x09, // F-Lock status
	BATTERY_MILEAGE          : 0x0D, // Battery mileage {read-only}
	FEATURES_CONT            : 0x0E, // Enable individual features (extension to Reg. 0x01)
	PROFILE_MGT              : 0x0F, // Profile management
	LCD_MODE                 : 0x10, // LCD mode {write-only}
	LCD_ICONS                : 0x11, // LCD notification icons {long} (obsolete, use \ref HIDPP_REG_UI_NOTIF)
	LCD_CMD                  : 0x12, // LCD command {write-only}
	LCD_DESCR                : 0x13, // LCD descriptor {long} (obsolete)
	LCD_FW_VERSION           : 0x14, // LCD firmware version {long} {read-only} (obsolete)
	LCD_CONTRAST             : 0x15, // LCD contrast (obsolete)
	LCD_BACKLIGHT            : 0x17, // LCD backlight
	LCD_DATAL1B0             : 0x20, // LCD content line 1, buffer 0 {long}
	LCD_DATAL1B1             : 0x21, // LCD content line 1, buffer 1 {long}
	LCD_DATAL1B2             : 0x22, // LCD content line 1, buffer 2 {long}
	LCD_DATAL2B0             : 0x23, // LCD content line 2, buffer 0 {long}
	LCD_DATAL2B1             : 0x24, // LCD content line 2, buffer 1 {long}
	LCD_DATAL2B2             : 0x25, // LCD content line 2, buffer 2 {long}
	LCD_DATAL3B0             : 0x26, // LCD content line 3, buffer 0 {long}
	LCD_DATAL3B1             : 0x27, // LCD content line 3, buffer 1 {long}
	LCD_DATAL3B2             : 0x28, // LCD content line 3, buffer 2 {long}
	TIME_FORMAT              : 0x30, // Time and date format
	TIME_VALUE               : 0x31, // Time value
	DATE_VALUE_MDW           : 0x32, // Date value: month, day and week
	DATE_VALUE_Y             : 0x33, // Date value: year
	USER_NAME                : 0x34, // User name {long}
	CALC_FORMAT              : 0x35, // Calculator format
	LOC_WRITE_CONTROL        : 0x40, // Localization write control (status) {read-only}
	LOC_MONTH_NAME_00_0F     : 0x41, // Localized month names, non-volatile memory addresses 0x00 .. 0x0F {long}
	LOC_MONTH_NAME_10_1F     : 0x42, // Localized month names, non-volatile memory addresses 0x10 .. 0x1F {long}
	LOC_MONTH_NAME_20_2F     : 0x43, // Localized month names, non-volatile memory addresses 0x20 .. 0x2F {long}
	LOC_MONTH_NAME_30_3B     : 0x44, // Localized month names, non-volatile memory addresses 0x30 .. 0x3B {long}
	LOC_STRING_CONNECTING    : 0x45, // Localized string "Connecting" {long}
	LOC_STRING_NOT_AVAILABLE : 0x46, // Localized string "Not available" {long}
	LOC_STRING_CONNECTED     : 0x47, // Localized string "Connected" {long}
	LOC_STRING_FAILED        : 0x48, // Localized string "Failed" {long}
	UI_ACTION                : 0x50, // User interface action
	UI_LEDS                  : 0x51, // User interface LEDs
	UI_NOTIF                 : 0x52, // User interface notification icons {short, long}
	TOUCHPAD_SETTINGS        : 0x53, // Touchpad settings
	LED_INTENSITY            : 0x54, // LED intensity
	AIR_FEATURE              : 0x55, // Air feature (3D movement)
	THREEG_ROLLER_CONTROL    : 0x56, // 3G roller control
	LED_RGB_CONTROL          : 0x57, // LED RGB color control
	DAC_CODEC                : 0x59, // Dedicated audio controls: CODEC
	DAC_PARAM                : 0x5A, // Dedicated audio controls: parameters
	VOIP_STATUS              : 0x5C, // VoIP call and audio status
	SENSOR_SETTINGS          : 0x60, // Optical sensor configuration settings
	SENSOR_RAW_DATA          : 0x61, // Optical sensor raw data
	SENSOR_PROCESS_DATA      : 0x62, // Optical sensor processed data {read-only}
	SENSOR_RESOLUTION        : 0x63, // Optical sensor resolution and frame rate {short, long}
	TIME_BETWEEN_REPORT      : 0x64, // Time between report
	SENSOR_RES_TOGGLE        : 0x65, // Optical sensor resolution toggle
	GESTURE_CONTROL          : 0x68, // Gesture control
	BIRKIN_CONTROL           : 0x69, // Birkin control {long} {read-only}
	RX0_RADIO_LINK_CONTROL   : 0x70, // Receiver 0 radio link control
	RX1_RADIO_LINK_CONTROL   : 0x71, // Receiver 1 radio link control
	TX_RADIO_LINK_CONTROL    : 0x72, // Transmitter radio link control
	RECEIVER_SETTINGS        : 0x73, // Receiver scan settings
	TRACEABILITY_ACCESS      : 0x78, // Traceability access {short, long} {read-only}
	RECEIVER_MODE            : 0x80, // Receiver mode
	DISCOVER_DEVICE          : 0x81, // Device discovery
	CONNECT_DEVICE           : 0x82, // Device connection & deconnection
	CURRENT_DEVICE           : 0x83, // Current active device {long}
	LEDS_ALL_ON              : 0x84, // LED blinking pattern for connection {write-only}
	VIRTUAL_CABLE_KBD        : 0x85, // Virtual cable information for keyboard {long}
	VIRTUAL_CABLE_MSE        : 0x86, // Virtual cable information for mouse {long}
	VIRTUAL_CABLE_NUM        : 0x87, // Virtual cable information for numeric pad {long}
	PIN_CODE                 : 0x88, // Current device (see \ref HIDPP_REG_CURRENT_DEVICE) PIN code (BT) {long}
	DEVICE_CONNECTIVITY      : 0x89, // Device connectivity control {write-only}
	RFID_PAIRING_INFO        : 0x8A, // RFID pairing information (BT) {long} {read-only}
	DEVICE_DESCR             : 0x90, // Current device (see \ref HIDPP_REG_CURRENT_DEVICE) SDP information (BT) {long} {read-only}
	DEVICE_NAME_1            : 0x91, // Current device (see \ref HIDPP_REG_CURRENT_DEVICE) name part 1 (bytes 1 to 16) (BT) {long} {read-only}
	DEVICE_NAME_2            : 0x92, // Current device (see \ref HIDPP_REG_CURRENT_DEVICE) name part 2 (bytes 17 to 32) (BT) {long} {read-only}
	SIGNAL_QUALITY           : 0x93, // Signal quality (BT) {read-only}
	BQB                      : 0x99, // Current device (see \ref HIDPP_REG_CURRENT_DEVICE) BQB test command (BT) {short, long}
	MEMORY_MGT               : 0xA0, // Memory management {long} {write-only}
	HOT_SYNERGY              : 0xA1, // HOT/SYNERGY command
	READ_SECTOR              : 0xA2, // Read sector {long} {read-only}
	LINK_QUALITY_INFO        : 0xB0, // Link quality information (Quad) {read-only}
	FORCE_QUAD_CHANNEL       : 0xB1, // Force Quad channel / disable agility (Quad)
	QUAD_CONNECT_DEVICE      : 0xB2, // Quad device connection and disconnection (Quad, eQuad) {write-only}
	DEVICE_ACTIVITY          : 0xB3, // Device activity (eQuad) {long} {read-only}
	NV_PAIRING_INFO          : 0xB5, // Non-volatile and pairing information (Quad, eQuad) {long}
	TESTMODE_CONTROL         : 0xD0, // Test mode control
	RF_REGISTER              : 0xD1, // RF register access {short, long}
	RAM_SINGLE               : 0xD2, // RAM access (single-byte)
	RAM_MULTI                : 0xD3, // RAM access (multi-bytes) {long}
	EEPROM_SINGLE            : 0xD4, // Non-volatile memory (EEPROM) access (single-byte)
	EEPROM_MULTI             : 0xD5, // Non-volatile memory (EEPROM) access (multi-bytes) {long}
	EEPROM_SECURED           : 0xD6, // Non-volatile memory (EEPROM) secured access
	EEPROM_OPERATION         : 0xD7, // Non-volatile memory (EEPROM) operation {write-only}
	EEPROM_CHECKSUM          : 0xD8, // Non-volatile memory (EEPROM) checksum {read-only}
	UI                       : 0xD9, // User interface
	SPI                      : 0xDA, // SPI access
	MOUSE_SENSOR             : 0xDB, // Optical mouse sensor {long} {read-only}
	PAIRING_INFORMATION      : 0xDC, // Device pairing information in non-volatile memory and RAM
	POWER_MODE               : 0xDD, // Power mode {write-only}
	SPI_CS                   : 0xDE, // SPI chip select
	MANUFACTURING_PARAM      : 0xDF, // Manufacturing parameters (device-dependent) {read-only}
	SET_HCI                  : 0xE1, // Send an HCI request {long, very-long} {write-only}
	SET_ACL                  : 0xE2, // Send an ACL request {long, very-long} {write-only}
	DFU_LITE_DATA            : 0xE2, // DFU Lite data {long}
	RFID                     : 0xE3, // RFID access {long}
	HW_STATE                 : 0xE4, // Hardware state {read-only}
	UART_TX                  : 0xE5, // UART transmitted string {very-long} {write-only}
	ENCRYPT_ERR_COUNTER      : 0xE6, // Encryption rejected-frame counter (eQuad, debug) {read-only}
	USB_ICP                  : 0xF0, // Enter USB ICP/DFU/OTA DFU programming mode
	FW_VERSION               : 0xF1, // Firmware version {read-only}
	RESET                    : 0xF2, // Reset {write-only}
	DEBUG_MODE               : 0xF3, // Debug mode (eQuad)
	FW_DEBUG                 : 0xFD  // General-purpose firmware debug (device-dependent) {short, long}
};

////////////////////////////////////////////////////////
// RAP_DEVICE_TYPE

my.RAP_DEVICE_TYPE = {
	UNKNOWN        : 0x00,	// Undefined
	KEYBOARD       : 0x01,	// Keyboard
	MOUSE          : 0x02,	// Mouse
	NUMPAD         : 0x03,	// Numeric pad
	PRESENTER      : 0x04,	// Presenter
	HEADSET        : 0x05,	// Headset (Trifecta, Reg. HIDPP_REG_CURRENT_DEVICE)
	SPEAKER_PHONE  : 0x06,	// Speaker phone (2Face, Reg. HIDPP_REG_CURRENT_DEVICE)
	REMOTE_CONTROL : 0x07,	// Remote control
	TRACKBALL      : 0x08,	// Trackball
	TOUCHPAD       : 0x09,	// Touchpad (relative displacement)
	TABLET         : 0x0A,	// Tablet (absolute coordinates)
	GAMEPAD        : 0x0B,	// Gamepad
	JOYSTICK       : 0x0C,	// Joystick

	toString : function(value) {
		return this.getOwnPropertyNameForValue(value)
			.replace(/_/g, " ").capitalize();
	}
};

////////////////////////////////////////////////////////
// RAP_PROTOCOL_TYPE

my.RAP_PROTOCOL_TYPE = {
	BLUETOOTH           : 0x01, // Bluetooth protocol
	TWENTY_SEVEN_MHZ    : 0x02, // 27 MHz protocol
	QUAD                : 0x03, // Quad or eQuad step 1 .. 3  protocol
	EQUAD_DJ            : 0x04, // eQuad step 4 "DJ" protocol
	DFU_LITE            : 0x05, // DFU Lite protocol
	EQUAD_LITE          : 0x06, // eQuad step 4 Lite protocol
	EQUAD_HIGH_RPT_RATE : 0x07, // eQuad step 4 gaming protocol (high report-rate gaming mice/keyboard)
	EQUAD_GAMEPAD       : 0x08, // eQuad step 4 protocol for gamepads

	toString : function(value) {
		return this.getOwnPropertyNameForValue(value)
			.replace(/_/g, " ").capitalize();
	}
};

////////////////////////////////////////////////////////
// RAP_DISCONNECT_TYPE

my.RAP_DISCONNECT_TYPE = {
	DEVICE_DISCONNECT : 0x00, // Link disconnection (BT)
	LINK_LOSS         : 0x01, // Link loss (BT)
	DEVICE_UNPLUG     : 0x02, // Permanent disconnection

	toString : function(value) {
		return this.getOwnPropertyNameForValue(value)
			.replace(/_/g, " ").capitalize();
	}
};

////////////////////////////////////////////////////////
// RAP_CONNECT_MASK

my.RAP_CONNECT_MASK = {
	SW_PRESENT : 4,
	ENCRYPTED : 5,
	NO_LINK : 6,
	LOGITECH : 7,
	PAYLOAD : 7,

	TYPE_MASK : 0x0F,

	SW_PRESENT_MASK : (1 << 4),
	ENCRYPTED_MASK : (1 << 5),
	NO_LINK_MASK : (1 << 6),
	LOGITECH_MASK : (1 << 7),
	PAYLOAD_MASK : (1 << 7),

	SECURED : 7, // Secured/encrypted link (0 : non-secured link, 1 : secured link) (27 MHz)

	RX_LINK_MASK : 0x03, // Receiver link (1 ..3) or \ref RAP_CONNECT_MASK.NO_RXTX_LINK (27 MHz)
	TX_LINK_MASK : 0x18, // Transmitter link (1 ..3) or \ref RAP_CONNECT_MASK.NO_RXTX_LINK (27 MHz)
	SECURED_MASK : (1 << 7),
	NO_RXTX_LINK : 0 // No receiver/transmitter link (27 MHz)
};

////////////////////////////////////////////////////////
// RAP_QUAD_CONNECT_DEVICES

my.RAP_QUAD_CONNECT_DEVICES = {
	OPEN_LOCK       : 0x01, // Quad open lock (receiver)
	CLOSE_LOCK      : 0x02, // Quad close lock (receiver)
	DISCONNECT      : 0x03, // Quad disconnect/unplug (device)
	DISCONNECT_LINK : 0x05, // Quad disconnect link (device)
	OPEN_LOCK_OTP   : 0x06  // Quad open lock (receiver) allowing last OTP pairing slots to be used
}

////////////////////////////////////////////////////////
// RAP_QUAD_LOCKING_INFO_ERROR

my.RAP_QUAD_LOCKING_INFO_ERROR = {
	NO_ERR          : 0x00, // No error
	TIMEOUT         : 0x01, // Timeout
	UNSUPP_DEV      : 0x02, // Unsupported device
	TOO_MANY_DEV    : 0x03, // Too many devices
	RCV_OTP_MEM     : 0x04, // Receiver out of OTP memory
	DVC_OTP_MEM     : 0x05, // Device out of OTP memory
	CONNECT_TIMEOUT : 0x06, // Connection sequence timeout

	toString : function(value) {
		return this.getOwnPropertyNameForValue(value)
			.replace(/_/g, " ").capitalize();
	}
};

////////////////////////////////////////////////////////
// RAP_QUAD_LOCKING_INFO_ERROR

my.RAP_FW_VERSION_ITEM = {
	FW_VERSION : 0x01, // Firmware name/number & version
	FW_BUILD   : 0x02, // Firmware build number
	HW_VERSION : 0x03, // Hardware version
	BL_VERSION : 0x04, // Bootloader version & build number

	toString : function(value) {
		return this.getOwnPropertyNameForValue(value)
			.replace(/_/g, " ").capitalize();
	}
};

////////////////////////////////////////////////////////
// HidppRapRequest

// Arguments : deviceIndex, subId, register, payload (implicit)
my.HidppRapRequest = function(deviceIndex, subId, register) {
	devices.HidppRequest.call(this, deviceIndex);

	var array = new Uint8Array(this.data);
	array[2] = subId;
	array[3] = register;

	var payload = Array.prototype.slice.call(arguments, 3);
	for (var i=0; i<Math.min(array.length-3, payload.length); i++) {
		array[4+i] = payload[i];
	}
}

my.HidppRapRequest.prototype = core.inherit(devices.HidppRequest.prototype);

// Request
// dataIn : ArrayBuffer
// returns { "matchResult" : MATCH_RESULT, "errCode" : RAP_ERROR };
my.HidppRapRequest.prototype.matches = function(dataIn) {
	core.Contract.assert(dataIn instanceof ArrayBuffer, "Expected ArrayBuffer.");

	var result = {
		"matchResult" : devices.MATCH_RESULT.NO_MATCH,
		"errCode"     : my.RAP_ERROR.SUCCESS
	};

	var reqView = new DataView(this.data);
	core.Contract.assert(reqView.byteLength >= 4);

	var rspView = new DataView(dataIn);
	core.Contract.assert(rspView.byteLength >= 4);

	// RAP register header
	var reqDeviceIndex = reqView.getUint8(1);
	var rspDeviceIndex = rspView.getUint8(1);
	var reqSubId       = reqView.getUint8(2);
	var rspSubId       = rspView.getUint8(2);
	var reqAddress     = reqView.getUint8(3);
	var rspAddress     = rspView.getUint8(3);

	var rapMatch =
		reqDeviceIndex == rspDeviceIndex
		   && reqSubId == rspSubId
		 && reqAddress == rspAddress;

	if (rapMatch) {
		result.matchResult = devices.MATCH_RESULT.SUCCESS;

	} else if (rspSubId == my.RAP_SUBID.ERROR_MSG_REGISTER){
		core.Contract.assert(rspView.byteLength == 7); // HID++ short message

		// ERROR_MSG_REGISTER
		var errSubIdSource = rspView.getUint8(3);
		var errAddress     = rspView.getUint8(4);

		rapMatch =
			reqDeviceIndex == rspDeviceIndex
			   && reqSubId == errSubIdSource
			 && reqAddress == errAddress;

		if (rapMatch) {
			result.errCode = rspView.getUint8(5); // RAP_ERROR
			result.matchResult = devices.MATCH_RESULT.ERROR;
		}
	}

	return result;
}

////////////////////////////////////////////////////////
// GetReportingFlags

my.GetReportingFlags = function(deviceIndex) {
	my.HidppRapRequest.call(this, deviceIndex,
		my.RAP_SUBID.GET_REGISTER, my.RAP_REGISTER.HIDPP_REPORTING);
}

my.GetReportingFlags.prototype = core.inherit(my.HidppRapRequest.prototype);

// Request
my.GetReportingFlags.prototype.onMatched = function(response) {
	var dataView = new DataView(response.data);

	response.flags = {
		bFlagsDevices:     dataView.getUint8(4),
		bFlagsReceiver:    dataView.getUint8(5),
		bFlagsDevicesCont: dataView.getUint8(6),
	};
}

////////////////////////////////////////////////////////
// SetReportingFlags

my.SetReportingFlags = function(deviceIndex,
		bFlagsDevices, bFlagsReceiver, bFlagsDevicesCont) {
	my.HidppRapRequest.call(this, deviceIndex,
		my.RAP_SUBID.SET_REGISTER, my.RAP_REGISTER.HIDPP_REPORTING,
		bFlagsDevices, bFlagsReceiver, bFlagsDevicesCont);
}

my.SetReportingFlags.prototype = core.inherit(my.HidppRapRequest.prototype);

////////////////////////////////////////////////////////
// FakeDeviceArrival

my.FakeDeviceArrival = function() {
	var bAction = 2; // fake device arrival
	my.HidppRapRequest.call(this, devices.HIDPP_DEVICE_INDEX.RECEIVER,
		my.RAP_SUBID.SET_REGISTER, my.RAP_REGISTER.CONNECTION_STATE,
		bAction);
}

my.FakeDeviceArrival.prototype = core.inherit(my.HidppRapRequest.prototype);

////////////////////////////////////////////////////////
// GetBatteryStatus

my.GetBatteryStatus = function(deviceIndex) {
	my.HidppRapRequest.call(this, deviceIndex,
		my.RAP_SUBID.GET_REGISTER, my.RAP_REGISTER.BATTERY_STATUS);
}

my.GetBatteryStatus.prototype = core.inherit(my.HidppRapRequest.prototype);

// Request
my.GetBatteryStatus.prototype.onMatched = function(response) {
	var dataView = new DataView(response.data);
	response.status = {
		level: dataView.getUint8(4),
		charge_level: dataView.getUint8(5),
		warning_level: dataView.getUint8(6)
	}
}

////////////////////////////////////////////////////////
// GetBatteryMileage

my.GetBatteryMileage = function(deviceIndex) {
	my.HidppRapRequest.call(this, deviceIndex,
		my.RAP_SUBID.GET_REGISTER, my.RAP_REGISTER.BATTERY_MILEAGE);
}

my.GetBatteryMileage.prototype = core.inherit(my.HidppRapRequest.prototype);

// Request
my.GetBatteryMileage.prototype.onMatched = function(response) {
	var dataView = new DataView(response.data);
	response.mileage = {
		charge: dataView.getUint8(4),
		nominalCondition: ( dataView.getUint8(5)
			| ( dataView.getUint8(6) & 0x0F) << 8),
		timeUnit: (dataView.getUint8(6) & 0x30) >> 4,
		chargeState: (dataView.getUint8(6) & 0xC0) >> 4
	}
}

////////////////////////////////////////////////////////
// UnpairDevice

// Parameters:
// integer deviceIndex
my.UnpairDevice = function(deviceIndex) {
	my.HidppRapRequest.call(this, devices.HIDPP_DEVICE_INDEX.RECEIVER,
		my.RAP_SUBID.SET_REGISTER, my.RAP_REGISTER.QUAD_CONNECT_DEVICE,
		my.RAP_QUAD_CONNECT_DEVICES.DISCONNECT, deviceIndex);
	this.targetDeviceIndex = deviceIndex;
}

my.UnpairDevice.prototype = core.inherit(my.HidppRapRequest.prototype);

////////////////////////////////////////////////////////
// OpenOrCloseLock

// Parameters:
// integer timeout
my.OpenOrCloseLock = function(timeout) {
	my.HidppRapRequest.call(this, devices.HIDPP_DEVICE_INDEX.RECEIVER,
		my.RAP_SUBID.SET_REGISTER, my.RAP_REGISTER.QUAD_CONNECT_DEVICE,
		timeout == 0 ? my.RAP_QUAD_CONNECT_DEVICES.CLOSE_LOCK :
			my.RAP_QUAD_CONNECT_DEVICES.OPEN_LOCK,
		0x00, timeout);
}

my.OpenOrCloseLock.prototype = core.inherit(my.HidppRapRequest.prototype);

////////////////////////////////////////////////////////
// GetNonVolatileAndPairingInfo

// Parameters:
// integer deviceIndex
my.GetNonVolatileAndPairingInfo = function(chunkId, deviceIndex) {
	my.HidppRapRequest.call(this, devices.HIDPP_DEVICE_INDEX.RECEIVER,
		my.RAP_SUBID.GET_LONG_REGISTER, my.RAP_REGISTER.NV_PAIRING_INFO,
		chunkId);
	this.targetDeviceIndex = deviceIndex;
}

my.GetNonVolatileAndPairingInfo.prototype = core.inherit(my.HidppRapRequest.prototype);

// dataIn : ArrayBuffer
// returns { "matchResult" : MATCH_RESULT, "errCode" : RAP_ERROR };
my.GetNonVolatileAndPairingInfo.prototype.matches = function(dataIn) {
	core.Contract.assert(dataIn instanceof ArrayBuffer, "Expected ArrayBuffer.");
	
	var result = my.HidppRapRequest.prototype.matches.call(this, dataIn);

	if (result.matchResult == devices.MATCH_RESULT.SUCCESS) {
		var reqView = new DataView(this.data);
		var rspView = new DataView(dataIn);
		if (reqView.getUint8(4) != rspView.getUint8(4)) {
			// Chunk IDs do not match.
			result.matchResult = devices.MATCH_RESULT.NO_MATCH;
		}
	}

	return result;
}

////////////////////////////////////////////////////////
// GetTransceiverEQuadInformation

my.GetTransceiverEQuadInformation = function() {
	my.GetNonVolatileAndPairingInfo.call(this, 0x03);
}

my.GetTransceiverEQuadInformation.prototype = core.inherit(
	my.GetNonVolatileAndPairingInfo.prototype);

// Request
my.GetTransceiverEQuadInformation.prototype.onMatched = function(response) {
	var dataView = new DataView(response.data);

	var eQuadBaseAddress = "";
	for (var i=0; i<4; i++) {
		eQuadBaseAddress += dataView.getUint8(i + 5)
			.toString(16).padLeft("0", 2) + "-";
	}

	response.eQuadInfo = {
		baseAddress: eQuadBaseAddress.replace(/-$/, '')
	};
}

////////////////////////////////////////////////////////
// GetNonVolatileEQuadDjExtendedPairingInfo

// Parameters:
// integer deviceIndex
my.GetNonVolatileEQuadDjExtendedPairingInfo = function(deviceIndex) {
	var chunkId = 0x30 + (deviceIndex - 1 );
	my.GetNonVolatileAndPairingInfo.call(this, chunkId, deviceIndex);
}

my.GetNonVolatileEQuadDjExtendedPairingInfo.prototype = core.inherit(
	my.GetNonVolatileAndPairingInfo.prototype);

// Request
my.GetNonVolatileEQuadDjExtendedPairingInfo.prototype.onMatched = function(response) {
	var dataView = new DataView(response.data);

	var serialNumber = "";
	for (var i=0; i<4; i++) {
		serialNumber += dataView.getUint8(i + 5)
			.toString(16).padLeft("0", 2) + "-";
	}

	response.serialNumber = serialNumber.replace(/-$/, '');
}

////////////////////////////////////////////////////////
// GetNonVolatileEQuadDjDeviceName

// Parameters:
// integer deviceIndex
my.GetNonVolatileEQuadDjDeviceName = function(deviceIndex) {
	var chunkId = 0x40 + (deviceIndex - 1 );
	my.GetNonVolatileAndPairingInfo.call(this, chunkId, deviceIndex);
}

my.GetNonVolatileEQuadDjDeviceName.prototype = core.inherit(
	my.GetNonVolatileAndPairingInfo.prototype);

// Request
my.GetNonVolatileEQuadDjDeviceName.prototype.onMatched = function(response) {
	var dataView         = new DataView(response.data);
	var deviceNameLength = dataView.getUint8(5) + 1;
	
	var deviceName = "";
	for (var i=0; i<deviceNameLength; i++) {
		deviceName += String.fromCharCode(dataView.getUint8(i + 5));
	}

	response.deviceName = deviceName;
}

////////////////////////////////////////////////////////
// GetFirmwareVersionItem

// Parameters:
// integer deviceIndex
my.GetFirmwareVersionItem = function(deviceIndex, fwVersionItem) {
	my.HidppRapRequest.call(this, deviceIndex,
		my.RAP_SUBID.GET_REGISTER, my.RAP_REGISTER.FW_VERSION,
		fwVersionItem);
	this.fwVersionItem = fwVersionItem;
}

my.GetFirmwareVersionItem.prototype = core.inherit(my.HidppRapRequest.prototype);

// Request
my.GetFirmwareVersionItem.prototype.onMatched = function(response) {
	var dataView = new DataView(response.data);
	
	switch (this.fwVersionItem) {
		case my.RAP_FW_VERSION_ITEM.FW_VERSION: {
				response.fwVersion =
					dataView.getUint8(5).toString(16) + "." +
					dataView.getUint8(6).toString(16);
			}
			break;

		case my.RAP_FW_VERSION_ITEM.FW_BUILD: {
				response.fwBuild =
					dataView.getUint16(5).toString(16);
			}
			break;

		case my.RAP_FW_VERSION_ITEM.HW_VERSION: {
				// TODO(jracle): parse it
			}
			break;

		case my.RAP_FW_VERSION_ITEM.BL_VERSION: {
				// TODO(jracle): parse it
			}
			break;

		default:
			break;
	}
}

////////////////////////////////////////////////////////
// DeviceActivity

my.DeviceActivity = function() {
	my.HidppRapRequest.call(this, devices.HIDPP_DEVICE_INDEX.RECEIVER,
		my.RAP_SUBID.GET_LONG_REGISTER, my.RAP_REGISTER.DEVICE_ACTIVITY);
}

my.DeviceActivity.prototype = core.inherit(my.HidppRapRequest.prototype);

my.DeviceActivity.prototype.onMatched = function(response) {
	response.activities = response.data.slice(4, 10);
}

////////////////////////////////////////////////////////
// DeviceActivityMonitor

my.DeviceActivityMonitor = function(rap) {
	core.Contract.assert(rap instanceof devices.HidppRap, "Expected HidppRap.");
	core.Observable.call(this);
	this.rap     = rap;
	this.worker  = null;
	this.started = false;
	this.exiting = false;
}

my.DeviceActivityMonitor.prototype = core.inherit(core.Observable.prototype);

// Closable
my.DeviceActivityMonitor.prototype.onClosing = function(callback) {
	this.exiting = true;

	if (this.worker) {
		clearTimeout(this.worker);
		this.worker = null;
	}

	core.Closable.prototype.onClosing.apply(this, [callback]);
}

// Observable
my.DeviceActivityMonitor.prototype.onObserveEvent = function(name, callback) {
	this.start();
}

my.DeviceActivityMonitor.prototype.start = function() {
	if (!this.started) {
		this.started = true;
		this.workerFunc();
	}
}

my.DeviceActivityMonitor.prototype.workerFunc = function() {
	var self = this;

	var doMonitorActivity = function() {
		var request = new my.DeviceActivity();
		request.isCritical = false;
		self.rap.requests.enqueue(request);

		request.waitResponse(function(response) {
			if (!self.exiting) {
				self.raiseEvent('deviceActivity', response.activities);
				self.worker = setTimeout(doMonitorActivity, 500);
			}
		});
	};

	doMonitorActivity();
}

////////////////////////////////////////////////////////
// HidppRap

my.HidppRap = function(device) {
	devices.HidppProtocol.call(this, device);

	this.deviceActivities      = null;
	this.deviceActivityHandle  = this.onDeviceActivity.bind(this);
	this.deviceActivityMonitor = null;
}

my.HidppRap.prototype = core.inherit(devices.HidppProtocol.prototype);

// Object
my.HidppRap.prototype.toString = function() {
	return "HidppRap";
}

// Closable
my.HidppRap.prototype.onClosing = function(callback) {
	if (this.deviceActivityMonitor) {
		this.deviceActivityMonitor.unobserveEvent('deviceActivity',
			this.deviceActivityHandle);
		this.deviceActivityMonitor.close();
	}

	devices.HidppProtocol.prototype.onClosing.apply(this, [callback]);
}

// Protocol
my.HidppRap.prototype.onStarting = function(callback) {
	var self = this;

	var getReportingFlags = new my.GetReportingFlags(self.device.index);
	self.requests.enqueue(getReportingFlags);

	getReportingFlags.waitResponse(function(response) {
		// Device is a receiver
		if (self.device.index == devices.HIDPP_DEVICE_INDEX.RECEIVER) {
			// Enable wireless notifications
			var enableReporting = new my.SetReportingFlags(self.device.index,
				response.flags.bFlagsDevices,
				response.flags.bFlagsReceiver | 0x01,
				response.flags.bFlagsDevicesCont);
			self.requests.enqueue(enableReporting);

			enableReporting.waitResponse(function(response) {
				// Fake device arrival
				var fakeDeviceArrival = new my.FakeDeviceArrival();
				self.requests.enqueue(fakeDeviceArrival);

				fakeDeviceArrival.waitResponse(function(response) {
					self.deviceActivityMonitor = new my.DeviceActivityMonitor(self);
					self.deviceActivityMonitor.observeEvent('deviceActivity',
						self.deviceActivityHandle);

					devices.HidppProtocol.prototype.onStarting.apply(self);
				});
			});

		// Wireless device
		} else {
			// Enable battery status/mileage change notifications
			var enableReporting = new my.SetReportingFlags(self.device.index,
				response.flags.bFlagsDevices | 0x10,
				response.flags.bFlagsReceiver,
				response.flags.bFlagsDevicesCont);
			self.requests.enqueue(enableReporting);

			enableReporting.waitResponse(function(response) {
				devices.HidppProtocol.prototype.onStarting.apply(self);
			});
		}
	});
}

// TransactionProtocol
my.HidppRap.prototype.onNotification = function(notification) {
	core.Contract.assert(notification instanceof devices.Notification,
		"Expected Notification");

	var dataView = new DataView(notification.data);
	notification.deviceIndex = dataView.getUint8(1);

	switch (dataView.byteLength) {
		case 7:
			this.onShortNotification(notification, dataView);
			break;

		case 20:
			this.onLongNotification(notification, dataView);
			break;

		default:
			break;
	}

	return notification;
}

my.HidppRap.prototype.onShortNotification = function(notification, dataView) {
	core.Contract.assert(notification instanceof devices.Notification,
		"Expected Notification");
	core.Contract.assert(dataView instanceof DataView, "Expected DataView");
	core.Contract.assert(dataView.getUint8(0) == devices.REPORT_ID.HIDPP_SHORT,
		"Expected 0x10");

	switch (dataView.getUint8(2)) { // report sub-ID
		case my.RAP_SUBID.BATTERY_STATUS:
			{
				if (this.device.index != devices.HIDPP_DEVICE_INDEX.RECEIVER) {
					var status = {
						level: dataView.getUint8(3),
						charge_level: dataView.getUint8(4),
						warning_level: dataView.getUint8(5)
					};

					this.raiseEvent('batteryStatusChanged', status);
				}
			}
			break;

		case my.RAP_SUBID.BATTERY_MILEAGE:
			{
				if (this.device.index != devices.HIDPP_DEVICE_INDEX.RECEIVER) {
					var mileage = {
						charge: dataView.getUint8(3),
						nominalCondition: ( dataView.getUint8(4)
							| ( dataView.getUint8(5) & 0x0F) << 8),
						timeUnit: (dataView.getUint8(5) & 0x30) >> 4,
						chargeState: (dataView.getUint8(5) & 0xC0) >> 4
					};

					this.raiseEvent('batteryMileageChanged', mileage);
				}
			}
			break;

		case my.RAP_SUBID.DEVICE_DISCONNECT:
			{
				if (this.device.index == devices.HIDPP_DEVICE_INDEX.RECEIVER) {
					var disconnectType = dataView.getUint8(3);

					core.Log.info("Wireless device #%d removed (type: '%s')",
						notification.deviceIndex,
						my.RAP_DISCONNECT_TYPE.toString(disconnectType));

					this.device.provider.onWirelessDeviceDisconnect(
						this.device, notification.deviceIndex, disconnectType);
				}
			}
			break;

		case my.RAP_SUBID.DEVICE_CONNECT:
			{
				core.Contract.assert(notification.deviceIndex
					!= devices.HIDPP_DEVICE_INDEX.RECEIVER);

				if (this.device.index == devices.HIDPP_DEVICE_INDEX.RECEIVER) {
					notification.protocolType = dataView.getUint8(3);

					switch (notification.protocolType) {
						case my.RAP_PROTOCOL_TYPE.TWENTY_SEVEN_MHZ:
							core.Contract.assert(false, "Not supported");
							return;

						case my.RAP_PROTOCOL_TYPE.BLUETOOTH:
							notification.devInfo = dataView.getUint8(4);
							notification.devId   = dataView.getUint8(5)
								| dataView.getUint8(6) << 8;
							break;

						default:
							notification.devInfo = dataView.getUint8(4);
							notification.devId   = dataView.getUint8(5)
								| dataView.getUint8(6) << 8;
							break;
					}

					var noLinkMask = 1 << my.RAP_CONNECT_MASK.NO_LINK;
					var noLink = notification.devInfo & noLinkMask;
					var deviceType = notification.devInfo & my.RAP_CONNECT_MASK.TYPE_MASK;

					notification.connected = !noLink;

					core.Log.info("Wireless device #%d (0x%s) %s",
						notification.deviceIndex, notification.devId.toString(16).padLeft("0", 2),
						notification.connected ? "connected" : "disconnected");

					this.device.provider.onWirelessDeviceArrival(
						this.device, notification.deviceIndex, notification.connected, {
							productId : notification.devId,
							isHidpp : true,
							rapDeviceType : deviceType
						});
				}
			}
			break;

		case my.RAP_SUBID.QUAD_LOCKING_INFO:
			{
				if (notification.deviceIndex == devices.HIDPP_DEVICE_INDEX.RECEIVER) {
					var isLocked = dataView.getUint8(3) != 1;
					var  errCode = dataView.getUint8(4);

					this.device.isLocked = isLocked;

					core.Log.info("Receiver status changed: %s, error: '%s'",
						isLocked ? "locked" : "unlocked",
						my.RAP_QUAD_LOCKING_INFO_ERROR.toString(errCode));
				}
			}
			break;

		default:
			break;
	}
}

my.HidppRap.prototype.onLongNotification = function(notification, dataView) {
	core.Contract.assert(notification instanceof devices.Notification,
		"Expected Notification");
	core.Contract.assert(dataView instanceof DataView, "Expected DataView");
	core.Contract.assert(dataView.getUint8(0) == devices.REPORT_ID.HIDPP_LONG,
		"Expected 0x11");

	switch (dataView.getUint8(2)) { // report sub-ID
		// TODO(jracle) : implement other notifications

		default:
			break;
	}
}

my.HidppRap.prototype.onDeviceActivity = function(activities) {
	var self = this;

	var onWirelessDeviceActivityChanged = function(deviceIndex, active) {
		var wirelessDevice = self.device.provider
			.getWirelessDevice(self.device, deviceIndex);

		if (wirelessDevice) {
			wirelessDevice.active = active;
		}
	}

	if (self.deviceActivities) {
		var dataView1 = new DataView(this.deviceActivities);
		var dataView2 = new DataView(activities);

		for (var i=0; i<activities.byteLength; i++) {
			if (dataView1.getUint8(i) !== dataView2.getUint8(i)) {
				onWirelessDeviceActivityChanged(i+1, true);
			} else {
				onWirelessDeviceActivityChanged(i+1, false);
			}
		}
	}

	self.deviceActivities = activities;
}

my.HidppRap.prototype.getChildDeviceSerialNumber = function(device, callback) {
	core.Contract.assert(this.device.index == devices.HIDPP_DEVICE_INDEX.RECEIVER,
		"Expected receiver");
	var self = this;

	var request = new my.GetNonVolatileEQuadDjExtendedPairingInfo(device.index);
	self.requests.enqueue(request);

	request.waitResponse(function(response) {
		var serialNumber = device.productId.toString(16) + "-" + response.serialNumber;
		callback && callback.apply(self, [serialNumber]);
	});
}

my.HidppRap.prototype.getChildDeviceName = function(device, callback) {
	core.Contract.assert(this.device.index == devices.HIDPP_DEVICE_INDEX.RECEIVER,
		"Expected receiver");
	var self = this;

	var request = new my.GetNonVolatileEQuadDjDeviceName(device.index);
	self.requests.enqueue(request);

	request.waitResponse(function(response) {
		callback && callback.apply(self, [response.deviceName]);
	});
}

////////////////////////////////////////////////////////
// Device extensions

devices.Device.prototype.openOrCloseLock = function(timeout, callback, failCallback) {
	core.Contract.assert(this.index == devices.HIDPP_DEVICE_INDEX.RECEIVER,
		"Expected receiver");
	var self = this;

	self.waitProperty('protocol', function(protocol) {
		core.Contract.assert(protocol instanceof devices.HidppRap,
			"Expected Hid++ 1.0");

		var request = new my.OpenOrCloseLock(timeout);

		if (failCallback) {
			core.Contract.assert(typeof failCallback == 'function',
				"Expected function");
			request.maxRetryCount = 0;
		}

		protocol.requests.enqueue(request);

		if (failCallback) {
			request.waitResponse(function() {
				callback && callback.apply(self);
			},
			failCallback);

		} else {
			request.waitResponse(function() {
				callback && callback.apply(self);
			});
		}
	});
}

devices.Device.prototype.unpair = function(callback, failCallback) {
	core.Contract.assert(this.index != devices.HIDPP_DEVICE_INDEX.RECEIVER,
		"Expected wireless device");
	core.Contract.assert(this.parent.index == devices.HIDPP_DEVICE_INDEX.RECEIVER,
		"Expected parent is receiver");
	var self = this;

	self.parent.waitProperty('protocol', function(protocol) {
		core.Contract.assert(protocol instanceof devices.HidppRap,
			"Expected Hid++ 1.0");

		protocol.requests.clean(self.index, function() {
			var request = new my.UnpairDevice(self.index);

			if (failCallback) {
				core.Contract.assert(typeof failCallback == 'function',
					"Expected function");
				request.maxRetryCount = 0;
			}

			protocol.requests.enqueue(request);

			if (failCallback) {
				request.waitResponse(function(response) {
					callback && callback.apply(self);
				},
				failCallback);

			} else {
				request.waitResponse(function(response) {
					callback && callback.apply(self);
				});
			}
		});
	});
}

return my;

}(devices || {}));
