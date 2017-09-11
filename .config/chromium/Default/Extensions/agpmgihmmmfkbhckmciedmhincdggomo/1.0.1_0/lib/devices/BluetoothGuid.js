'use strict';

var devices = (function (my) {

// https://developer.bluetooth.org
// https://www.bluetooth.org/en-us/specification/assigned-numbers

// Used in promoting 16-bit and 32-bit UUIDs to 128-bit UUIDs
my.BLUETOOTH_UUID_128_PAYLOAD = "-0000-1000-8000-00805f9b34fb";
my.LOGITECH_UUID_128_PAYLOAD = "-0000-1000-8000-011f2000046d";

// List of known unique identifiers, expresset as 32-bit UUIDs
// Short (16-bit) and long (32-bit) uuids can be promoted to Guid format (128-bit)
// via above payloads.
my.BLUETOOTH_UUID = {

    ///////////////////////////////////////////////////////////////////
    // Standard UUIDs

    ////////////
    // Protocols

    Protocol_SDP                               : 0x00000001,
    Protocol_UDP                               : 0x00000002,
    Protocol_RFCOMM                            : 0x00000003,
    Protocol_TCP                               : 0x00000004,
    Protocol_TCS_BIN                           : 0x00000005,
    Protocol_TCS_AT                            : 0x00000006,
    // 0x0007
    Protocol_OBEX                              : 0x00000008,
    Protocol_IP                                : 0x00000009,
    Protocol_FTP                               : 0x0000000A,
    // 0x000B
    Protocol_HTTP                              : 0x0000000C,
    // 0x000D
    Protocol_WSP                               : 0x0000000E,
    Protocol_BNEP                              : 0x0000000F,
    Protocol_UPNP                              : 0x00000010,
    Protocol_HIDP                              : 0x00000011,
    Protocol_HardcopyControlChannel            : 0x00000012,
    // 0x0013
    Protocol_HardcopyDataChannel               : 0x00000014,
    // 0x0015
    Protocol_HardcopyNotification              : 0x00000016,
    Protocol_AVCTP                             : 0x00000017,
    // 0x0018
    Protocol_AVDTP                             : 0x00000019,
    // 0x001A
    Protocol_CMTP                              : 0x0000001B,
    // ...
    Protocol_UDI_C_Plane                       : 0x00000017,
    // ...
    Protocol_L2CAP                             : 0x00000100,
    
    //////////////////////////////////////////
    // Services (Class Identifiers)

    // TODO(jracle): complete list
    Service_ServiceDiscoveryServer             : 0x00001000,
    Service_BrowseGroupDescriptor              : 0x00001001,
    Service_PublicBrowseGroup                  : 0x00001002,
    Service_SerialPort                         : 0x00001101,
    //Service_LANAccessUsingPPP
    //Service_DialupNetworking
    //Service_IrMCSync
    //Service_OBEXObjectPush
    //Service_OBEXFileTransfer
    //Service_IrMCSyncCommand
    //Service_Headset
    //Service_CordlessTelephony
    //Service_AudioSource
    //Service_AudioSink
    //Service_AV_RemoteControlTarget
    //Service_AdvancedAudioDistribution
    //Service_AV_RemoteControl
    // ...
    Service_0x1124_HumanInterface             : 0x00001124,
    // ...
    Service_0x1400_HealthDeviceProfile        : 0x00001400,
    Service_0x1400_HealthDeviceProfile_Source : 0x00001401,
    Service_0x1400_HealthDeviceProfile_Sink   : 0x00001402,

    ///////////////
    // BLE Services

    Service_0x1800_GenericAccess              : 0x00001800,
    Service_0x1801_GenericAttribute           : 0x00001801,
    Service_0x1802_ImmediateAlert             : 0x00001802,
    Service_0x1803_LinkLoss                   : 0x00001803,
	Service_0x1804_TxPower                    : 0x00001804,
    Service_0x1805_CurrentTime                : 0x00001805,
    Service_0x1806_ReferenceTimeUpdate        : 0x00001806,
    Service_0x1807_NextDSTChange              : 0x00001807,
    Service_0x1808_Glucose                    : 0x00001808,
    Service_0x1809_HealthThermometer          : 0x00001809,
    Service_0x180A_DeviceInformation          : 0x0000180A,
    //Service_0x180B                          : 0x0000180B,
    //Service_0x180C                          : 0x0000180C,
    Service_0x180D_HeartRate                  : 0x0000180D,
    Service_0x180E_PhoneAlertStatus           : 0x0000180E,
    Service_0x180F_Battery                    : 0x0000180F,
    Service_0x1810_BloodPressure              : 0x00001810,
    Service_0x1811_AlertNotification          : 0x00001811,
    Service_0x1812_HID_Smart                  : 0x00001812,
    Service_0x1813_ScanParameters             : 0x00001813,
    Service_0x1814_RunningSpeedAndCadence     : 0x00001814,
    //Service_0x1815                          : 0x00001815,
    Service_0x1816_CyclingSpeedAndCadence     : 0x00001816,
    //Service_0x1817                          : 0x00001817,
    Service_0x1818_CyclingPower               : 0x00001818,
    Service_0x1819_LocationAndNavigation      : 0x00001819,

    //////////////////////
    // BLE Characteristics
    // TODO(jracle)

    ///////////////////////////////////////////////////////////////////
    // Logitech UUIDs

    Service_0x0001_HID_PlusPlus        : 0x00010000,
    Service_0x0002_SanJuan             : 0x00020000,
    Service_0x0003_IM_Glasses          : 0x00030000,
    Service_0x0004_IM_RC               : 0x00040000,
    Service_0x0005_Princess_Bootloader : 0x00050000,
    Service_0x01E0_iXP                 : 0x01E00000,

    // Helpers
    parse : function(guid) {
        core.Contract.assert(typeof guid === "string", "Expected String");

        var standardGuid = /^([0-9a-f]{8})-0000-1000-8000-00805f9b34fb$/i;
        var result = guid.match(standardGuid);
        if (result) {
            return { standardUuid : parseInt(result[1], 16) };
        }

        var logitechGuid = /^([0-9a-f]{8})-0000-1000-8000-011f2000046d$/i;
        result = guid.match(logitechUuid);
        if (result) {
            return { logitechUuid : parseInt(result[1], 16) };
        }

        return { };
    },
    isBleService : function(guid) {
        core.Contract.assert(typeof guid === "string", "Expected String");

        var uuid = my.BLUETOOTH_UUID.parse(guid);

        if (uuid.standardUuid) {
            return uuid.standardUuid >= my.BLUETOOTH_UUID.Service_0x1800_GenericAccess
                && uuid.standardUuid <= 0x18FF; // TOFIX(jracle): upper limit

        } else if (uuid.logitechUuid) {
            // Can be a characteristic, need to mask to verify
            return (uuid.logitechUuid | 0x0000FFFF) === 0x0000;
        }
    }
};

return my;

}(devices || {}));
