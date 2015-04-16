

#ifdef HAVE_FTDI
#include "ftd2xx.h"
#endif

#ifdef HAVE_FTDI
const char* ft_strerror(FT_STATUS status)
{
    switch(status) {
    case FT_OK:	return "ok";
    case FT_INVALID_HANDLE: return "invalid handle";
    case FT_DEVICE_NOT_FOUND: return "device not found";
    case FT_DEVICE_NOT_OPENED: return "device not opened";
    case FT_IO_ERROR:	return "io error";
    case FT_INSUFFICIENT_RESOURCES: return "insufficent resources";
    case FT_INVALID_PARAMETER: return "invalid paramter";
    case FT_INVALID_BAUD_RATE: return "invalid baud rate";
    case FT_DEVICE_NOT_OPENED_FOR_ERASE: return "device not opened for erase";
    case FT_DEVICE_NOT_OPENED_FOR_WRITE: return "device not opened for write";
    case FT_FAILED_TO_WRITE_DEVICE: return "faile to write device";
    case FT_EEPROM_READ_FAILED: return "eeprom read failed";
    case FT_EEPROM_WRITE_FAILED: return "eeprom write failed";
    case FT_EEPROM_ERASE_FAILED: return "eeprom erase failed";
    case FT_EEPROM_NOT_PRESENT: return "eeprom not present";
    case FT_EEPROM_NOT_PROGRAMMED: return "eeprom not programmed";
    case FT_INVALID_ARGS: return "invalid arguments";
    case FT_NOT_SUPPORTED: return "not supported";
    case FT_OTHER_ERROR: return "other error";
    default: return "unknown error (%d)";
    }
}
#endif

