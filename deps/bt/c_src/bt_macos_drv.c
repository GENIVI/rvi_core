/*
 * BT driver for Erlang
 *
 */
#include <AvailabilityMacros.h>
#include <sys/types.h>
#include <string.h>
#include <memory.h>
#include <CoreFoundation/CoreFoundation.h>
#include <IOBluetooth/IOBluetoothUtilities.h>
#include <IOBluetooth/IOBluetoothUserLib.h>
#include <IOBluetooth/objc/IOBluetoothDevice.h>
#include <IOBluetooth/objc/IOBluetoothSDPServiceRecord.h>
#include <IOBluetooth/objc/IOBluetoothDeviceInquiry.h>
#include <IOBluetooth/objc/IOBluetoothRFCOMMChannel.h>
#include <IOBluetooth/objc/IOBluetoothL2CAPChannel.h>
#include <IOBluetooth/objc/IOBluetoothSDPDataElement.h>
#include <IOBluetooth/objc/IOBluetoothSDPUUID.h>
#include <IOBluetooth/objc/IOBluetoothHostController.h>

#include "dthread/include/dlog.h"
#include "dthread/include/ddata.h"
#include "bt_drv.h"

#define alloc_type(type) calloc(1, sizeof(type))

//  Object structure
// +----------+-------------------------------------------v
// |          |     +----------+     +----------+     +----------+
// | ctx.list | <-> | sub_link | <-> | sub_link | <-> | sub_link |
// +----------+     +----+-----+     +-----+----+     +----------+
//                       |                 |
//                  +----v-------+   +-----v------+
//                  | subscriber1|   | subscriber2|
//                  |   INQUIRY  |   |   RFCOMM   |
//                  +----+-------+   +-------+----+
//                       | handle            | handle
//      +----------------v---------+   +-----v--------------------+
//      | IOBluetoothDeviceInquiry |   | IOBluetoothRFCOMMChannel |
//      +-------+------------------+   +-----+--------------------+
//              |                            |
//      +-------v---------+            +-----v--------------------+
//      | InquiryDelegate |            | RFCOMMChannelDelegate    |
//      +-------+---------+            +----------+---------------+
//              |                                 |
//              v                                 v
//            subscriber1 !                   subscriber2 ! 
//

// -----------------------------------------------------------------------
//   Delgate classes
// -----------------------------------------------------------------------

@interface InquiryDelegate : NSObject <IOBluetoothDeviceInquiryDelegate> {
    subscription_t* mSub;
}
- (instancetype)initWithSub:(subscription_t*)s;
- (void) dealloc;
@end

@interface RemoteNameDelegate : NSObject <IOBluetoothDeviceInquiryDelegate> {
    subscription_t* mSub;
}
- (instancetype)initWithSub:(subscription_t*) s;
- (void) dealloc;
@end

@interface ConnectDelegate : NSObject <IOBluetoothDeviceInquiryDelegate> {
    subscription_t* mSub;
}
- (instancetype)initWithSub:(subscription_t*) s;
- (void) dealloc;
@end

@interface SDPQueryDelegate : NSObject <IOBluetoothDeviceInquiryDelegate> {
    subscription_t* mSub;
    IOBluetoothSDPUUID* mUUID;
}
- (instancetype)initWithSub:(subscription_t*) s andUUID:(IOBluetoothSDPUUID*)uuid;
- (void) dealloc;
@end

@interface RFCOMMChannelDelegate : NSObject <IOBluetoothRFCOMMChannelDelegate> {
    subscription_t* mSub;
    bt_ctx_t* mCtx;
}
- (instancetype)initWithSub:(subscription_t*) s andCtx:(bt_ctx_t*) ctx;
- (void) dealloc;
@end

@interface L2CAPChannelDelegate : NSObject <IOBluetoothL2CAPChannelDelegate> {
    subscription_t* mSub;
    bt_ctx_t* mCtx;
}
- (instancetype)initWithSub:(subscription_t*) s andCtx:(bt_ctx_t*) ctx;
- (void) dealloc;
@end

static inline void put_date(ddata_t* data, NSDate* date)
{
    uint8_t* ptr = ddata_alloc(data, 5);
    NSTimeInterval at = 0;
    uint32_t secs;

    if (date != NULL)
	at = [date timeIntervalSince1970];
    secs = (uint32_t) at;
    *ptr++ = DATE;
    DDATA_PUT_UINT32(ptr, secs);
}

static inline void put_ns_string(ddata_t* data, NSString* str)
{
    char buffer[1024];
    if ([str getCString:buffer maxLength:1024 encoding:NSUTF8StringEncoding])
	ddata_put_string(data, buffer);
    else
	ddata_put_string(data, "?");
}

static inline void ddata_put_addr(ddata_t* data, const BluetoothDeviceAddress* addr)
{
    uint8_t* ptr = ddata_alloc(data, sizeof(BluetoothDeviceAddress)+1);
    *ptr++ = ADDR;
    memcpy(ptr, addr, sizeof(BluetoothDeviceAddress));
}

static inline int get_address(ddata_t* data, BluetoothDeviceAddress* val)
{
    if (ddata_r_avail(data) >= sizeof(BluetoothDeviceAddress)) {
	memcpy(val, data->rd, sizeof(BluetoothDeviceAddress));
	data->rd += sizeof(BluetoothDeviceAddress);
	return 1;
    }
    return 0;
}


#define ERR_SHORT 1
#define ERR_LONG  2

/* type: 1=short-atom  2=string-long  3=both (encapsule in LIST ot TUPLE) */
static void ddata_put_io_error(ddata_t* data, IOReturn error, int type)
{
    switch(error) {
    case kIOReturnSuccess:
	if (type&1) ddata_put_atom(data, "Success");
	if (type&2) ddata_put_string(data, "OK");
	break;
    case kIOReturnError:
	if (type&1) ddata_put_atom(data, "Error");
	if (type&2) ddata_put_string(data, "general error");
	break;
    case kIOReturnNoMemory:
        if (type&1) ddata_put_atom(data, "NoMemory");
	if (type&2) ddata_put_string(data, "can't allocate memory");
	break;
    case kIOReturnNoResources:
	if (type&1) ddata_put_atom(data, "NoResources");
	if (type&2) ddata_put_string(data, "resource shortage ");
	break;
    case kIOReturnIPCError:
        if (type&1) ddata_put_atom(data, "IPCError");
	if (type&2) ddata_put_string(data, "error during IPC ");
	break;
    case kIOReturnNoDevice:
        if (type&1) ddata_put_atom(data, "NoDevice");
	if (type&2) ddata_put_string(data, "no such device ");
	break;
    case kIOReturnNotPrivileged:
	if (type&1) ddata_put_atom(data, "NotPrivileged");
	if (type&2) ddata_put_string(data, "privilege violation ");
	break;
    case kIOReturnBadArgument:
	if (type&1) ddata_put_atom(data, "BadArgument");
	if (type&2) ddata_put_string(data, "invalid argument ");
	break;
    case kIOReturnLockedRead:
	if (type&1) ddata_put_atom(data, "LockedRead");
	if (type&2) ddata_put_string(data, "device read locked ");
	break;
    case kIOReturnLockedWrite:
	if (type&1) ddata_put_atom(data, "LockedWrite");
	if (type&2) ddata_put_string(data, "device write locked ");
	break;
    case kIOReturnExclusiveAccess:
	if (type&1) ddata_put_atom(data, "ExclusiveAccess");
	if (type&2) ddata_put_string(data, "exclusive access and device already open");
	break;
    case kIOReturnBadMessageID:
	if (type&1) ddata_put_atom(data, "BadMessageID");
	if (type&2) ddata_put_string(data, "sent/received messages had different msg_id");
	break;
    case kIOReturnUnsupported:
	if (type&1) ddata_put_atom(data, "Unsupported");
	if (type&2) ddata_put_string(data, "unsupported function");
	break;
    case kIOReturnVMError:
	if (type&1) ddata_put_atom(data, "VMError");
	if (type&2) ddata_put_string(data, "misc. VM failure ");
	break;
    case kIOReturnInternalError:
	if (type&1) ddata_put_atom(data, "InternalError");
	if (type&2) ddata_put_string(data, "internal error ");
	break;
    case kIOReturnIOError:
	if (type&1) ddata_put_atom(data, "IOError");
	if (type&2) ddata_put_string(data, "General I/O error ");
	break;
    case kIOReturnCannotLock:
	if (type&1) ddata_put_atom(data, "CannotLock");
	if (type&2) ddata_put_string(data, "can't acquire lock");
	break;
    case kIOReturnNotOpen:
	if (type&1) ddata_put_atom(data, "NotOpen");
	if (type&2) ddata_put_string(data, "device not open ");
	break;
    case kIOReturnNotReadable:
	if (type&1) ddata_put_atom(data, "NotReadable");
	if (type&2) ddata_put_string(data, "read not supported ");
	break;
    case kIOReturnNotWritable:
	if (type&1) ddata_put_atom(data, "NotWritable");
	if (type&2) ddata_put_string(data, "write not supported");
	break;
    case kIOReturnNotAligned:
	if (type&1) ddata_put_atom(data, "NotAligned");
	if (type&2) ddata_put_string(data, "alignment error");
	break;
    case kIOReturnBadMedia:
        if (type&1) ddata_put_atom(data, "BadMedia");
	if (type&2) ddata_put_string(data, "Media Error");
	break;
    case kIOReturnStillOpen:
	if (type&1) ddata_put_atom(data, "StillOpen");
	if (type&2) ddata_put_string(data, "device(s) still open");
	break;
    case kIOReturnRLDError:
        if (type&1) ddata_put_atom(data, "RLDError");
	if (type&2) ddata_put_string(data, "rld failure");
	break;
    case kIOReturnDMAError:
        if (type&1) ddata_put_atom(data, "DMAError");
	if (type&2) ddata_put_string(data, "DMA failure");
	break;
    case kIOReturnBusy:
	if (type&1) ddata_put_atom(data, "Busy");
	if (type&2) ddata_put_string(data, "Device Busy");
	break;
    case kIOReturnTimeout:
	if (type&1) ddata_put_atom(data, "Timeout");
	if (type&2) ddata_put_string(data, "I/O Timeout");
	break;
    case kIOReturnOffline:
	if (type&1) ddata_put_atom(data, "Offline");
	if (type&2) ddata_put_string(data, "device offline");
	break;
    case kIOReturnNotReady:
        if (type&1) ddata_put_atom(data, "NotReady");
	if (type&2) ddata_put_string(data, "not ready");
	break;
    case kIOReturnNotAttached:
	if (type&1) ddata_put_atom(data, "NotAttached");
	if (type&2) ddata_put_string(data, "device not attached");
	break;
    case kIOReturnNoChannels:
	if (type&1) ddata_put_atom(data, "NoChannels");
	if (type&2) ddata_put_string(data, "no DMA channels left");
	break;
    case kIOReturnNoSpace:
	if (type&1) ddata_put_atom(data, "NoSpace");
	if (type&2) ddata_put_string(data, "no space for data");
	break;
    case kIOReturnPortExists:
	if (type&1) ddata_put_atom(data, "PortExists");
	if (type&2) ddata_put_string(data, "port already exists");
	break;
    case kIOReturnCannotWire:
	if (type&1) ddata_put_atom(data, "CannotWire");
	if (type&2) ddata_put_string(data, "can't wire down physical memory");
	break;
    case kIOReturnNoInterrupt:
	if (type&1) ddata_put_atom(data, "NoInterrupt");
	if (type&2) ddata_put_string(data, "no interrupt attached");
	break;
    case kIOReturnNoFrames:
        if (type&1) ddata_put_atom(data, "NoFrames");
	if (type&2) ddata_put_string(data, "no DMA frames enqueued");
	break;
    case kIOReturnMessageTooLarge:
	if (type&1) ddata_put_atom(data, "MessageTooLarge");
	if (type&2) ddata_put_string(data, "oversized msg received");
	break;
    case kIOReturnNotPermitted:
	if (type&1) ddata_put_atom(data, "NotPermitted");
	if (type&2) ddata_put_string(data, "not permitted");
	break;
    case kIOReturnNoPower:
	if (type&1) ddata_put_atom(data, "NoPower");
	if (type&2) ddata_put_string(data, "no power to device");
	break;
    case kIOReturnNoMedia:
	if (type&1) ddata_put_atom(data, "NoMedia");
	if (type&2) ddata_put_string(data, "media not present");
	break;
    case kIOReturnUnformattedMedia:
	if (type&1) ddata_put_atom(data, "UnformattedMedia");
	if (type&2) ddata_put_string(data, "media not formatted");
	break;
    case kIOReturnUnsupportedMode:
	if (type&1) ddata_put_atom(data, "UnsupportedMode");
	if (type&2) ddata_put_string(data, "no such mode");
	break;
    case kIOReturnUnderrun:
        if (type&1) ddata_put_atom(data, "Underrun");
	if (type&2) ddata_put_string(data, "data underrun");
	break;
    case kIOReturnOverrun:
	if (type&1) ddata_put_atom(data, "Overrun");
	if (type&2) ddata_put_string(data, "data overrun");
	break;
    case kIOReturnDeviceError:
	if (type&1) ddata_put_atom(data, "DeviceError");
	if (type&2) ddata_put_string(data, "the device is not working properly!");
	break;
    case kIOReturnNoCompletion:
	if (type&1) ddata_put_atom(data, "NoCompletion");
	if (type&2) ddata_put_string(data, "a completion routine is required");
	break;
     case kIOReturnAborted:
	 if (type&1) ddata_put_atom(data, "Aborted");
	 if (type&2) ddata_put_string(data, "operation aborted");
	break;
    case kIOReturnNoBandwidth:
	if (type&1) ddata_put_atom(data, "NoBandwidth");
	if (type&2) ddata_put_string(data, "bus bandwidth would be exceeded");
	break;
    case kIOReturnNotResponding:
	if (type&1) ddata_put_atom(data, "NotResponding");
	if (type&2) ddata_put_string(data, "device not responding");
	break;
    case kIOReturnIsoTooOld:
	if (type&1) ddata_put_atom(data, "IsoTooOld");
	if (type&2) ddata_put_string(data, "isochronous I/O request for distant past!");
	break;
    case kIOReturnIsoTooNew:
	if (type&1) ddata_put_atom(data, "IsoTooNew");
	if (type&2) ddata_put_string(data, "isochronous I/O request for distant future");
	break;
    case kIOReturnNotFound:
	if (type&1) ddata_put_atom(data, "NotFound");
	if (type&2) ddata_put_string(data, "data was not found");
	break;
    case kIOReturnInvalid:
	if (type&1) ddata_put_atom(data, "Invalid");
	if (type&2) ddata_put_string(data, "should never be seen");
	break;
	/* Add HCI error codes */
    default:
	DEBUGF("ERROR: code=%d, sub=%d, system=%d",
	    err_get_code(error), 
	    err_get_sub(error), 
	    err_get_system(error));
	if (type&1) ddata_put_atom(data, "Unknown");
	if (type&2) ddata_put_string(data, "do not know about this error");
	break;
    }
}

static void cleanup(subscription_t* s)
{
    DEBUGF("cleanup: %s", format_subscription(s));
    if (s->handle != NULL) {
	switch(s->type) {
	case INQUIRY: {
	    IOBluetoothDeviceInquiry* obj = s->handle;
	    if (obj) {
		InquiryDelegate* delegate = [obj delegate];
		[delegate release];
		[obj release];
	    }
	    break;
	}

	case REMOTE_NAME: {
	    IOBluetoothDeviceInquiry* obj = s->handle;
	    if (obj) {
		RemoteNameDelegate* delegate = [obj delegate];
		[delegate release];
		[obj release];
	    }
	    break;
	}

	case CONNECT: {
	    IOBluetoothDeviceInquiry* obj = s->handle;
	    if (obj) {
		ConnectDelegate* delegate = [obj delegate];
		[delegate release];
		[obj release];
	    }
	    break;
	}

	case SDP_QUERY: {
	    IOBluetoothDeviceInquiry* obj = s->handle;
	    if (obj) {
		SDPQueryDelegate* delegate = [obj delegate];
		[delegate release];
		[obj release];
	    }
	    break;
	}

	case SDP: {
	    IOBluetoothSDPServiceRecord* serviceRecord = s->handle;
	    [serviceRecord removeServiceRecord];
	    [serviceRecord release];
	    break;
	}
	case RFCOMM: {
	    IOBluetoothRFCOMMChannel* obj = s->handle;
	    if (obj) {
		RFCOMMChannelDelegate* delegate = [obj delegate];
		[delegate release];
		[obj release];
	    }
	    break;
	}
	case RFCOMM_LISTEN: {
	    IOBluetoothUserNotification* obj = s->handle;
	    if (obj)
		[obj unregister];
	    // check all acceptors here?
	    [obj release];
	    break;
	}
	case L2CAP: {
	    IOBluetoothL2CAPChannel* obj = s->handle;
	    if (obj) {
		L2CAPChannelDelegate* delegate = [obj delegate];
		[delegate release];
		[obj release];
	    }
	    break;
	}
	case L2CAP_LISTEN: {
	    IOBluetoothUserNotification* obj = s->handle;
	    if (obj)
		[obj unregister];
	    [obj release];
	    break;
	}
	default:  // warn?
	    break;
	}
    }
}


/* Send OK reply */
void bt_ok(uint32_t cmdid)
{
    uint8_t buf[16];
    ddata_t data;

    ddata_init(&data, buf, sizeof(buf), 0);
    ddata_put_UINT32(&data, 0);    
    ddata_put_tag(&data, REPLY_OK);
    ddata_put_UINT32(&data, cmdid);
    ddata_send(&data, 1);
    ddata_final(&data);
}

/* Send ERROR reply */
void bt_error(uint32_t cmdid, IOReturn err)
{
    uint8_t buf[128];
    ddata_t data;

    ddata_init(&data, buf, sizeof(buf), 0);
    ddata_put_UINT32(&data, 0);
    ddata_put_tag(&data, REPLY_ERROR);
    ddata_put_UINT32(&data, cmdid);
    ddata_put_io_error(&data, err, ERR_SHORT);
    ddata_send(&data, 1);
    ddata_final(&data);
}

// simple event
void bt_event(uint32_t sid, const char* evtname)
{
    uint8_t buf[64];
    ddata_t data;
    ddata_init(&data, buf, sizeof(buf), 0);
    ddata_put_UINT32(&data, 0);
    ddata_put_tag(&data, REPLY_EVENT);
    ddata_put_UINT32(&data, sid);
    ddata_put_atom(&data, evtname);
    ddata_send(&data, 1);
    ddata_final(&data);
}

/*
 * Given array of devices reply with
 *  OK LIST ADDR <addr> ...  LIST_END
 */
void bt_device_list(NSArray* devices, ddata_t* data_out)
{
    NSUInteger i, n;

    if (devices == NULL)
	n = 0;
    else
	n = [devices count];
    /* reply:8 cmdid:32 LIST ADDR <addr> ADDR <addr> LIST_END */
    DEBUGF("bt_device_list n=%d", n);

    ddata_put_tag(data_out, LIST);
    for (i = 0; i < n; i++) {
	DEBUGF("bt_device_list i=%d, n=%d", n);
	IOBluetoothDevice* device = (IOBluetoothDevice*)
	    [devices objectAtIndex:i];
	const BluetoothDeviceAddress* bt_addr = [device getAddress];
	ddata_put_addr(data_out, bt_addr);
	i++;
    }
    ddata_put_tag(data_out, LIST_END);
}

/*
 * Given a IOBluetoothSDPDataElementRef produce a SDP attribute value element
 * Format a binary version of SDP Element
 */
	
int ddata_put_sdp_elem(IOBluetoothSDPDataElement* value, ddata_t* data_out, int level)
{
    BluetoothSDPDataElementTypeDescriptor sdp_type;
    BluetoothSDPDataElementSizeDescriptor sdp_size;
    uint32_t byte_size;

    sdp_type = [value getTypeDescriptor];
    sdp_size = [value getSizeDescriptor];
    byte_size = [value getSize];
    DEBUGF("    %d: sdp:tag=%d,size=%d, avail=%d",
	   level,sdp_type<<3|(sdp_size&7),byte_size,
	   ddata_r_avail(data_out));

    switch(sdp_type) {
    case kBluetoothSDPDataElementTypeNil: /* only a tag byte */
	ddata_put_UINT8(data_out, ((sdp_type<<3)|(sdp_size&0x7)));
	break;
    case kBluetoothSDPDataElementTypeUnsignedInt:
    case kBluetoothSDPDataElementTypeSignedInt: {
	ddata_put_UINT8(data_out, ((sdp_type<<3)|(sdp_size&0x7)));
	switch(sdp_size) {
	case 0: {
	    NSNumber* num = [value getNumberValue];
	    ddata_put_UINT8(data_out, num.unsignedCharValue);
	    break;
	}
	case 1: {
	    NSNumber* num = [value getNumberValue];
	    ddata_put_UINT16(data_out, num.unsignedShortValue); 
	    break;
	}
	case 2: {
	    NSNumber* num = [value getNumberValue];
	    ddata_put_UINT32(data_out, num.unsignedLongValue); 
	    break;
	}
	case 3: {
	    NSNumber* num = [value getNumberValue];
	    ddata_put_UINT64(data_out, num.unsignedLongLongValue); 
	    break;
	}
	case 4: {
	    NSData* data = [value getDataValue];
	    NSUInteger len = [data length];
	    const void* ptr = [data bytes];
	    ddata_add(data_out, (uint8_t*)ptr, len);
	    break;
	}
	default:
	    return -1;
	}
	break;
    }

    case kBluetoothSDPDataElementTypeUUID: {
	IOBluetoothSDPUUID* uuid = [value getUUIDValue];
	NSUInteger len = [uuid length];
	const void* ptr = [uuid bytes];
	ddata_put_UINT8(data_out, ((sdp_type<<3)|(sdp_size&0x7)));
	switch(sdp_size) {
	case 1:
	    ddata_add(data_out, (uint8_t*)ptr, len);
	    break;
	case 2:
	    ddata_add(data_out, (uint8_t*)ptr, len);
	    break;
	case 4:
	    ddata_add(data_out, (uint8_t*)ptr, len);
	    break;
	default:
	    return -1;
	}
	break;
    }

    case kBluetoothSDPDataElementTypeURL:
    case kBluetoothSDPDataElementTypeString: {
	NSString* str = [value getStringValue];
	NSUInteger size = [str lengthOfBytesUsingEncoding:NSUTF8StringEncoding];
	NSUInteger len = [str length];
	NSUInteger ulen;
	NSRange range = NSMakeRange(0, len);
	uint8_t* ptr;

	if (size < 256) {
	    ddata_put_UINT8(data_out, ((sdp_type<<3)|5));
	    ptr = ddata_alloc(data_out, 1+size);
	    DDATA_PUT_UINT8(ptr, size); ptr += 1;
	    [str getBytes:(void*)ptr  maxLength:size
	       usedLength:&ulen encoding:NSUTF8StringEncoding
		  options:0 range:range remainingRange:NULL];
	}
	else if (size < 65536) {
	    ddata_put_UINT8(data_out, ((sdp_type<<3)|6));
	    ptr = ddata_alloc(data_out, 2+size);
	    DDATA_PUT_UINT16(ptr, size); ptr += 2;
	    [str getBytes:(void*)ptr maxLength:size
	     usedLength:&ulen encoding:NSUTF8StringEncoding
	     options:0 range:range remainingRange:NULL];
	}
	else {
	    ddata_put_UINT8(data_out, ((sdp_type<<3)|7));
	    ptr = ddata_alloc(data_out, 4+size);
	    DDATA_PUT_UINT32(ptr, size); ptr += 4;
	    [str getBytes:(void*)ptr maxLength:size
	       usedLength:&ulen encoding:NSUTF8StringEncoding
		  options:0 range:range remainingRange:NULL];
	}
	break;
    }

    case kBluetoothSDPDataElementTypeBoolean: {
	NSNumber* num = [value getNumberValue];
	ddata_put_UINT8(data_out, ((sdp_type<<3)|(sdp_size&0x7)));
	ddata_put_UINT8(data_out, num.unsignedCharValue);
	break;
    }

    case kBluetoothSDPDataElementTypeDataElementSequence:
    case kBluetoothSDPDataElementTypeDataElementAlternative: {
	NSArray* array = [value getArrayValue];
	NSUInteger n = array.count;
	NSUInteger i;
	size_t bin_size;
	size_t used_size;
	intptr_t patch_offset;

	// always use 4 byte header so we can patch 
	ddata_put_UINT8(data_out, ((sdp_type<<3)|(7)));
	patch_offset = ddata_used(data_out);  /* keep offset */
	ddata_put_UINT32(data_out, 0);             /* patch this later */

	used_size = ddata_used(data_out);    // size before
	for (i = 0; i < n; i++) {
	    IOBluetoothSDPDataElement* elem = 
		(IOBluetoothSDPDataElement*) [array objectAtIndex:i];
	    if (ddata_put_sdp_elem(elem, data_out, level+1) < 0)
		return -1;
	}
	bin_size = (ddata_used(data_out) - used_size);  // size after
	DDATA_PUT_UINT32(data_out->base+patch_offset, bin_size);
	break;
    }
    default:
	return -1;
    }
    return 0;
}

/*
 * Ouput a SDP service record
 * LIST ServiceAttributes LIST_END
 */
void ddata_put_sdp_service(IOBluetoothSDPServiceRecord* serv, ddata_t* data_out)
{
    NSDictionary* dict;
    NSNumber* key;

    ddata_put_tag(data_out, LIST);
    dict = [serv attributes];

    for (key in dict) {
	uint16_t aid;
	size_t bin_size;
	size_t used_size;
	intptr_t patch_offset;
	uint8_t type;
	IOBluetoothSDPDataElement* value = [dict objectForKey:key];

	ddata_put_UINT8(data_out, BINARY);
	patch_offset = ddata_used(data_out);  /* keep offset */
	ddata_put_UINT32(data_out, 0);             /* patch this later */
    
	// FIXME: do not assume key is a number?
	aid = key.unsignedShortValue;
	type=((kBluetoothSDPDataElementTypeUnsignedInt << 3) | 1);
	ddata_put_UINT8(data_out, type);
	ddata_put_UINT16(data_out, aid);
	DEBUGF("  type:%d id=%d avail=%d", type, aid, ddata_r_avail(data_out));
	used_size = ddata_used(data_out); // size before
	ddata_put_sdp_elem(value, data_out, 0);
	bin_size = 3 + (ddata_used(data_out) - used_size);  // size after
	DDATA_PUT_UINT32(data_out->base+patch_offset, bin_size);
    }
    ddata_put_tag(data_out, LIST_END);
}

/* read a UUID from data_in
 * if data_in == 0 then all records are returned
 *    data_in == 2  UUID16 is assumed and searched for
 *    data_in == 4  UUID32 is assumed and searched for
 *    data_in == 16 UUID is assumed and searched
 */

int get_uuid(ddata_t* data_in, IOBluetoothSDPUUID** uuid)
{
    switch(ddata_r_avail(data_in)) {
    case 2: {
	BluetoothSDPUUID16 uuid16;
	ddata_get_uint16(data_in, &uuid16);
	*uuid = [IOBluetoothSDPUUID uuid16:uuid16];
	break;
    }
    case 4: {
	BluetoothSDPUUID32 uuid32;
	ddata_get_uint32(data_in, &uuid32);
	*uuid = [IOBluetoothSDPUUID uuid32:uuid32];
	break;
    }
    case 16: {
	*uuid = [IOBluetoothSDPUUID uuidWithBytes:data_in->rd length:16];
	break;
    }
    case 0:
	*uuid = NULL;
	break;
    default:
	return 0;
    }
    return 1;
}


void bt_sdp_info(IOBluetoothDevice* device, IOBluetoothSDPUUID* uuid,
		 ddata_t* data_out)
{
    if (uuid != NULL) {
	IOBluetoothSDPServiceRecord* serv =
	    [device getServiceRecordForUUID:uuid];
	if (serv != NULL)
	    ddata_put_sdp_service(serv, data_out);
	[uuid release];
    }
    else {
	NSArray* services;

	ddata_put_tag(data_out, LIST);

	services = [device services];
	if (services != NULL) {
	    NSUInteger i, n;

	    n = [services count];
	    DEBUGF("bt_sdp_info: %lu services found", n);
	    for (i = 0; i < n; i++) {
		IOBluetoothSDPServiceRecord* serv = [services objectAtIndex:i];
		DEBUGF("bt_sdp_info: service %lu %p", i, serv);
		ddata_put_sdp_service(serv, data_out);
	    }
	    DEBUGF("bt_sdp_info: %lu services done", n);
	}
	ddata_put_tag(data_out, LIST_END);
    }
}

/* construct a NSDictionary that makes up a Bluetooth service entry 
 * the input format MUST be on form
 * LIST
 * BINARY <sdp-attribute> <sdp-value>
 * BINARY <sdp-attribute> <spd-value>
 * BINARY <sdp-attribute> <sdp-value>
 * LIST_END
 * 
 */
NSDictionary* bt_type_dict(int sdp_type)
{
    id keys[1];
    id objects[1];

    keys[0] = [NSString stringWithUTF8String:"DataElementType"];
    objects[0] = [NSNumber numberWithInt:sdp_type];

    return [NSDictionary dictionaryWithObjects:objects forKeys:keys count:1];
}

NSDictionary* bt_data_dict(int sdp_type, void* value)
{
    id keys[2];
    id objects[2];

    keys[0] = [NSString stringWithUTF8String:"DataElementType"];
    objects[0] = [NSNumber numberWithInt:sdp_type];

    keys[1] = [NSString stringWithUTF8String:"DataElementValue"]; 
    objects[1] = value;

    return [NSDictionary dictionaryWithObjects:objects forKeys:keys count:2];
}

NSDictionary* bt_value_dict(int sdp_type, int sdp_size, void* value)
{
    id keys[3];
    id objects[3];

    if (value == NULL)
	return NULL;
    
    keys[0] = [NSString stringWithUTF8String:"DataElementSize"];
    objects[0] = [NSNumber numberWithInt:sdp_size];
    
    keys[1] = [NSString stringWithUTF8String:"DataElementType"];
    objects[1] = [NSNumber numberWithInt:sdp_type];

    keys[2] = [NSString stringWithUTF8String:"DataElementValue"]; 
    objects[2] = value;

    return [NSDictionary dictionaryWithObjects:objects forKeys:keys count:3];
}


int bt_dynamic_len(ddata_t* data_in, int sdp_size, NSUInteger* length)
{
    switch(sdp_size) {
    case 5: {
	uint8_t len;
	if (!ddata_get_uint8(data_in, &len)) return 0;
	*length = (NSUInteger) len;
	return 1;
    }
    case 6: {
	uint16_t len;
	if (!ddata_get_uint16(data_in, &len)) return 0;
	*length = (NSUInteger) len;
	return 1;
    }
    case 7: {
	uint32_t len;
	if (!ddata_get_uint32(data_in, &len)) return 0;
	*length = (NSUInteger) len;
	return 1;
    }
    default:
	return 0;
    }
}

int bt_fixed_len(int sdp_size, NSUInteger* length)
{
    switch(sdp_size) {
    case 0: *length=1; return 1;
    case 1: *length=2; return 1;
    case 2: *length=4; return 1;
    case 3: *length=8; return 1;
    case 4: *length=16; return 1;
    default: return 0;
    }
}


NSObject* bt_get_sdp_value(ddata_t* data_in)
{
    uint8_t  sdp_tag;
    int  sdp_type;
    int  sdp_size;

    ddata_get_uint8(data_in, &sdp_tag);
    sdp_type = (sdp_tag >> 3);
    sdp_size = sdp_tag & 0x7;

    switch(sdp_type) {
    case kBluetoothSDPDataElementTypeNil:
	return bt_type_dict(sdp_type);

    case kBluetoothSDPDataElementTypeUnsignedInt:
    case kBluetoothSDPDataElementTypeSignedInt: {
	void* value;
	switch(sdp_size) {
	case 0: {
	    uint8_t uval;
	    int val;
	    ddata_get_uint8(data_in, &uval);
	    val = uval;
	    value = [NSNumber numberWithInt:val];
	    return bt_value_dict(sdp_type,1,value);
	}
	case 1: {
	    uint16_t uval;
	    int val;
	    if (!ddata_get_uint16(data_in, &uval)) return NULL;
	    val = uval;
	    value = [NSNumber numberWithInt:val];
	    return bt_value_dict(sdp_type,2,value);
	}
	case 2: {
	    uint32_t uval;
	    int val;
	    if (!ddata_get_uint32(data_in, &uval)) return NULL;
	    val = uval;
	    value = [NSNumber numberWithInt:val];
	    if (sdp_type == kBluetoothSDPDataElementTypeUnsignedInt)
		return value;  /* Special case ... */
	    else 
		return bt_value_dict(sdp_type,4,value);
	}

	case 3: {
	    value = [NSData dataWithBytes:data_in->rd length:8];
	    ddata_forward(data_in, 8);
	    return bt_data_dict(sdp_type,value);
	}
	case 4: {
	    value = [NSData dataWithBytes:data_in->rd length:16];
	    ddata_forward(data_in, 16);
	    return bt_data_dict(sdp_type,value);
	}
	default:
	    return NULL;
	}
    }


    case kBluetoothSDPDataElementTypeUUID: {
	NSData* data;
	NSUInteger len;
	if (!bt_fixed_len(sdp_size, &len)) return NULL;
	if ((len == 8) || (len==1)) return NULL;
	data = [NSData dataWithBytes:data_in->rd length:len];
	ddata_forward(data_in, len);
	return (void*) data;
    }

    case kBluetoothSDPDataElementTypeURL:
    case kBluetoothSDPDataElementTypeString: {
	NSUInteger len;
	void* value;

	if (!bt_dynamic_len(data_in, sdp_size, &len)) return NULL;

	value = [[NSString alloc]initWithBytes:(const void*)data_in->rd 
		 length:len
		 encoding:NSUTF8StringEncoding];
	ddata_forward(data_in, len);
	if (sdp_type == kBluetoothSDPDataElementTypeURL)
	    return bt_value_dict(sdp_type, sdp_size, value);
	else
	    return value;
    }

    case kBluetoothSDPDataElementTypeBoolean: {
	uint8_t bval;
	int val;
	void* value;
	
	ddata_get_uint8(data_in, &bval);
	val = bval;
	value = [NSNumber numberWithInt:val];
	return bt_value_dict(sdp_type,1,value);
    }

    case kBluetoothSDPDataElementTypeDataElementSequence:
    case kBluetoothSDPDataElementTypeDataElementAlternative: {
	NSMutableArray* array;
	NSUInteger len;
	uint8_t* end_ptr;

	if (!bt_dynamic_len(data_in, sdp_size, &len)) return NULL;

	end_ptr = data_in->rd + len;
	
	array = [[NSMutableArray alloc] init];
	while(data_in->rd < end_ptr) {
	    id value = bt_get_sdp_value(data_in);
	    if (value == NULL) {
		[array release];
		return NULL;
	    }
	    [array addObject:value];
	}
	if (sdp_type==kBluetoothSDPDataElementTypeDataElementAlternative) 
	    return bt_data_dict(sdp_type, array);
	else
	    return array;
    }

    default:
	return NULL;
    }
}

NSDictionary* bt_make_sdp_dict(ddata_t* data_in)
{
    uint8_t tag;
    NSMutableDictionary* servDict;
    void* value;

    if (!ddata_get_uint8(data_in, &tag) || (tag != LIST))
	return NULL;
    if ((servDict = [[NSMutableDictionary alloc] initWithCapacity:10]) == NULL)
	return NULL;
    
    DEBUGF("Service add: found LIST tag", 0);
    
    while(ddata_get_uint8(data_in, &tag)) {
	if (tag == LIST_END) {
	    DEBUGF("Service add: found LIST_END tag", 0);
	    return servDict;
	}
	else if (tag == BINARY) {
	    uint32_t bin_sz;
	    uint8_t  sdp_tag;
	    uint8_t  sdp_type;
	    uint8_t  sdp_size;
	    uint16_t attributeID;
	    NSString* key;
	    char attr[5];

	    if (!ddata_get_uint32(data_in, &bin_sz)) goto error;
	    DEBUGF("Service add: found BINARY size=%d", bin_sz);
	    if (!ddata_get_uint8(data_in, &sdp_tag)) goto error;
	    sdp_type = (sdp_tag >> 3);
	    sdp_size = sdp_tag & 0x7;
	    if (sdp_type != kBluetoothSDPDataElementTypeUnsignedInt) goto error;
	    if (sdp_size != 1) goto error;
	    if (!ddata_get_uint16(data_in, &attributeID)) goto error;

	    /* Generate the attribute ID as Hex String Key 4 digits */
	    snprintf(attr, 5, "%04X", attributeID);
	    DEBUGF("Service add: found attribute %s", attr);
	    key = [[NSString alloc]initWithBytes:(const void*)attr 
		   length:4 encoding:NSUTF8StringEncoding];
	    value = bt_get_sdp_value(data_in);
	    [servDict setObject:value forKey:key];
	}
	else
	    goto error;
    }

error:
    return NULL;
}

void bt_local_info(ddata_t* data_in, ddata_t* data_out)
{
    uint8_t op_code;

    ddata_put_tag(data_out, LIST);

    while(ddata_get_uint8(data_in, &op_code)) {
	switch(op_code) {
	case NFO_LOCAL_NAME: {
	    NSString* name =
		[[IOBluetoothHostController defaultController] nameAsString];
	    put_ns_string(data_out, name);
	    break;
	}

	case NFO_LOCAL_CLASS: {
	    BluetoothClassOfDevice value =
		[[IOBluetoothHostController defaultController] classOfDevice];
	    ddata_put_uint32(data_out, value);
	    break;
	}

	case NFO_LOCAL_ADDRESS: {
	    BluetoothDeviceAddress addr;
	    NSString* addrStr =
		[[IOBluetoothHostController defaultController] addressAsString];
	    IOBluetoothNSStringToDeviceAddress(addrStr, &addr);
	    ddata_put_addr(data_out, &addr);
	    break;
	}
	    
	case NFO_LOCAL_DISCOVERABLE: {
	    // Boolean value;
	    // if (IOBluetoothLocalDeviceGetDiscoverable(&value) == kIOReturnSuccess)
	    ddata_put_boolean(data_out, true);
	    break;
	}

	case NFO_LOCAL_POWER_STATE: {
	    BluetoothHCIPowerState powerState =
		[[IOBluetoothHostController defaultController] powerState];
	    if (powerState == kBluetoothHCIPowerStateON)
		ddata_put_atom(data_out, "on");
	    else if (powerState == kBluetoothHCIPowerStateOFF)
		ddata_put_atom(data_out, "off");
	    else if (powerState == kBluetoothHCIPowerStateUnintialized)
		ddata_put_atom(data_out, "unintialized");
	    else 
		ddata_put_atom(data_out, "unknown");
	    break;
	}
	default:
	    break;
	}
    }
    ddata_put_tag(data_out, LIST_END);
}


void bt_device_info(IOBluetoothDevice* device, ddata_t* data_in, ddata_t* data_out)
{
    uint8_t op_code;

    ddata_put_tag(data_out, LIST);

    while(ddata_get_uint8(data_in, &op_code)) {
	switch(op_code) {
	case NFO_DEVICE_NAME: {
	    NSString* name = [device name];
	    put_ns_string(data_out, name);
	    break;
	}

	case NFO_DEVICE_CLASS: {
	    BluetoothClassOfDevice value = [device classOfDevice];
	    ddata_put_uint32(data_out, value);
	    break;
	}

	case NFO_DEVICE_CLOCK: {
	    BluetoothClockOffset value = [device getClockOffset];
	    ddata_put_uint16(data_out, value);
	    break;		
	}

	case NFO_DEVICE_IS_FAVORITE: {
	    Boolean value = [device isFavorite];
	    ddata_put_boolean(data_out, value);
	    break;
	    
	}

	case NFO_DEVICE_IS_PAIRED:  {
	    Boolean value = [device isPaired];
	    ddata_put_boolean(data_out, value);
	    break;
	}

	case NFO_DEVICE_IS_CONNECTED:  {
	    Boolean value = [device isConnected];
	    ddata_put_boolean(data_out, value);
	    break;
	}

	case NFO_DEVICE_INQUIRY: {
	    NSDate* date = [device getLastInquiryUpdate];
	    put_date(data_out, date);
	    break;
	}
	case NFO_DEVICE_ACCESS: {
	    NSDate* date = [device recentAccessDate];
	    put_date(data_out, date);
	    break;
	}
	case NFO_DEVICE_UPDATE: {
	    NSDate* date = [device getLastServicesUpdate];
	    put_date(data_out, date);
	    break;
	}
	default:
	    break;
	}
    }
    ddata_put_tag(data_out, LIST_END);

}

// -----------------------------------------------------------------------
//   InquiryDelegate
// -----------------------------------------------------------------------

@implementation InquiryDelegate

- (instancetype)initWithSub:(subscription_t*)s
{
    if (self = [super init]) {
	mSub = s; // weak!
    }
    return self;
}

- (void) dealloc
{
    DEBUGF("InquiryDelegate: dealloc");
    [super dealloc];
}

- (void) deviceInquiryStarted:(IOBluetoothDeviceInquiry*)sender
{
    (void) sender;
    uint8_t buf[64];
    ddata_t data;

    DEBUGF("InquiryDelegate: started");

    ddata_init(&data, buf, sizeof(buf), 0);
    ddata_put_UINT32(&data, 0);
    ddata_put_tag(&data, REPLY_EVENT);
    ddata_put_UINT32(&data, mSub->id);
    ddata_put_atom(&data, "started");

    ddata_send(&data, 1);
    ddata_final(&data);
}

- (void) deviceInquiryDeviceFound:(IOBluetoothDeviceInquiry*)sender 
device:(IOBluetoothDevice*)device
{
    (void) sender;
    const BluetoothDeviceAddress* bt_addr = [device getAddress];
    uint8_t buf[64];
    ddata_t data;

    DEBUGF("InquiryDelegate: device found");

    ddata_init(&data, buf, sizeof(buf), 0);
    ddata_put_UINT32(&data, 0);
    ddata_put_tag(&data, REPLY_EVENT);
    ddata_put_UINT32(&data, mSub->id);
    ddata_put_tag(&data, TUPLE);
    ddata_put_atom(&data, "device");
    ddata_put_addr(&data, bt_addr);
    ddata_put_tag(&data, TUPLE_END);

    ddata_send(&data, 1);
    ddata_final(&data);
}

- (void) deviceInquiryUpdatingDeviceNamesStarted:(IOBluetoothDeviceInquiry*)sender devicesRemaining:(uint32_t)devicesRemaining
{
    (void) sender;
    (void) devicesRemaining;

    DEBUGF("InquiryDelegate: %d name started: devicesRemaining=%d",
	   mSub->id, devicesRemaining);
}


- (void) deviceInquiryDeviceNameUpdated:(IOBluetoothDeviceInquiry*)sender device:(IOBluetoothDevice*)device devicesRemaining:(uint32_t)devicesRemaining
{
    (void) sender;
    (void) device;
    (void) devicesRemaining;

    DEBUGF("InquiryDelegate: %d device name updated: deviceRemaining=%d",
	   mSub->id, devicesRemaining);
}

- (void) deviceInquiryComplete:(IOBluetoothDeviceInquiry*)sender error:(IOReturn)error aborted:(BOOL)aborted
{
    (void) sender;
    (void) error;

    DEBUGF("InquiryDelegate: %d stopped",mSub->id);

    if (!aborted) { /* no need to send event on user abort */
	uint8_t buf[64];
	ddata_t data;

	ddata_init(&data, buf, sizeof(buf), 0);
	ddata_put_UINT32(&data, 0);
	ddata_put_tag(&data, REPLY_EVENT);
	ddata_put_UINT32(&data, mSub->id);
	ddata_put_atom(&data, "stopped");
	ddata_send(&data, 1);
	ddata_final(&data);
    }
}

@end

// -----------------------------------------------------------------------
//   RemoteNameDelegate
// -----------------------------------------------------------------------

@implementation RemoteNameDelegate

- (instancetype)initWithSub:(subscription_t*) s
{
    if (self = [super init]) {
	mSub = s; // weak
    }
    return self;
}

- (void) dealloc
{
    DEBUGF("RemoteNameDelegate dealloc");
    [super dealloc];
}

- (void)remoteNameRequestComplete:(IOBluetoothDevice *)device status:(IOReturn)status
{
    if (status == kIOReturnSuccess) {
	uint8_t buf[265];
	ddata_t data;
	NSString* name = [device name];

	ddata_init(&data, buf, sizeof(buf), 0);
	ddata_put_UINT32(&data, 0);
	
	ddata_put_tag(&data, REPLY_OK);
	ddata_put_UINT32(&data, mSub->cmdid);
	put_ns_string(&data, name);
	ddata_send(&data, 1);
	ddata_final(&data);
    }
    else
	bt_error(mSub->cmdid, status);
    // only for Inquiry objects
    release_subscription(mSub);
}

@end

// -----------------------------------------------------------------------
//   SDPQueryDelegate
// -----------------------------------------------------------------------

@implementation SDPQueryDelegate

- (instancetype)initWithSub:(subscription_t*) s andUUID:(IOBluetoothSDPUUID*) uuid
{
    DEBUGF("SDPQueryDelegate: alloc");
    if (self = [super init]) {
	mSub = s; // weak
	mUUID = [uuid retain];
    }
    return self;
}

- (void) dealloc 
{
    DEBUGF("SDPQueryDelegate: dealloc");
    [mUUID release];
    [super dealloc];
}

- (void)sdpQueryComplete:(IOBluetoothDevice *)device status:(IOReturn)status
{
    uint8_t  out_buf[2048];
    ddata_t data_out;

    if (status == kIOReturnSuccess) {
	ddata_init(&data_out, out_buf, sizeof(out_buf), 0);
	ddata_put_UINT32(&data_out, 0);
	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, mSub->cmdid);
	bt_sdp_info(device, mUUID, &data_out);
	ddata_send(&data_out, 1);
	ddata_final(&data_out);
    }
    else
	bt_error(mSub->cmdid, status);
    // only for Inquiry objects
    release_subscription(mSub);
}
@end

// -----------------------------------------------------------------------
//   Connectelegate
// -----------------------------------------------------------------------

@implementation ConnectDelegate

- (instancetype)initWithSub:(subscription_t*) s
{
    if (self = [super init]) {
	mSub = s; // weak
    }
    return self;
}

- (void) dealloc 
{
    DEBUGF("Connectelegate: dealloc");
    [super dealloc];
}


- (void)connectionComplete:(IOBluetoothDevice *)device status:(IOReturn)status
{
    (void) device;
    if (status == kIOReturnSuccess)
	bt_ok(mSub->cmdid);
    else
	bt_error(mSub->cmdid, status);
    release_subscription(mSub);
}
@end

// -----------------------------------------------------------------------
//   RFCOMMChannelDelegate
// -----------------------------------------------------------------------

@implementation RFCOMMChannelDelegate

- (instancetype)initWithSub:(subscription_t*) s andCtx:(bt_ctx_t*) ctx
{
    if (self = [super init]) {
	mSub = s; // weak
	mCtx = ctx;
    }
    return self;
}

- (void) dealloc
{
    DEBUGF("RFCOMMChannelDelegate: dealloc");
    [super dealloc];
}

- (void)rfcommChannelData:(IOBluetoothRFCOMMChannel*)rfcommChannel data:(void *)dataPointer length:(size_t)dataLength
{
    uint8_t buf[128];
    ddata_t data;
    (void) rfcommChannel;

    DEBUGF("RFCOMMChannelDelegate: Data size=%d", dataLength);
    ddata_init(&data, buf, sizeof(buf), 0);
    ddata_put_UINT32(&data, 0);
    ddata_put_tag(&data, REPLY_EVENT);
    ddata_put_UINT32(&data, mSub->id);
    ddata_put_tag(&data, TUPLE);
    ddata_put_atom(&data, "data");
    ddata_put_binary(&data, dataPointer, dataLength);
    ddata_put_tag(&data, TUPLE_END);
    ddata_send(&data, 1);
    ddata_final(&data);
}

- (void)rfcommChannelOpenComplete:(IOBluetoothRFCOMMChannel*)rfcommChannel status:(IOReturn)error
{
    (void) rfcommChannel;
    (void) error;
    DEBUGF("RFCOMMChannelDelegate: OpenComplete error=%d", error);
    bt_ok(mSub->cmdid);
    mSub->cmdid = 0;
}

- (void)rfcommChannelClosed:(IOBluetoothRFCOMMChannel*)rfcommChannel
{
    (void) rfcommChannel;
    DEBUGF("RFCOMMChannelDelegate: Closed");
    if (mSub->cmdid == 0) {
	uint8_t buf[64];
	ddata_t data;
	ddata_init(&data, buf, sizeof(buf), 0);
	ddata_put_UINT32(&data, 0);
	ddata_put_tag(&data, REPLY_EVENT);
	ddata_put_UINT32(&data, mSub->id);
	ddata_put_atom(&data, "closed");
	ddata_send(&data, 1);
	ddata_final(&data);
    }
    else {
	bt_ok(mSub->cmdid);
	mSub->cmdid = 0;
    }
    remove_subscription(&mCtx->list,RFCOMM,mSub->id);
}

- (void)rfcommChannelControlSignalsChanged:(IOBluetoothRFCOMMChannel*)rfcommChannel
{
    (void) rfcommChannel;
    DEBUGF("RFCOMMChannelDelegate: ControlSignalsChanged");
}

- (void)rfcommChannelFlowControlChanged:(IOBluetoothRFCOMMChannel*)rfcommChannel
{
    if ([rfcommChannel isTransmissionPaused])
	DEBUGF("RFCOMMChannelDelegate: FlowControlChanged OFF");
    else
	DEBUGF("RFCOMMChannelDelegate: FlowControlChanged ON");
}

- (void)rfcommChannelWriteComplete:(IOBluetoothRFCOMMChannel*)rfcommChannel refcon:(void*)refcon status:(IOReturn)error
{
    ddata_t* out = (ddata_t*) refcon;
    uint32_t len;
    (void) error;

    // FIXME: handle error!

    DEBUGF("RFCOMMChannelDelegate: WriteComplete");
    if ((len=ddata_r_avail(out)) == 0) {
	ddata_free(out);
	bt_ok(mSub->cmdid);
	mSub->cmdid = 0;
    }
    else {
	BluetoothRFCOMMMTU mtu = [rfcommChannel getMTU];
	uint8_t* ptr = out->wr;
	IOReturn err;
	if (len > mtu) len = mtu;
	ddata_forward(out, len); /* move to next block */
	err = [rfcommChannel writeAsync:ptr length:len refcon:out];
	if (err != kIOReturnSuccess) {
	    bt_error(mSub->cmdid, err);
	    mSub->cmdid = 0;
	    ddata_free(out);
	}
    }
}

- (void)rfcommChannelQueueSpaceAvailable:(IOBluetoothRFCOMMChannel*)rfcommChannel
{
    (void) rfcommChannel;
    // FIXME: probably a good place to fill output data here ?
    DEBUGF("RFCOMMChannelDelegate: QueueSpaceAvailable");
}

@end

// -----------------------------------------------------------------------
//   L2CAPChannelDelegate
// -----------------------------------------------------------------------

@implementation L2CAPChannelDelegate

- (instancetype)initWithSub:(subscription_t*) s andCtx:(bt_ctx_t*) ctx
{
    if (self = [super init]) {
	mSub = s; // weak
	mCtx = ctx;
    }
    return self;
}

- (void) dealloc
{
    DEBUGF("L2CAPDelegate: dealloc");
    [super dealloc];
}

- (void)l2capChannelData:(IOBluetoothL2CAPChannel*)l2capChannel data:(void *)dataPointer length:(size_t)dataLength
{
    uint8_t buf[128];
    ddata_t data;
    (void) l2capChannel;

    DEBUGF("L2CAPChannelDelegate: Data size=%d", dataLength);
    ddata_init(&data, buf, sizeof(buf), 0);
    ddata_put_UINT32(&data, 0);
    ddata_put_tag(&data, REPLY_EVENT);
    ddata_put_UINT32(&data, mSub->id);
    ddata_put_tag(&data, TUPLE);
    ddata_put_atom(&data, "data");
    ddata_put_binary(&data, dataPointer, dataLength);
    ddata_put_tag(&data, TUPLE_END);
    ddata_send(&data, 1);
    ddata_final(&data);
}

- (void)l2capChannelOpenComplete:(IOBluetoothL2CAPChannel*)l2capChannel status:(IOReturn)error
{
    (void) l2capChannel;
    (void) error;  // fixme check this
    DEBUGF("L2CAPChannelDelegate: OpenComplete");
    bt_ok(mSub->cmdid);
    mSub->cmdid = 0;
}

- (void)l2capChannelClosed:(IOBluetoothL2CAPChannel*)l2capChannel
{
    (void) l2capChannel;
    DEBUGF("L2CAPChannelDelegate: Closed this");
    if (mSub->cmdid == 0) {
	uint8_t buf[64];
	ddata_t data;
	    
	ddata_init(&data, buf, sizeof(buf), 0);
	ddata_put_UINT32(&data, 0);
	ddata_put_tag(&data, REPLY_EVENT);
	ddata_put_UINT32(&data, mSub->id);
	ddata_put_atom(&data, "closed");
	ddata_send(&data, 1);
	ddata_final(&data);
    }
    else {
	bt_ok(mSub->cmdid);
    }
    remove_subscription(&mCtx->list,L2CAP,mSub->id);
}

- (void)l2capChannelReconfigured:(IOBluetoothL2CAPChannel*)l2capChannel
{
    (void) l2capChannel;
    DEBUGF("L2CAPChannelDelegate: Reconfigured");
}

- (void)l2capChannelWriteComplete:(IOBluetoothL2CAPChannel*)l2capChannel refcon:(void*)refcon status:(IOReturn)error
{
    ddata_t* out = (ddata_t*) refcon;
    uint32_t len;
    (void)error;

    DEBUGF("L2CAPChannelDelegate: WriteComplete");
    if ((len=ddata_r_avail(out)) == 0) {
	ddata_free(out);
	bt_ok(mSub->cmdid);
	mSub->cmdid = 0;
    }
    else {
	BluetoothL2CAPMTU mtu = [l2capChannel outgoingMTU];
	uint8_t* ptr = out->wr;
	IOReturn err;
	if (len > mtu) len = mtu;
	ddata_forward(out, len); /* move to next block */
	err = [l2capChannel writeAsync:ptr length:len refcon:out];
	if (err != kIOReturnSuccess) {
	    bt_error(mSub->cmdid, err);
	    mSub->cmdid=0;
	    ddata_free(out);
	}
    }
}

- (void)l2capChannelQueueSpaceAvailable:(IOBluetoothL2CAPChannel*)l2capChannel
{
    (void) l2capChannel;
    DEBUGF("L2CAPChannelDelegate: QueueSpaceAvailable");
}

@end

// -----------------------------------------------------------------------


/* check if we have a listner setup channel event listner */
void rfcomm_accept(subscription_t* listen)
{
    listen_queue_t*  lq = (listen_queue_t*) listen->opaque;
    bt_ctx_t* ctx = lq->ctx;

    if (listen->type != RFCOMM_LISTEN) {
	DEBUGF("RFCOMM: not a listen subscription", 0);
    }
    else if (lq->qh == lq->qt) {
	DEBUGF("RFCOMM: listen queue empty");
    }
    else {
	subscription_link_t* link;
	
	if ((link = lq->wait.last) == NULL) {  // peek
	    DEBUGF("RFCOMM: no acceptor");
	}
	else {
	    RFCOMMChannelDelegate* delegate;
	    subscription_t* s = link->s;

	    delegate = [[RFCOMMChannelDelegate alloc] initWithSub:s andCtx:ctx];

	    /* loop until we find a working channel */
	    while (lq->qh != lq->qt) {
		IOBluetoothRFCOMMChannel* channel = 
		    (IOBluetoothRFCOMMChannel*) lq->qelem[lq->qt];
		IOReturn err;

		lq->qelem[lq->qt] = NULL;
		lq->qt = (lq->qt+1) % LISTEN_QUEUE_LENGTH;

		err = [channel setDelegate:delegate];

		if (err != kIOReturnSuccess) {
		    DEBUGF("RFCOMM: accept error %d:", err);
		    [channel closeChannel];
		}
		else {
		    const BluetoothDeviceAddress* addr;
		    BluetoothRFCOMMChannelID channel_id;
		    IOBluetoothDevice* device;
		    uint8_t buf[64];
		    ddata_t data;
		    
		    if ((device = [channel getDevice]) == NULL) {
			DEBUGF("RFCOMM: accept no device");
			[channel closeChannel];
			[channel release];
			continue;
		    }
		    if ((addr = [device getAddress]) == NULL) {
			DEBUGF("RFCOMM: accept no address");
			[channel closeChannel];
			[channel release];
			continue;
		    }
		    s->handle = channel;
		    s->accept = NULL;
		    channel_id = [channel getChannelID];

		    DEBUGF("RFCOMM: accept on %d", channel_id);

		    unlink_subscription(link);

		    /* send EVENT id {accept,Address,Channel} */
		    ddata_init(&data, buf, sizeof(buf), 0);
		    ddata_put_UINT32(&data, 0);
		    ddata_put_tag(&data, REPLY_EVENT);
		    ddata_put_UINT32(&data, s->id);
		    ddata_put_tag(&data, TUPLE);
		    ddata_put_atom(&data, "accept");
		    ddata_put_addr(&data, addr);
		    ddata_put_uint8(&data, channel_id);
		    ddata_put_tag(&data, TUPLE_END);
		    ddata_send(&data, 1);
		    ddata_final(&data);
		    return;
		}
	    }
	    [delegate release];  // no working channel found
	}
    }
}

// special open callback ! 
void cb_rfcomm_open(void * userRefCon, IOBluetoothUserNotificationRef inRef, IOBluetoothObjectRef objectRef )
{
    IOBluetoothRFCOMMChannel* channel = (IOBluetoothRFCOMMChannel*) objectRef;
    subscription_t* listen = (subscription_t*) userRefCon;
    listen_queue_t* lq = (listen_queue_t*) listen->opaque;
    int qh_next;
    (void) inRef;

    DEBUGF("RFCOMM: OpenNotification", 0);
    qh_next = (lq->qh+1) % LISTEN_QUEUE_LENGTH;
    if (qh_next == lq->qt) {
	DEBUGF("RFCOMM: listen queue full");
	/* If queue is full possibly delete the oldest/newest ? */
	// close it?
	return;
    }
    else {
	lq->qelem[lq->qh] = channel;
	lq->qh = qh_next;
	[channel retain];
	rfcomm_accept(listen);
    }
}

/* check if we have a listner setup channel event listner */
void l2cap_accept(subscription_t* listen)
{
    listen_queue_t*  lq = (listen_queue_t*) listen->opaque;
    bt_ctx_t* ctx = lq->ctx;

    if (listen->type != L2CAP_LISTEN) {
	DEBUGF("L2CAP: not a listen subscription", 0);
    }
    else if (lq->qh == lq->qt) {
	DEBUGF("L2CAP: listen queue empty", 0);
    }
    else {
	subscription_link_t* link;
	
	if ((link = lq->wait.last) == NULL) {  // peek
	    DEBUGF("L2CAP: no acceptor");
	}
	else {
	    L2CAPChannelDelegate* delegate;
	    subscription_t* s = link->s;

	    delegate = [[L2CAPChannelDelegate alloc] initWithSub:s andCtx:ctx];

	    /* loop until we find a working channel */
	    while (lq->qh != lq->qt) {
		IOBluetoothL2CAPChannel* channel = 
		    (IOBluetoothL2CAPChannel*) lq->qelem[lq->qt];
		IOReturn err;
		
		lq->qelem[lq->qt] = NULL;
		lq->qt = (lq->qt+1) % LISTEN_QUEUE_LENGTH;

		err = [channel setDelegate:delegate];

		if (err != kIOReturnSuccess) {
		    DEBUGF("L2CAP: accept error %d:", err);
		    [channel closeChannel];
		    [channel release];
		}
		else {
		    const BluetoothDeviceAddress* addr;
		    BluetoothL2CAPPSM psm;
		    IOBluetoothDevice* device;
		    uint8_t buf[64];
		    ddata_t data;
		
		    if ((device = [channel device]) == NULL) {
			DEBUGF("L2CAP: accept no device", 0);
			[channel closeChannel];
			[channel release];
			continue;
		    }
		    psm = [channel PSM];
		    if ((addr = [device getAddress]) == NULL) {
			DEBUGF("L2CAP: accept no address", 0);
			[channel closeChannel];
			[channel release];
			continue;
		    }
		    DEBUGF("L2CAP: accept psm=%d", psm);
		    s->handle = channel;
		    s->accept = NULL;
		    unlink_subscription(link);

		    /* send EVENT id {accept,Address,Psm} */
		    ddata_init(&data, buf, sizeof(buf), 0);
		    ddata_put_UINT32(&data, 0);
		    ddata_put_tag(&data, REPLY_EVENT);
		    ddata_put_UINT32(&data, s->id);
		    ddata_put_tag(&data, TUPLE);
		    ddata_put_atom(&data, "accept");
		    ddata_put_addr(&data, addr);
		    ddata_put_uint16(&data, psm);
		    ddata_put_tag(&data, TUPLE_END);
		    ddata_send(&data, 1);
		    ddata_final(&data);
		    return;
		}
	    }
	    [delegate release];  // no working channel found
	}
    }
}

void cb_l2cap_open(void * userRefCon, IOBluetoothUserNotificationRef inRef, IOBluetoothObjectRef objectRef )
{
    IOBluetoothL2CAPChannel* l2capChannel = 
	(IOBluetoothL2CAPChannel*) objectRef;
    subscription_t* listen = (subscription_t*) userRefCon;
    listen_queue_t* lq = (listen_queue_t*) listen->opaque;
    int qh_next;
    (void) inRef;

    DEBUGF("L2CAP: OpenNotification", 0);
    qh_next = (lq->qh+1) % LISTEN_QUEUE_LENGTH;
    if (qh_next == lq->qt) {
	DEBUGF("L2CAP: listen queue full", 0);
	/* If queue is full possibly delete the oldest/newest ? */
	// close?
	return;
    }
    else {
	lq->qelem[lq->qh] = l2capChannel;
	lq->qh = qh_next;
	[l2capChannel retain];
	l2cap_accept(listen);
    }
}


void bt_command(bt_ctx_t* ctx, const uint8_t* src, uint32_t src_len)
{
    uint8_t  op = 0;
    uint32_t cmdid = 0;
    IOReturn bt_error = kIOReturnSuccess;
    uint8_t  out_buf[2048];
    ddata_t data_in;
    ddata_t data_out;

    // dump subscription list
    if (dlog_debug_level == DLOG_DEBUG) {
	subscription_link_t* p = ctx->list.first;
	fprintf(stderr, "ctx.list = {");
	while(p) {
	    fprintf(stderr, " %s,", format_subscription(p->s));
	    p = p->next;
	}
	fprintf(stderr, "}\r\n");
    }

    ddata_r_init(&data_in, (uint8_t*)src, src_len, 0);
    ddata_init(&data_out, out_buf, sizeof(out_buf), 0);
    ddata_put_UINT32(&data_out, 0);

    if (!ddata_get_uint8(&data_in, &op))
	goto badarg;
    if (!ddata_get_uint32(&data_in, &cmdid))
	goto badarg;

    switch (op) {
    case CMD_PING: {
	DEBUGF("CMD_PING cmdid=%d", cmdid);
	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	ddata_put_string(&data_out, "pong");
	goto reply;
    }

    case CMD_DEBUG: {  // <<level>>
	int level;
	DEBUGF("CMD_DEBUG cmdid=%d", cmdid);
	if (!ddata_get_int32(&data_in, &level))
	    goto badarg;
	dlog_set_debug(level);
	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	goto reply;
    }	

    case CMD_PAIRED_DEVICES: {
	NSArray* devices = [IOBluetoothDevice pairedDevices];
	DEBUGF("CMD_PAIRED_DEVICE cmdid=%d", cmdid);

	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	bt_device_list(devices, &data_out);
	[devices release];
	goto reply;
    }

    case CMD_FAVORITE_DEVICES: {
	NSArray* devices = [IOBluetoothDevice favoriteDevices];

	DEBUGF("CMD_FAVORITE_DEVICE cmdid=%d", cmdid);

	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	bt_device_list(devices, &data_out);
	[devices release];
	goto reply;
    }

    case CMD_RECENT_DEVICES: {
	NSArray* devices = [IOBluetoothDevice recentDevices:0];

	DEBUGF("CMD_RECENT_DEVICES cmdid=%d", cmdid);
	
	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	bt_device_list(devices, &data_out);
	[devices release];
	goto reply;
    }

    case CMD_INQUIRY_START: { /* arguments: id:32 secs:32 */
	/* FIXME: check that no inquery is running !!! */
	uint32_t sid;
	uint32_t secs;
	InquiryDelegate* delegate;
	IOBluetoothDeviceInquiry* inquiry;
	subscription_t* s;
	
	/* NOTE: The behavior when running multiple inquery is
	 * random (buggy, undefined?) use one at the time as
	 * a work around 
	 */
	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if (!ddata_get_uint32(&data_in, &secs))
	    goto badarg;

	DEBUGF("CMD_INQUIRY_START cmdid=%d id=%u secs=%u", 
	       cmdid, sid, secs);

	if ((s = new_subscription(INQUIRY,sid,0,NULL,cleanup)) == NULL)
	    goto mem_error;

	delegate = [[InquiryDelegate alloc] initWithSub:s];
	inquiry = [[IOBluetoothDeviceInquiry alloc] initWithDelegate:delegate];

	s->handle = inquiry;
	inquiry.inquiryLength = secs;
	inquiry.updateNewDeviceNames = FALSE;
	
	bt_error=[inquiry start];
	if (bt_error == kIOReturnSuccess) {
	    insert_last(&ctx->list, s);
	    goto ok;
	}
	else {
	    release_subscription(s);
	    [inquiry release];
	    [delegate release];
	    goto error;
	}
	break;
    }

    case CMD_INQUIRY_STOP: { /* arguments: id:32 */
	uint32_t sid;
	IOBluetoothDeviceInquiry* inquiry;
	subscription_t* s;

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;

	DEBUGF("CMD_INQUIRY_STOP cmdid=%d, sid=%u", cmdid, sid);

	if ((s = find_subscription(&ctx->list,INQUIRY,sid)) == NULL)
	    goto badarg;
	inquiry = (IOBluetoothDeviceInquiry*)(s->handle);
	[inquiry stop];
	remove_subscription(&ctx->list,INQUIRY,sid);
	goto ok;
    }

    case CMD_CONNECT: { /* arg = bt-address(6) [timeout:32 [auth:8]] */
	BluetoothDeviceAddress bt_addr;
	IOBluetoothDevice* device;
	BluetoothHCIPageTimeout timeout = 0;
	Boolean auth = FALSE;
	Boolean opts = FALSE;
	uint32_t milli;
	ConnectDelegate* delegate;
	subscription_t* s;

	DEBUGF("CMD_CONNECT cmdid=%d", cmdid);

	if(!get_address(&data_in, &bt_addr))
	    goto badarg;
	if (ddata_get_uint32(&data_in, &milli)) {
	    opts = TRUE;
	    if (milli == 0)
		timeout = 0;
	    else {
		uint32_t t;
		if (milli > 40960) 
		    milli = 40960;
		t = ((milli*1600) / 1000);
		timeout = t;
	    }
	    if (!ddata_get_boolean(&data_in, &auth))
		auth = FALSE;
	    if (ddata_r_avail(&data_in) != 0)
		goto badarg;
	}
	if ((s = new_subscription(CONNECT,0,cmdid,NULL,cleanup)) == NULL)
	    goto mem_error;
	if ((device = [IOBluetoothDevice deviceWithAddress:&bt_addr]) == NULL)
	    goto badarg;
	delegate = [[ConnectDelegate alloc] initWithSub:s];
	if (opts)
	    bt_error = [device openConnection:delegate
			withPageTimeout:timeout
			authenticationRequired: auth];
	else
	    bt_error = [device openConnection:delegate];
	if (bt_error != kIOReturnSuccess) {
	    [delegate release];
	    release_subscription(s);
	    goto error;
	}
	/* callback will do the rest */
	break;
    }

    case CMD_DISCONNECT: { /* arg = bt-address(6)  */
	BluetoothDeviceAddress bt_addr;
	IOBluetoothDevice* device;

	DEBUGF("CMD_DISCONNECT cmdid=%d", cmdid);

	if(!get_address(&data_in, &bt_addr))
	    goto badarg;
	if (ddata_r_avail(&data_in) != 0)
	    goto badarg;
	if ((device = [IOBluetoothDevice deviceWithAddress:&bt_addr]) == NULL)
	    goto badarg;
	bt_error = [device closeConnection];
	if (bt_error != kIOReturnSuccess)
	    goto error;
	goto ok;
    }

    case CMD_REMOTE_NAME: { /* arg = bt-address(6) timeout:32 */
	/* FIXME: check that no inquery is running !!! */
	BluetoothDeviceAddress bt_addr;
	IOBluetoothDevice* device;
	Boolean opts = FALSE;
	uint32_t milli;
	BluetoothHCIPageTimeout timeout;
	RemoteNameDelegate* delegate;
	subscription_t* s;

	DEBUGF("CMD_REMOTE_NAME cmdid=%d", cmdid);

	if(!get_address(&data_in, &bt_addr))
	    goto badarg;

	if (ddata_get_uint32(&data_in, &milli)) {
	    opts = TRUE;

	    if (milli == 0)
		timeout = 0;
	    else {
		uint32_t t;
		if (milli > 40960) 
		    milli = 40960;
		t = ((milli*1600) / 1000);
		timeout = t;
	    }
	}

	if (ddata_r_avail(&data_in) != 0)
	    goto badarg;
	if ((s = new_subscription(REMOTE_NAME,0,cmdid,NULL,cleanup)) == NULL)
	    goto mem_error;
	if ((device = [IOBluetoothDevice deviceWithAddress:&bt_addr]) == NULL)
	    goto badarg;
	delegate = [[RemoteNameDelegate alloc] initWithSub:s];
	if (opts && (timeout != 0))
	    bt_error = [device remoteNameRequest:delegate withPageTimeout:timeout];
	else 
	    bt_error = [device remoteNameRequest:delegate];
	if (bt_error != kIOReturnSuccess) {
	    [delegate release];
	    release_subscription(s);
	    goto error;
	}
	/* callback will do the rest */
	break;
    }

    case CMD_DEVICE_INFO: { /* arg = bt-address(6) info-items */
	BluetoothDeviceAddress bt_addr;
	IOBluetoothDevice* device;

	DEBUGF("CMD_DEVICE_INFO cmdid=%d", cmdid);

	if(!get_address(&data_in, &bt_addr))
	    goto badarg;
	if ((device = [IOBluetoothDevice deviceWithAddress:&bt_addr]) == NULL)
	    goto badarg;
	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	bt_device_info(device, &data_in, &data_out);
	goto reply;
    }

    case CMD_LOCAL_INFO: { /* arg = info-items */
	DEBUGF("CMD_LOCAL_INFO cmdid=%d", cmdid);
	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	bt_local_info(&data_in, &data_out);
	goto reply;
    }
       
    case CMD_SERVICE_INFO: { /* addr:48 [uuid:16|uuid:32|uuid:128] */
	BluetoothDeviceAddress bt_addr;
	IOBluetoothDevice* device;
	IOBluetoothSDPUUID* uuid;
	uint32_t n;

	DEBUGF("CMD_SERVICE_INFO cmdid=%d", cmdid);

	if(!get_address(&data_in, &bt_addr))
	    goto badarg;
	if ((device = [IOBluetoothDevice deviceWithAddress:&bt_addr]) == NULL)
	    goto badarg;
	n = ddata_r_avail(&data_in);
	DEBUGF("SERVICE_INFO: avail=%d", n);
	if (get_uuid(&data_in, &uuid)) {
	    ddata_put_tag(&data_out, REPLY_OK);
	    ddata_put_UINT32(&data_out, cmdid);
	    bt_sdp_info(device, uuid, &data_out);
	    goto reply;
	}
	goto badarg;
    }

    case CMD_SERVICE_QUERY: { /* addr:48 [uuid:16|uuid:32|uuid:128]*/
	BluetoothDeviceAddress bt_addr;
	IOBluetoothDevice* device;
	IOBluetoothSDPUUID* uuid;
	SDPQueryDelegate* delegate;
	subscription_t* s;

	DEBUGF("CMD_SERVICE_QUERY cmdid=%d", cmdid);

	if(!get_address(&data_in, &bt_addr))
	    goto badarg;
	if(!get_uuid(&data_in, &uuid))
	    goto badarg;
	if ((device = [IOBluetoothDevice deviceWithAddress:&bt_addr]) == NULL)
	    goto badarg;
	if (ddata_r_avail(&data_in) != 0)
	    goto badarg;

	if ((s = new_subscription(SDP_QUERY,0,cmdid,NULL,cleanup)) == NULL)
	    goto mem_error;
	delegate = [[SDPQueryDelegate alloc] initWithSub:s andUUID:uuid];
	// FIXME: add uuid match
	bt_error = [device performSDPQuery:delegate];
	if (bt_error != kIOReturnSuccess) {
	    [delegate release];
	    release_subscription(s);
	    goto error;
	}
	goto done;
    }

    case CMD_SERVICE_DEL: { /* id:32 */
	uint32_t sid;
	subscription_link_t* link;

	DEBUGF("CMD_SERVICE_DEL cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if (ddata_r_avail(&data_in) != 0)
	    goto badarg;
	DEBUGF("SERVICE_CLOSE: id=%d", sid);
	if ((link=find_subscription_link(&ctx->list,SDP,sid)) != NULL) {
	    unlink_subscription(link);
	    goto ok;
	}
	goto badarg;
    }

    case CMD_SERVICE_ADD: { /* id:32 sdp-service-data/binary */
	uint32_t sid;
	NSDictionary* serviceDict;
	IOBluetoothSDPServiceRecord* serviceRecord;

	DEBUGF("CMD_SERVICE_ADD cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;

	if ((serviceDict = bt_make_sdp_dict(&data_in)) == NULL)
	    goto badarg;
// #ifdef debug
	/* DEBUG print the plist to file for debugging */
	{
	    id plist = serviceDict;       // Assume this property list exists.
	    NSData *xmlData;
	    NSError *error;
	    NSString *path = [NSString stringWithUTF8String:"Service.plist"];
	    xmlData = [NSPropertyListSerialization 
		       dataWithPropertyList:plist
		       format:NSPropertyListXMLFormat_v1_0
		       options:0
		       error:&error];
	    if(xmlData) {
		[xmlData writeToFile:path atomically:YES];
	    }
	    else {
		NSLog(@"xmlData error %@", error);
		[error release];
	    }
	}
// #endif
	// fixme: add "LocalAttributes"
	// "Persistent" boolean()/integer(), 
	// "TargetApplication"
	serviceRecord = [IOBluetoothSDPServiceRecord
			 publishedServiceRecordWithDictionary:serviceDict];
	if (serviceRecord == NULL) {
	    bt_error = kIOReturnNoMemory;
	    goto error;
	}
	else {
	    subscription_t* s;
	    BluetoothSDPServiceRecordHandle serviceRecordHandle;

	    if ((s = new_subscription(SDP,sid,cmdid,serviceRecord,cleanup)) 
		== NULL)
		goto mem_error;
	    if (insert_last(&ctx->list, s) < 0)
		goto mem_error;
	    bt_error = [serviceRecord 
			getServiceRecordHandle:&serviceRecordHandle];
	    if (bt_error != kIOReturnSuccess)
		goto error;
	    ddata_put_tag(&data_out, REPLY_OK);
	    ddata_put_UINT32(&data_out, cmdid);
	    ddata_put_uint32(&data_out, serviceRecordHandle);
	    goto reply;
	}
    }

    case CMD_SERVICE_RFCOMM: { /* id:32 */
	uint32_t sid;
	subscription_t* s;

	DEBUGF("CMD_SERVICE_RFCOMM cmdid=%d", cmdid);
	
	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if (ddata_r_avail(&data_in) != 0)
	    goto badarg;
	if ((s = find_subscription(&ctx->list,SDP,sid)) != NULL) { 
	    BluetoothRFCOMMChannelID channelID;
	    IOBluetoothSDPServiceRecord* serviceRecord = 
		(IOBluetoothSDPServiceRecord*) s->handle;
	    bt_error = [serviceRecord getRFCOMMChannelID:&channelID];
	    if (bt_error == kIOReturnSuccess) {
		ddata_put_tag(&data_out, REPLY_OK);
		ddata_put_UINT32(&data_out, cmdid);
		ddata_put_uint8(&data_out, channelID);
		goto reply;
	    }
	    goto error;
	}
	goto badarg;
    }

    case CMD_RFCOMM_OPEN: { /* id:32 bt-address(6) channel-id:8 */
	uint32_t sid;
	BluetoothDeviceAddress bt_addr;
	IOBluetoothDevice* device;
	IOBluetoothRFCOMMChannel* channel;
	BluetoothRFCOMMChannelID channel_id;
	RFCOMMChannelDelegate* delegate;
	subscription_t* s;

	DEBUGF("CMD_RFCOMM_OPEN cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if(!get_address(&data_in, &bt_addr))
	    goto badarg;
	if(!ddata_get_uint8(&data_in, &channel_id))
	    goto badarg;
	if (!RFCOMM_CHANNEL_ID_IS_VALID(channel_id))
	    goto badarg;
	if (ddata_r_avail(&data_in) != 0)
	    goto badarg;
	if ((device = [IOBluetoothDevice deviceWithAddress:&bt_addr]) == NULL)
	    goto badarg;

	if ((s = new_subscription(RFCOMM,sid,cmdid,NULL,cleanup)) == NULL)
	    goto mem_error;
	delegate = [[RFCOMMChannelDelegate alloc] initWithSub:s andCtx:ctx];
	bt_error = [device openRFCOMMChannelAsync:&channel
				    withChannelID:channel_id 
					 delegate:delegate];
	if (bt_error == kIOReturnSuccess) {
	    s->handle = channel;
	    insert_last(&ctx->list, s);
	    goto done;
	}
	else {
	    [delegate release];
	    release_subscription(s);
	    goto error;
	}
	break;
    }
	
    case CMD_RFCOMM_CLOSE: { /* arguments: id:32 */
	uint32_t sid;
	subscription_link_t* link;

	DEBUGF("CMD_RFCOMM_CLOSE cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if (ddata_r_avail(&data_in) != 0)
	    goto badarg;

	if ((link = find_subscription_link(&ctx->list,RFCOMM,sid)) != NULL) {
	    IOBluetoothRFCOMMChannel* rfcommChannel;
	    subscription_t* s = link->s;
	    s->cmdid = cmdid;
	    rfcommChannel = (IOBluetoothRFCOMMChannel*) (s->handle);
	    if (rfcommChannel != NULL) {
		DEBUGF("RFCOMM_CLOSE: channel=%p", rfcommChannel);	       
		bt_error = [rfcommChannel closeChannel];
		if (bt_error == kIOReturnSuccess)
		    goto done;
	    }
	    else if (s->accept != NULL) {
		listen_queue_t* lq = (listen_queue_t*)((s->accept)->opaque);
		remove_subscription(&lq->wait,RFCOMM,sid);
		unlink_subscription(link);
		goto ok;
	    }
	}
	else if ((link = find_subscription_link(&ctx->list,RFCOMM_LISTEN,sid)) != NULL) {
	    subscription_t* listen = link->s;
	    listen_queue_t* lq = (listen_queue_t*)listen->opaque;
	    subscription_link_t* link1;

	    /* remove all waiters */
	    while((link1=lq->wait.first) != NULL) {
		bt_event(link1->s->id, "closed");
		unlink_subscription(link1);
	    }
	    unlink_subscription(link);
	    goto ok;
	}
	goto error;
    }

    case CMD_RFCOMM_LISTEN: { /* id:32,  channel:8 */
	uint32_t sid;
	BluetoothRFCOMMChannelID channel_id;
	IOBluetoothUserNotificationRef ref;
	subscription_t* listen;
	listen_queue_t* lq;

	DEBUGF("CMD_RFCOMM_LISTEN cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if(!ddata_get_uint8(&data_in, &channel_id))
	    goto badarg;	
	if ((channel_id != 0) && !RFCOMM_CHANNEL_ID_IS_VALID(channel_id))
	    goto badarg;
	if (ddata_r_avail(&data_in) != 0)
	    goto badarg;

	if ((listen = new_subscription(RFCOMM_LISTEN,sid,cmdid,NULL,cleanup))
	    == NULL)
	    goto mem_error;
	if ((lq = alloc_type(listen_queue_t)) == NULL) {
	    release_subscription(listen);
	    goto mem_error;
	}
	lq->ctx = ctx;
	listen->opaque = (void*) lq;

	if (channel_id == 0)
	    ref = IOBluetoothRegisterForRFCOMMChannelOpenNotifications(cb_rfcomm_open, (void*)listen);
	else
	    ref = IOBluetoothRegisterForFilteredRFCOMMChannelOpenNotifications(
		cb_rfcomm_open, (void*) listen, channel_id,
		kIOBluetoothUserNotificationChannelDirectionIncoming);
	if (ref == NULL) {
	    release_subscription(listen);
	    goto mem_error;
	}
	listen->handle = ref;
	insert_last(&ctx->list, listen);
	goto ok;
    }

    case CMD_RFCOMM_ACCEPT: { /* id:32 listen_id:32 */
	uint32_t sid;
	uint32_t listen_id;
	listen_queue_t* lq;
	subscription_t* listen;
	subscription_t* s;

	DEBUGF("CMD_RFCOMM_ACCEPT cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if (!ddata_get_uint32(&data_in, &listen_id))
	    goto badarg;
	if (ddata_r_avail(&data_in) != 0)
	    goto badarg;

	if (find_subscription(&ctx->list,RFCOMM,sid) != NULL) {
	    DEBUGF("subscription %d already exists", sid);
	    goto badarg;
	}
	if ((listen = find_subscription(&ctx->list,
					RFCOMM_LISTEN,listen_id))==NULL) {
	    DEBUGF("listen subscription %d does not exists", listen_id);
	    goto badarg;
	}
	if ((s = new_subscription(RFCOMM,sid,cmdid,NULL,cleanup)) == NULL)
	    goto mem_error;
	s->accept = listen;  // mark that we are accepting
	lq = (listen_queue_t*) listen->opaque;
	insert_last(&lq->wait, s);
	insert_last(&ctx->list, s);

	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	ddata_send(&data_out, 1);
	rfcomm_accept(listen);
	goto done;
    }

    case CMD_RFCOMM_SEND: { /* id:32, data/rest */
	uint32_t sid;
	subscription_t* s;
	IOBluetoothRFCOMMChannel* rfcommChannel;
	uint32_t len;
	ddata_t* out;
	uint8_t* ptr;
	BluetoothRFCOMMMTU mtu;

	DEBUGF("CMD_RFCOMM_SEND cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if ((s = find_subscription(&ctx->list,RFCOMM,sid)) == NULL)
	    goto badarg;
	rfcommChannel = (IOBluetoothRFCOMMChannel*)(s->handle);
	if (rfcommChannel == NULL)
	    goto badarg;
	s->cmdid = cmdid;
	/* we may have to retain the data while sending !!!
	 * create a Data and pace the sending MTU wise...
	 */
	out = ddata_new(data_in.rd, ddata_r_avail(&data_in));
	mtu = [rfcommChannel getMTU];
	if ((len = ddata_r_avail(out)) > mtu)
	    len = mtu;
	ptr = out->wr;
	ddata_forward(out, len); /* move to next block */
	bt_error = [rfcommChannel writeAsync:ptr length:len refcon:out];
	if (bt_error != kIOReturnSuccess)
	    goto error;
	/* event call back will do the rest ? */
	goto done;
    }

    case CMD_RFCOMM_MTU: { /* id:32 */
	uint32_t sid;
	subscription_t* s;
	IOBluetoothRFCOMMChannel* rfcommChannel;
	BluetoothRFCOMMMTU mtu;

	DEBUGF("CMD_RFCOMM_MTU cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if ((s = find_subscription(&ctx->list,RFCOMM,sid)) == NULL)
	    goto badarg;
	rfcommChannel = (IOBluetoothRFCOMMChannel*) (s->handle);
	if (rfcommChannel == NULL)
	    goto badarg;
	mtu = [rfcommChannel getMTU];
	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	ddata_put_uint16(&data_out, mtu);
	goto reply;
    }

    case CMD_RFCOMM_ADDRESS: { /* id:32 */
	uint32_t sid;
	subscription_t* s;
	IOBluetoothRFCOMMChannel* rfcommChannel;
	const BluetoothDeviceAddress* addr;
	IOBluetoothDevice* device;

	DEBUGF("CMD_RFCOMM_ADDRESS cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if ((s = find_subscription(&ctx->list,RFCOMM,sid)) == NULL)
	    goto badarg;
	rfcommChannel = (IOBluetoothRFCOMMChannel*)(s->handle);
	if (rfcommChannel == NULL)
	    goto badarg;
	if ((device = [rfcommChannel getDevice]) == NULL)
	    goto badarg;
	if ((addr = [device getAddress]) == NULL)
	    goto badarg;
	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	ddata_put_addr(&data_out, addr);
	goto reply;
    }

    case CMD_RFCOMM_CHANNEL: { /* id:32 */
	uint32_t sid;
	subscription_t* s;
	IOBluetoothRFCOMMChannel* rfcommChannel;
	BluetoothRFCOMMChannelID channel_id;

	DEBUGF("CMD_RFCOMM_CHANNEL cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if ((s = find_subscription(&ctx->list,RFCOMM,sid)) == NULL)
	    goto badarg;
	rfcommChannel = (IOBluetoothRFCOMMChannel*)(s->handle);
	channel_id = [rfcommChannel getChannelID];
	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	ddata_put_uint8(&data_out, channel_id);
	goto reply;
    }

    case CMD_L2CAP_OPEN: { /* id:32 bt-address(6) psm:16 */
	uint32_t sid;
	BluetoothDeviceAddress bt_addr;
	IOBluetoothDevice* device;
	IOBluetoothL2CAPChannel* channel;
	BluetoothL2CAPPSM psm;
	subscription_t* s;
	L2CAPChannelDelegate* delegate;

	DEBUGF("CMD_L2CAP_OPEN cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if(!get_address(&data_in, &bt_addr))
	    goto badarg;
	if(!ddata_get_uint16(&data_in, &psm))
	    goto badarg;
	if (ddata_r_avail(&data_in) != 0)
	    goto badarg;

	if ((device = [IOBluetoothDevice deviceWithAddress:&bt_addr]) == NULL)
	    goto badarg;

	DEBUGF("L2CAP_OPEN; %d psm=%d", sid, psm);
	
	if ((s = new_subscription(L2CAP,sid,cmdid,NULL,cleanup)) == NULL)
	    goto mem_error;
	delegate = [[L2CAPChannelDelegate alloc] initWithSub:s andCtx:ctx];
	bt_error = [device openL2CAPChannelAsync:&channel
					 withPSM:psm
					delegate:delegate];
	if (bt_error == kIOReturnSuccess) {
	    s->handle = channel;
	    insert_last(&ctx->list, s);
	    goto done;
	}
	else {
	    [delegate release];
	    release_subscription(s);
	    goto error;
	}
	break;
    }

    case CMD_L2CAP_CLOSE: { /* arguments: id:32 */
	uint32_t sid;
	subscription_link_t* link;

	DEBUGF("CMD_L2CAP_CLOSE cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if (ddata_r_avail(&data_in) != 0)
	    goto badarg;
	DEBUGF("L2CAP_CLOSE: id=%d", sid);
	if ((link = find_subscription_link(&ctx->list,L2CAP,sid)) != NULL) {
	    IOBluetoothL2CAPChannel* l2capChannel;
	    subscription_t* s = link->s;
	    s->cmdid = cmdid;
	    l2capChannel = (IOBluetoothL2CAPChannel*)(s->handle);
	    if (l2capChannel != NULL) {
		DEBUGF("L2CAP_CLOSE: channel=%p", l2capChannel);
		bt_error = [l2capChannel closeChannel];
		if (bt_error == kIOReturnSuccess)
		    goto done;
		
	    }
	    else if (s->accept != NULL) {
		listen_queue_t* lq = (listen_queue_t*)(s->accept)->opaque;
		remove_subscription(&lq->wait,L2CAP,sid);
		unlink_subscription(link);
		goto ok;
	    }
	}
	else if ((link = find_subscription_link(&ctx->list,L2CAP_LISTEN,sid))
		 != NULL) {
	    subscription_t* listen = link->s;
	    listen_queue_t* lq = (listen_queue_t*)listen->opaque;
	    subscription_link_t* link1;
	    
	    // move this code to free subscription ?
	    /* remove all waiters */
	    while((link1=lq->wait.first) != NULL) {
		uint8_t buf[64];
		ddata_t data;

		ddata_init(&data, buf, sizeof(buf), 0);
		ddata_put_UINT32(&data, 0);
		ddata_put_tag(&data, REPLY_EVENT);
		ddata_put_UINT32(&data, link1->s->id);
		ddata_put_atom(&data, "closed");
		ddata_send(&data, 1);
		ddata_final(&data);
		unlink_subscription(link1);
	    }
	    unlink_subscription(link);
	    goto ok;
	}
	goto error;
    }

    case  CMD_L2CAP_LISTEN: { /* id:32, psm:16 */
	uint32_t sid;
	BluetoothL2CAPPSM psm;
	IOBluetoothUserNotificationRef ref;
	subscription_t* listen;
	listen_queue_t* lq;

	DEBUGF("CMD_L2CAP_LISTEN cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if(!ddata_get_uint16(&data_in, &psm))
	    goto badarg;
	if (ddata_r_avail(&data_in) != 0)
	    goto badarg;

	if ((listen = new_subscription(L2CAP_LISTEN,sid,cmdid,NULL,cleanup))
	    == NULL)
	    goto mem_error;
	if ((lq = alloc_type(listen_queue_t)) == NULL) {
	    release_subscription(listen);
	    goto mem_error;
	}
	lq->ctx = ctx;
	listen->opaque = (void*) lq;
	if (psm == 0)
	    ref = IOBluetoothRegisterForL2CAPChannelOpenNotifications(cb_l2cap_open, (void*) listen);
	else
	    ref = IOBluetoothRegisterForFilteredL2CAPChannelOpenNotifications(
		cb_l2cap_open, (void*) listen, psm,
		kIOBluetoothUserNotificationChannelDirectionIncoming);
	if (ref == NULL) {
	    release_subscription(listen);
	    goto mem_error;
	}
	listen->handle = ref;
	insert_last(&ctx->list, listen);
	goto ok;
    }

    case  CMD_L2CAP_SEND: { /* id:32, data/rest */
	uint32_t sid;
	subscription_t* s;
	IOBluetoothL2CAPChannel* l2capChannel;
	uint32_t len;
	ddata_t* out;
	uint8_t* ptr;
	BluetoothL2CAPMTU mtu;

	DEBUGF("CMD_L2CAP_SEND cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if ((s = find_subscription(&ctx->list,L2CAP,sid)) == NULL)
	    goto badarg;
	l2capChannel = (IOBluetoothL2CAPChannel*)(s->handle);
	if (l2capChannel == NULL)
	    goto badarg;
	s->cmdid = cmdid;
	/* we may have to retain the data while sending !!!
	 * create a Data and pace the sending MTU wise...
	 */
	out = ddata_new(data_in.rd, ddata_r_avail(&data_in));
	mtu = [l2capChannel outgoingMTU];
	if ((len = ddata_r_avail(out)) > mtu)
	    len = mtu;
	ptr = out->wr;
	ddata_forward(out, len); /* move to next block */
	bt_error = [l2capChannel writeAsync:ptr length:len refcon:out];
	if (bt_error != kIOReturnSuccess)
	    goto error;
	/* event call back will do the rest ? */
	goto done;
    }

    case  CMD_L2CAP_ACCEPT: { /* id:32 listen_id:32 */
	uint32_t sid;
	uint32_t listen_id;
	listen_queue_t* lq;
	subscription_t* listen;
	subscription_t* s;

	DEBUGF("CMD_L2CAP_ACCEPT cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if (!ddata_get_uint32(&data_in, &listen_id))
	    goto badarg;
	if (ddata_r_avail(&data_in) != 0)
	    goto badarg;

	if (find_subscription(&ctx->list,L2CAP,sid) != NULL) {
	    DEBUGF("subscription %d already exists", sid);
	    goto badarg;
	}
	if ((listen = find_subscription(&ctx->list,L2CAP_LISTEN,listen_id))
	    ==NULL) {
	    DEBUGF("listen subscription %d does not exists", listen_id);
	    goto badarg;
	}
	if ((s = new_subscription(L2CAP,sid,cmdid,NULL,cleanup)) == NULL)
	    goto mem_error;
	s->accept = listen;  // mark that we are accepting
	lq = (listen_queue_t*) listen->opaque;
	insert_last(&lq->wait, s);
	insert_last(&ctx->list, s);

	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);	
	ddata_send(&data_out, 1);
	l2cap_accept(listen);
	goto done;
    }

    case  CMD_L2CAP_MTU: {/* id:32 */
	uint32_t sid;
	subscription_t* s;
	IOBluetoothL2CAPChannel* l2capChannel;
	BluetoothL2CAPMTU mtu;

	DEBUGF("CMD_L2CAP_MTU cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if ((s = find_subscription(&ctx->list,L2CAP,sid)) == NULL)
	    goto badarg;
	l2capChannel = (IOBluetoothL2CAPChannel*)(s->handle);
	if (l2capChannel == NULL)
	    goto badarg;
	mtu = [l2capChannel outgoingMTU];
	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	ddata_put_uint16(&data_out, mtu);
	goto reply;
    }

    case  CMD_L2CAP_ADDRESS: { /* id:32 */
	uint32_t sid;
	subscription_t* s;
	IOBluetoothL2CAPChannel* l2capChannel;
	const BluetoothDeviceAddress* addr;
	IOBluetoothDevice* device;

	DEBUGF("CMD_L2CAP_ADDRESS cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if ((s = find_subscription(&ctx->list,L2CAP,sid)) == NULL)
	    goto badarg;
	l2capChannel = (IOBluetoothL2CAPChannel*) (s->handle);
	if (l2capChannel == NULL)
	    goto badarg;
	if ((device = [l2capChannel device]) == NULL)
	    goto badarg;
	if ((addr = [device getAddress]) == NULL)
	    goto badarg;
	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	ddata_put_addr(&data_out, addr);
	goto reply;
    }

    case  CMD_L2CAP_PSM: {/* id:32 */
	uint32_t sid;
	subscription_t* s;
	IOBluetoothL2CAPChannel* l2capChannel;
	BluetoothL2CAPPSM psm;

	DEBUGF("CMD_L2CAP_PSM cmdid=%d", cmdid);

	if (!ddata_get_uint32(&data_in, &sid))
	    goto badarg;
	if ((s = find_subscription(&ctx->list,L2CAP,sid)) == NULL)
	    goto badarg;
	l2capChannel = (IOBluetoothL2CAPChannel*) (s->handle);
	psm = [l2capChannel PSM];
	ddata_put_tag(&data_out, REPLY_OK);
	ddata_put_UINT32(&data_out, cmdid);
	ddata_put_uint16(&data_out, psm);
	goto reply;
    }

    default:
	DEBUGF("CMD_UNKNOWN = %d cmdid=%d", op, cmdid);
	goto badarg;
    }

    goto done;

ok:
    if (cmdid == 0)
	goto done;
    ddata_put_tag(&data_out, REPLY_OK);
    ddata_put_UINT32(&data_out, cmdid);
    goto reply;

mem_error:
    if (cmdid == 0)
	goto done;
    bt_error = kIOReturnNoMemory;
    goto error;

badarg:
    if (cmdid == 0)
	goto done;
    bt_error = kIOReturnBadArgument;

error:
/* reset, just in case something was inserted */
    ddata_reset(&data_out);
    ddata_put_UINT32(&data_out, 0);
    ddata_put_tag(&data_out, REPLY_ERROR);
    ddata_put_UINT32(&data_out, cmdid);
    ddata_put_io_error(&data_out, bt_error, ERR_SHORT);
reply:
    ddata_send(&data_out, 1);
done:
    ddata_final(&data_out);
}

typedef CFSocketNativeHandle PipeNativeHandle;
typedef CFSocketCallBack PipeCallBack;
typedef CFSocketCallBackType PipeCallBackType;
typedef CFSocketContext PipeContext;
typedef CFSocketRef PipeRef;

#define kPipeNoCallBack kCGSocketNoCallBack
#define kPipeReadCallBack kCFSocketReadCallBack
#define kPipeDataCallBack kCFSocketDataCallBack
#define kPipeWriteCallBack kCFSocketWriteCallBack

PipeRef PipeCreateWithNative(CFAllocatorRef allocator, 
			     PipeNativeHandle pipe, 
			     CFOptionFlags callBackTypes, 
			     PipeCallBack callback, const PipeContext *context)
{
    return CFSocketCreateWithNative(allocator, pipe, 
				    callBackTypes,
				    callback,
				    context);
}

CFRunLoopSourceRef PipeCreateRunLoopSource(CFAllocatorRef allocator, 
					   PipeRef pipe, CFIndex order)
{
    return CFSocketCreateRunLoopSource(allocator, pipe, order);
}


/*
 * Handle input commands from Erlang (or other program)
 */

void pipe_callback(PipeRef pipe, PipeCallBackType type, CFDataRef address,
		   const void *data, void *info)
{
    bt_ctx_t* ctx = (bt_ctx_t*) info;
    (void) address;
    (void) data;

    // DEBUGF("PIPE: callback type=%d", type);
    if (type == kPipeReadCallBack) {
	int fd = CFSocketGetNative(pipe);
	int n;

	if (ctx->pbuf_len < sizeof(ctx->pbuf)) {
	    int r = sizeof(ctx->pbuf) - ctx->pbuf_len;
	    if ((n = read(fd, ctx->pbuf+ctx->pbuf_len, r)) < 0)
		goto error;
	    if (n == 0)
		goto closed;
	    ctx->pbuf_len += n;
	    // DEBUGF("READ: %d pbuf_len=%d", n, ctx->pbuf_len);
	    if (ctx->pbuf_len == sizeof(ctx->pbuf)) {
		ctx->len = (ctx->pbuf[0]<<24) + (ctx->pbuf[1]<<16) +
		    (ctx->pbuf[2]<<8) + ctx->pbuf[3];
		// DEBUGF("READ: %d packet len=%d", n, ctx->len);
		if (ctx->len > 0) {
		    ctx->remain = ctx->len;
		    ctx->packet = (uint8_t*) malloc(ctx->len);
		    ctx->ptr = ctx->packet;
		}
		else {
		    ctx->remain = 0;
		    ctx->pbuf_len = 0;
		}
	    }
	}
	else {
	    if ((n = read(fd, (void*)ctx->ptr, ctx->remain)) < 0)
		goto error;
	    if (n == 0)
		goto closed;
	    // DEBUGF("READ: %d packet bytes", n);
	    ctx->remain -= n;
	    // DEBUGF("PACKET: remain=%d", ctx->remain);
	    ctx->ptr += n;
	    if (ctx->remain == 0) {
		bt_command(ctx, ctx->packet, ctx->len);
		free(ctx->packet);
		ctx->packet = NULL;
		ctx->len = 0;
		ctx->pbuf_len = 0;
	    }
	}
    }
    // DEBUGF("PIPE: callback done",0);
    return;

error:
    DEBUGF("pipe read error",0);
    exit(1);

closed:
    DEBUGF("eof clean-up",0);
    CFRunLoopStop(CFRunLoopGetCurrent());
}



int main(int argc, char** argv)
{
    PipeRef pipe_in;
    PipeRef pipe_out;
    PipeNativeHandle in_fd = 0;
    PipeNativeHandle out_fd = 1;
    CFRunLoopSourceRef pipe_ref;
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    (void) argc;
    (void) argv;
// The global context (fixme - remove this)
    bt_ctx_t bt_info;
    PipeContext in_ctx;
    PipeContext out_ctx;

    dlog_init();

    memset(&bt_info, 0, sizeof(bt_ctx_t));
    memset(&in_ctx, 0, sizeof(PipeContext));
    memset(&out_ctx, 0, sizeof(PipeContext));

    in_ctx.info = &bt_info;
    out_ctx.info = &bt_info;

    // kCFSocketWriteCallBack

    pipe_in = PipeCreateWithNative(0, in_fd, 
				   kCFSocketReadCallBack,
				   pipe_callback,
				   &in_ctx);

    pipe_out = PipeCreateWithNative(0, out_fd, 
				    kCFSocketWriteCallBack,
				    pipe_callback,
				    &out_ctx);

    pipe_ref = PipeCreateRunLoopSource(0, pipe_in, 0);

    CFRunLoopAddSource(CFRunLoopGetCurrent(), pipe_ref, kCFRunLoopDefaultMode);

    CFRunLoopRun();

    [pool release];

    dlog_finish();
    DEBUGF("terminate",0);
    exit(0);
}
