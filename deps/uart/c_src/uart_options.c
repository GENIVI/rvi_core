//
// Option processing
//
#include "uart_drv.h"

// encode options [{opt,value}] into dterm
int uart_get_opts(dterm_t* t, uart_ctx_t* ctx, uint8_t* ptr, size_t len)
{
    uint8_t* ptr_end = ptr + len;
    dterm_mark_t m;

    dterm_list_begin(t, &m);

    while(ptr < ptr_end) {
	uint8_t opt = *ptr++;
	switch(opt) {
	case UART_OPT_DEVICE:
	    dterm_kv_string(t,am_device,ctx->option.device_name); 
	    break;
	case UART_OPT_IBAUD: 
	    dterm_kv_uint(t,am_ibaud,ctx->state.ibaud);
	    break;
	case UART_OPT_OBAUD: 
	    dterm_kv_uint(t,am_obaud,ctx->state.obaud);  
	    break;
	case UART_OPT_CSIZE: 
	    dterm_kv_uint(t,am_csize, ctx->state.csize);  
	    break;
	case UART_OPT_BUFSZ: 
	    dterm_kv_uint(t,am_bufsz, ctx->state.bufsz); 
	    break;
	case UART_OPT_BUFTM: 
	    dterm_kv_uint(t,am_buftm, ctx->state.buftm);  
	    break;
	case UART_OPT_STOPB: 
	    dterm_kv_uint(t,am_stopb, ctx->state.stopb);  
	    break;
	case UART_OPT_PARITY:
	    switch(ctx->state.parity) {
	    case UART_PARITY_NONE: dterm_kv_atom(t,am_parity, am_none); break;
	    case UART_PARITY_ODD:  dterm_kv_atom(t,am_parity, am_odd); break;
	    case UART_PARITY_EVEN: dterm_kv_atom(t,am_parity, am_even); break;
	    case UART_PARITY_MARK: dterm_kv_atom(t,am_parity, am_mark); break;
	    case UART_PARITY_SPACE: dterm_kv_atom(t,am_parity, am_space); break;
	    default: dterm_kv_uint(t,am_parity,ctx->state.parity); break;
	    }
	    break;

	case UART_OPT_IFLOW: {
	    dterm_mark_t m2;
	    dterm_tuple_begin(t, &m2); {
		dterm_atom(t, am_iflow);
		modem_state_dterm(t, ctx->state.iflow);
	    }
	    dterm_tuple_end(t, &m2);
	    break;
	}

	case UART_OPT_OFLOW: {
	    dterm_mark_t m2;
	    dterm_tuple_begin(t, &m2); {
		dterm_atom(t, am_oflow);
		modem_state_dterm(t, ctx->state.oflow);
	    }
	    dterm_tuple_end(t, &m2);
	    break;
	}

	case UART_OPT_XOFFCHAR:
	    dterm_kv_uint(t,am_xoffchar, ctx->state.xoffchar);
	    break;

	case UART_OPT_XONCHAR: 
	    dterm_kv_uint(t,am_xonchar, ctx->state.xonchar);
	    break;

	case UART_OPT_EOLCHAR: 
	    dterm_kv_uint(t,am_eolchar, ctx->option.eolchar); 
	    break;

	case UART_OPT_ACTIVE:
	    switch(ctx->option.active) {
	    case UART_PASSIVE: dterm_kv_atom(t,am_active, am_false); break;
	    case UART_ACTIVE:  dterm_kv_atom(t,am_active, am_true); break;
	    case UART_ONCE:    dterm_kv_atom(t,am_active, am_once); break;
	    default:  dterm_kv_int(t,am_active, ctx->option.active); break;
	    }
	    break;

	case UART_OPT_DELAY_SEND:
	    dterm_kv_bool(t,am_delay_send, ctx->option.delay_send);
	    break;

	case UART_OPT_DELIVER:
	    switch (ctx->option.deliver) {
	    case UART_DELIVER_PORT: dterm_kv_atom(t,am_deliver, am_port); break;
	    case UART_DELIVER_TERM: dterm_kv_atom(t,am_deliver, am_term); break;
	    default: dterm_kv_int(t,am_deliver,ctx->option.deliver); break;
	    }
	    break;

	case UART_OPT_MODE: 
	    switch (ctx->option.mode) {
	    case UART_MODE_LIST: dterm_kv_atom(t,am_mode, am_list); break;
	    case UART_MODE_BINARY: dterm_kv_atom(t,am_mode, am_binary); break;
	    default: dterm_kv_int(t,am_mode,ctx->option.mode); break;
	    }
	    break;

	case UART_OPT_HEADER: 
	    dterm_kv_uint(t,am_header,ctx->option.hsz);
	    break;

	case UART_OPT_PACKET: 
	    switch(ctx->option.htype & UART_PB_TYPE_MASK) {
	    case UART_PB_RAW: {
		uint16_t fsz;
		if ((fsz=((ctx->option.htype & UART_PB_FIXED_MASK)>>16)) == 0) {
		    dterm_kv_uint(t,am_packet, 0);
		}
		else {
		    dterm_mark_t m2,m3;
		    dterm_tuple_begin(t, &m2); {
			dterm_atom(t, am_packet);
			dterm_tuple_begin(t, &m3); {
			    dterm_atom(t, am_size);
			    dterm_uint(t, fsz);
			}
			dterm_tuple_end(t, &m3);
		    }
		    dterm_tuple_end(t, &m2);
		}
		break;
	    }
	    case UART_PB_N: {
		int pb = ((ctx->option.htype & UART_PB_BYTES_MASK)>>8);
		if (ctx->option.htype & UART_PB_LITTLE_ENDIAN)
		    dterm_kv_int(t,am_packet, -pb);
		else
		    dterm_kv_int(t,am_packet, pb);
		break;
	    }
	    case UART_PB_LINE_LF:
		dterm_kv_atom(t,am_packet, am_line); 
		break;
	    case UART_PB_BASIC_0710:
		dterm_kv_atom(t,am_packet, am_basic_0710); 
		break;
	    case UART_PB_ADVANCED_0710:
		dterm_kv_atom(t,am_packet, am_advanced_0710); 
		break;
	    case UART_PB_GSM_0710:
		dterm_kv_atom(t,am_packet, am_gsm_0710);
		break;
	    default:
		dterm_kv_uint(t,am_packet, ctx->option.htype);
		break;
	    }
	    break;

	case UART_OPT_PSIZE:
	    dterm_kv_uint(t,am_packet_size, ctx->option.psize); 
	    break;
	case UART_OPT_HIGH:
	    dterm_kv_uint(t,am_high_watermark, ctx->option.high); 
	    break;	    
	case UART_OPT_LOW: 
	    dterm_kv_uint(t,am_low_watermark, ctx->option.low); 
	    break;
	case UART_OPT_SENDTMO:
	    dterm_kv_uint(t,am_send_timeout, ctx->option.send_timeout);
	    break;
	case UART_OPT_CLOSETMO:
	    dterm_kv_uint(t,am_send_timeout_close, 
			  ctx->option.send_timeout_close); 
	    break;
	case UART_OPT_PTYPKT:
	    dterm_kv_bool(t,am_ptypkt, ctx->option.ptypkt); 
	    break;
	case UART_OPT_BUFFER: 
	    dterm_kv_uint(t,am_buffer, ctx->option.bsize);
	    break;
	case UART_OPT_EXITF:
	    dterm_kv_bool(t,am_exit_on_close, ctx->option.exitf);
	    break;
	case UART_OPT_DEBUG:
	    dterm_kv_int(t,am_debug, dlog_debug_level);
	    break;
	default:
	    break;
	}
    }
    dterm_list_end(t, &m);
    return dterm_used_size(t);
}

int uart_parse_opts(char* buf, ErlDrvSizeT len,
		    uart_com_state_t* state, uart_opt_t* option,
		    uint32_t* sflags)
{
    unsigned char*   ptr = (unsigned char*) buf;
    unsigned char*   ptr_end = ptr + len;

#define GET_UINT32(v) do { \
	if ((ptr_end - ptr) < 4) return -1; \
	v = (int) get_uint32(ptr); \
	DEBUGF("option %s value=0x%08x", #v, v); \
	ptr += 4; \
    } while(0)

    DEBUGF("set_opts: called");

    // process updates 
    while(ptr < ptr_end) {
	DEBUGF("set_opts: opt=%u", ptr[0]);
	*sflags |= (1 << ptr[0]);

	switch(*ptr++) {
	case UART_OPT_DEVICE: {
	    int n;
	    if ((ptr_end - ptr) < 1) return -1;
	    n = get_uint8(ptr);
	    ptr++;
	    if ((ptr_end - ptr) < n) return -1;
	    memcpy(option->device_name, ptr, n);
	    option->device_name[n] = 0;
	    DEBUGF("set_opts: device_name = %s", option->device_name);
	    ptr += n;
	    break;
	}
	case UART_OPT_DEBUG: {
	    int level;
	    if ((ptr_end - ptr) < 4) 
		return -1;
	    level = (int32_t) get_uint32(ptr);
	    DEBUGF("option debug value=0x%08x", level);
	    dlog_set_debug(level);
	    ptr += 4;
	    break;
	}
	case UART_OPT_IBAUD:    GET_UINT32(state->ibaud); break;
	case UART_OPT_OBAUD:    GET_UINT32(state->obaud); break;
	case UART_OPT_CSIZE:    GET_UINT32(state->csize); break;
	case UART_OPT_BUFSZ:    GET_UINT32(state->bufsz); break;
	case UART_OPT_BUFTM:    GET_UINT32(state->buftm); break;
	case UART_OPT_STOPB:    GET_UINT32(state->stopb); break;
	case UART_OPT_PARITY:   GET_UINT32(state->parity); break;
	case UART_OPT_IFLOW:    GET_UINT32(state->iflow); break;
	case UART_OPT_OFLOW:    GET_UINT32(state->oflow); break;
	case UART_OPT_XOFFCHAR: GET_UINT32(state->xoffchar); break;
	case UART_OPT_XONCHAR:  GET_UINT32(state->xonchar); break;
	case UART_OPT_EOLCHAR:  GET_UINT32(option->eolchar); break;
	case UART_OPT_ACTIVE:   GET_UINT32(option->active); break;
	case UART_OPT_DELAY_SEND: GET_UINT32(option->delay_send); break;
	case UART_OPT_DELIVER:  GET_UINT32(option->deliver); break;
	case UART_OPT_MODE:     GET_UINT32(option->mode); break;
	case UART_OPT_HEADER:   GET_UINT32(option->hsz); break;
	case UART_OPT_PACKET:   GET_UINT32(option->htype); break;
	case UART_OPT_PSIZE:    GET_UINT32(option->psize); break;
	case UART_OPT_HIGH:     GET_UINT32(option->high);  break;
	case UART_OPT_LOW:      GET_UINT32(option->low);   break;
	case UART_OPT_SENDTMO:  GET_UINT32(option->send_timeout); break;
	case UART_OPT_CLOSETMO: GET_UINT32(option->send_timeout_close); break;
	case UART_OPT_BUFFER:   GET_UINT32(option->bsize); break;
	case UART_OPT_EXITF:    GET_UINT32(option->exitf); break;
	case UART_OPT_PTYPKT:   GET_UINT32(option->ptypkt); break;
	default:
	    return -1;
	}
    }
    return 0;
#undef GET_UINT32
}
