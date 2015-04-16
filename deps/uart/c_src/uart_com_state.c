//
// com_state utils
//
#include <stdio.h>

#include "uart_drv.h"

void com_state_dump(FILE* f, uart_com_state_t* state)
{
    fprintf(f, "com { ");
    fprintf(f, "ibaud: %d,", state->ibaud);
    fprintf(f, "obaud: %d,", state->obaud);
    fprintf(f, "parity: %d,", state->parity);
    fprintf(f, "stopb: %d,", state->stopb);
    fprintf(f, "csize: %d,", state->csize);
    fprintf(f, "bufsz: %d,", state->bufsz);
    fprintf(f, "buftm: %d,", state->buftm);
    fprintf(f, "xonchar: %d,", state->xonchar);
    fprintf(f, "xoffchar: %d,", state->xoffchar);
    fprintf(f, "iflow: %d,", state->iflow);
    fprintf(f, "oflow: %d,", state->oflow);
    fprintf(f, "}\r\n");
}

void com_state_init(uart_com_state_t* ptr)
{
    ptr->ibaud    = 9600;
    ptr->obaud    = 9600;
    ptr->parity   = 0;
    ptr->stopb    = 1;
    ptr->csize    = 8;
    ptr->bufsz    = 1;
    ptr->buftm    = 0;
    ptr->xonchar  = 17;
    ptr->xoffchar = 19;
    ptr->iflow   = 0;
    ptr->oflow   = 0;
}

// Copy changed com state configs from source to destination
void com_state_copy(uart_com_state_t* dst,uart_com_state_t* src,
		    uint32_t sflags)
{
    // update the state1 
    if (sflags & (1<<UART_OPT_IBAUD)) dst->ibaud = src->ibaud;
    if (sflags & (1<<UART_OPT_OBAUD)) dst->obaud = src->obaud;
    if (sflags & (1<<UART_OPT_CSIZE)) dst->csize = src->csize;
    if (sflags & (1<<UART_OPT_BUFSZ)) dst->bufsz = src->bufsz;
    if (sflags & (1<<UART_OPT_BUFTM)) dst->buftm = src->buftm;
    if (sflags & (1<<UART_OPT_STOPB)) dst->stopb = src->stopb;
    if (sflags & (1<<UART_OPT_PARITY)) dst->parity = src->parity;
    if (sflags & (1<<UART_OPT_IFLOW)) dst->iflow = src->iflow; 
    if (sflags & (1<<UART_OPT_OFLOW)) dst->oflow = src->oflow; 
    if (sflags & (1<<UART_OPT_XOFFCHAR)) dst->xoffchar = src->xoffchar; 
    if (sflags & (1<<UART_OPT_XONCHAR)) dst->xonchar = src->xonchar; 
}
