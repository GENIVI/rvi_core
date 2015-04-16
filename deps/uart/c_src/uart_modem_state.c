//
// Modem state utils
//

#include "uart_drv.h"

// Build a list of modem state flags
int modem_state_dterm(dterm_t* t, uart_modem_state_t state)
{
    dterm_mark_t m;

    dterm_list_begin(t, &m);
    if (state & UART_DTR) dterm_atom(t, am_dtr);
    if (state & UART_RTS) dterm_atom(t, am_rts);
    if (state & UART_CTS) dterm_atom(t, am_cts);
    if (state & UART_CD)  dterm_atom(t, am_cd);
    if (state & UART_RI)  dterm_atom(t, am_ri);
    if (state & UART_DSR) dterm_atom(t, am_dsr);
    // SW is not really in modem state but used with flow control
    if (state & UART_SW)  dterm_atom(t, am_sw);
    dterm_list_end(t, &m);
    return dterm_used_size(t);
}

void modem_state_dump(FILE* f, uart_modem_state_t state)
{
    fprintf(f, "modem state:");
    if (state & UART_DTR) fprintf(f," DTR");
    if (state & UART_RTS) fprintf(f," RTS");
    if (state & UART_CTS) fprintf(f," CTS");
    if (state & UART_CD)  fprintf(f," CD");
    if (state & UART_RI)  fprintf(f," RI");
    if (state & UART_DSR) fprintf(f," DSR");
    if (state & UART_SW)  fprintf(f," SW");
    fprintf(f, "\r\n");
}
