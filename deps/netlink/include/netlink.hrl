
-ifndef(__NETLINK_HRL__).
-define(__NETLINK_HRL__, true).


-record(rtnl_link_stats, {
	rx_packets,		%% total packets received	
	tx_packets,		%% total packets transmitted	
	rx_bytes,		%% total bytes received 	
	tx_bytes,		%% total bytes transmitted	
	rx_errors,		%% bad packets received		
	tx_errors,		%% packet transmit problems	
	rx_dropped,		%% no space in linux buffers	
	tx_dropped,		%% no space available in linux	
	multicast,		%% multicast packets received	
	collisions,

	%% detailed rx_errors: 
	rx_length_errors,
	rx_over_errors,		%% receiver ring buff overflow	
	rx_crc_errors,		%% recved pkt with crc error	
	rx_frame_errors,	%% recv'd frame alignment error 
	rx_fifo_errors,		%% recv'r fifo overrun		
	rx_missed_errors,	%% receiver missed packet	

	%% detailed tx_errors 
	tx_aborted_errors,
	tx_carrier_errors,
	tx_fifo_errors,
	tx_heartbeat_errors,
	tx_window_errors,

	%% for cslip etc 
	rx_compressed,
	tx_compressed
}).

-endif.
