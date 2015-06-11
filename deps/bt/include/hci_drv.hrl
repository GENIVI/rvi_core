
-ifndef(__HCI_DRV_HRL__).
-define(__HCI_DRV_HRL__, true).

-type bdaddr_t() :: binary().  %% 6 bytes
-type uint8_t() :: 0..16#ff.
-type uint16_t() :: 0..16#ffff.
-type uint32_t() :: 0..16#ffffffff.
-type uint64_t() :: 0..16#ffffffffffffffff.

%% HCI Packet types
-define(HCI_COMMAND_PKT,	16#01).
-define(HCI_ACLDATA_PKT,	16#02).
-define(HCI_SCODATA_PKT,	16#03).
-define(HCI_EVENT_PKT,		16#04).
-define(HCI_VENDOR_PKT,		16#ff).

%% Command opcode pack/unpack
-define(cmd_opcode_pack(OGF, OCF), (((OCF) band 16#03ff)bor((OGF) bsl 10))).
-define(cmd_opcode_ogf(OP),	((OP) bsr 10)).
-define(cmd_opcode_ocf(OP),	((OP) band 16#03ff)).

%% ACL handle and flags pack/unpack
-define(acl_handle_pack(H, F),	(((H) band 16#0fff) bor ((F) bsl 12))).
-define(acl_handle(H),		((H) band 16#0fff)).
-define(acl_flags(H),		((H) bsr 12)).

-record(hci_dev_stats,
	{
	  err_rx::uint32_t(),
	  err_tx::uint32_t(),
	  cmd_tx::uint32_t(),
	  evt_rx::uint32_t(),
	  acl_tx::uint32_t(),
	  acl_rx::uint32_t(),
	  sco_tx::uint32_t(),
	  sco_rx::uint32_t(),
	  byte_rx::uint32_t(),
	  byte_tx::uint32_t()
	}).

%% HCI device flags
-define(HCI_UP, 0).
-define(HCI_INIT, 1).
-define(HCI_RUNNING, 2).
-define(HCI_PSCAN, 3).
-define(HCI_ISCAN, 4).
-define(HCI_AUTH, 5).
-define(HCI_ENCRYPT, 6).
-define(HCI_INQUIRY, 7).
-define(HCI_RAW, 8).

%% LE address type
-define(LE_PUBLIC_ADDRESS, 16#00).
-define(LE_RANDOM_ADDRESS, 16#01).

%% HCI Packet types
-define(HCI_2DH1,	16#0002).
-define(HCI_3DH1,	16#0004).
-define(HCI_DM1,	16#0008).
-define(HCI_DH1,	16#0010).
-define(HCI_2DH3,	16#0100).
-define(HCI_3DH3,	16#0200).
-define(HCI_DM3,	16#0400).
-define(HCI_DH3,	16#0800).
-define(HCI_2DH5,	16#1000).
-define(HCI_3DH5,	16#2000).
-define(HCI_DM5,	16#4000).
-define(HCI_DH5,	16#8000).

-define(HCI_HV1,	16#0020).
-define(HCI_HV2,	16#0040).
-define(HCI_HV3,	16#0080).

-define(HCI_EV3,	16#0008).
-define(HCI_EV4,	16#0010).
-define(HCI_EV5,	16#0020).
-define(HCI_2EV3,	16#0040).
-define(HCI_3EV3,	16#0080).
-define(HCI_2EV5,	16#0100).
-define(HCI_3EV5,	16#0200).

-define(SCO_PTYPE_MASK,	(?HCI_HV1 bor ?HCI_HV2 bor ?HCI_HV3)).
-define(ACL_PTYPE_MASK,	(?HCI_DM1 bor ?HCI_DH1 bor ?HCI_DM3 bor ?HCI_DH3 bor ?HCI_DM5 bor ?HCI_DH5)).

%% HCI controller types
-define(HCI_BREDR,	16#00).
-define(HCI_AMP,	16#01).


%% HCI bus types
-define(HCI_VIRTUAL,	0).
-define(HCI_USB,	1).
-define(HCI_PCCARD,	2).
-define(HCI_UART,	3).
-define(HCI_RS232,	4).
-define(HCI_PCI,	5).
-define(HCI_SDIO,	6).

%% Link policies
-define(HCI_LP_RSWITCH,	16#0001).
-define(HCI_LP_HOLD,	16#0002).
-define(HCI_LP_SNIFF,	16#0004).
-define(HCI_LP_PARK,	16#0008).

%% Link mode
-define(HCI_LM_ACCEPT,	 16#8000).
-define(HCI_LM_MASTER,	 16#0001).
-define(HCI_LM_AUTH,	 16#0002).
-define(HCI_LM_ENCRYPT,	 16#0004).
-define(HCI_LM_TRUSTED,	 16#0008).
-define(HCI_LM_RELIABLE, 16#0010).
-define(HCI_LM_SECURE,	 16#0020).

%% Link Key types
-define(HCI_LK_COMBINATION,	    16#00).
-define(HCI_LK_LOCAL_UNIT,	    16#01).
-define(HCI_LK_REMOTE_UNIT,	    16#02).
-define(HCI_LK_DEBUG_COMBINATION,   16#03).
-define(HCI_LK_UNAUTH_COMBINATION,  16#04).
-define(HCI_LK_AUTH_COMBINATION,    16#05).
-define(HCI_LK_CHANGED_COMBINATION, 16#06).
-define(HCI_LK_INVALID,		    16#FF).


%% ACL flags
-define(ACL_START_NO_FLUSH,	16#00).
-define(ACL_CONT,		16#01).
-define(ACL_START,		16#02).
-define(ACL_ACTIVE_BCAST,	16#04).
-define(ACL_PICO_BCAST,		16#08).

%% Baseband links
-define(SCO_LINK,	16#00).
-define(ACL_LINK,	16#01).
-define(ESCO_LINK,	16#02).

%% LMP features
-define(LMP_3SLOT,	16#01).
-define(LMP_5SLOT,	16#02).
-define(LMP_ENCRYPT,	16#04).
-define(LMP_SOFFSET,	16#08).
-define(LMP_TACCURACY,	16#10).
-define(LMP_RSWITCH,	16#20).
-define(LMP_HOLD,	16#40).
-define(LMP_SNIFF,	16#80).

-define(LMP_PARK,	 16#01).
-define(LMP_RSSI,	 16#02).
-define(LMP_QUALITY,	 16#04).
-define(LMP_SCO,	 16#08).
-define(LMP_HV2,	 16#10).
-define(LMP_HV3,	 16#20).
-define(LMP_ULAW,	 16#40).
-define(LMP_ALAW,	 16#80).

-define(LMP_CVSD,	 16#01).
-define(LMP_PSCHEME,	 16#02).
-define(LMP_PCONTROL,	 16#04).
-define(LMP_TRSP_SCO,	 16#08).
-define(LMP_BCAST_ENC,	 16#80).

-define(LMP_EDR_ACL_2M,	 16#02).
-define(LMP_EDR_ACL_3M,	 16#04).
-define(LMP_ENH_ISCAN,	 16#08).
-define(LMP_ILACE_ISCAN, 16#10).
-define(LMP_ILACE_PSCAN, 16#20).
-define(LMP_RSSI_INQ,	 16#40).
-define(LMP_ESCO,	 16#80).

-define(LMP_EV4,	 16#01).
-define(LMP_EV5,	 16#02).
-define(LMP_AFH_CAP_SLV, 16#08).
-define(LMP_AFH_CLS_SLV, 16#10).
-define(LMP_NO_BREDR,	 16#20).
-define(LMP_LE,		 16#40).
-define(LMP_EDR_3SLOT,	 16#80).

-define(LMP_EDR_5SLOT,	 16#01).
-define(LMP_SNIFF_SUBR,	 16#02).
-define(LMP_PAUSE_ENC,	 16#04).
-define(LMP_AFH_CAP_MST, 16#08).
-define(LMP_AFH_CLS_MST, 16#10).
-define(LMP_EDR_ESCO_2M, 16#20).
-define(LMP_EDR_ESCO_3M, 16#40).
-define(LMP_EDR_3S_ESCO, 16#80).

-define(LMP_EXT_INQ,	 16#01).
-define(LMP_LE_BREDR,	 16#02).
-define(LMP_SIMPLE_PAIR, 16#08).
-define(LMP_ENCAPS_PDU,	 16#10).
-define(LMP_ERR_DAT_REP, 16#20).
-define(LMP_NFLUSH_PKTS, 16#40).

-define(LMP_LSTO,	16#01).
-define(LMP_INQ_TX_PWR,	16#02).
-define(LMP_EPC,	16#04).
-define(LMP_EXT_FEAT,	16#80).

%% Extended LMP features
-define(LMP_HOST_SSP,	   16#01).
-define(LMP_HOST_LE,	   16#02).
-define(LMP_HOST_LE_BREDR, 16#04).

%% Scan flags used by HCISETSCAN/WRITE_SCAN_ENABLE
-define(SCAN_DISABLED,     16#00).
-define(SCAN_INQUIRY,      16#01).
-define(SCAN_PAGE,         16#02).


-record(hci_dev_info, 
	{
	  dev_id::uint16_t(),
	  name::binary(),     %% 8 bytes
	  bdaddr::bdaddr_t(), %% 6 bytes [2 pad bytes]
	  flags::uint32_t(),
	  type::uint8_t(),
	  features::binary(),  %% 64 bits

	  pkt_type::uint32_t(),
	  link_policy::uint32_t(),
	  link_mode::uint32_t(),
	  acl_mtu::uint16_t(),
	  acl_pkts::uint16_t(),
	  sco_mtu::uint16_t(),
	  sco_pkts::uint16_t(),
	  stat::#hci_dev_stats{}
	}).

-record(hci_conn_info,
	{
	  handle::uint16_t(),
	  bdaddr::bdaddr_t(),
	  type::uint8_t(),
	  out::uint8_t(),
	  state::uint16_t(),
	  link_mode::uint32_t()
	}).

-record(hci_filter,
	{
	  type_mask  :: uint32_t(),
	  event_mask :: uint64_t(),  %% encoded as 2*uint32_t()
	  opcode     :: uint16_t()
	}).


-define(HCI_FLT_TYPE_BITS,	31).
-define(HCI_FLT_EVENT_BITS,	63).
-define(HCI_FLT_OGF_BITS,	63).
-define(HCI_FLT_OCF_BITS,	127).

-type hci_socket_t() :: port().
-type hci_devid_t()  :: integer().
-type posix() :: atom().
-type level_t() :: boolean() | debug | info | notice | warning | error |
		   critical | alert | emergency | none.


-endif.
