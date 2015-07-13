%% -*- erlang -*-
-ifndef(__HCI_HRL__).
-define(__HCI_HRL__,true).


-define(HCI_MAX_DEV, 16).
-define(HCI_MAX_ACL_SIZE, 1024).
-define(HCI_MAX_SCO_SIZE, 255).
-define(HCI_MAX_EVENT_SIZE, 260).
-define(HCI_MAX_FRAME_SIZE, (?HCI_MAX_ACL_SIZE + 4)).
-define(HCI_DEV_REG, 1).
-define(HCI_DEV_UNREG, 2).
-define(HCI_DEV_UP, 3).
-define(HCI_DEV_DOWN, 4).
-define(HCI_DEV_SUSPEND, 5).
-define(HCI_DEV_RESUME, 6).
-define(HCI_UNKNOWN_COMMAND, 1).
-define(HCI_NO_CONNECTION, 2).
-define(HCI_HARDWARE_FAILURE, 3).
-define(HCI_PAGE_TIMEOUT, 4).
-define(HCI_AUTHENTICATION_FAILURE, 5).
-define(HCI_PIN_OR_KEY_MISSING, 6).
-define(HCI_MEMORY_FULL, 7).
-define(HCI_CONNECTION_TIMEOUT, 8).
-define(HCI_MAX_NUMBER_OF_CONNECTIONS, 9).
-define(HCI_MAX_NUMBER_OF_SCO_CONNECTIONS, 10).
-define(HCI_ACL_CONNECTION_EXISTS, 11).
-define(HCI_COMMAND_DISALLOWED, 12).
-define(HCI_REJECTED_LIMITED_RESOURCES, 13).
-define(HCI_REJECTED_SECURITY, 14).
-define(HCI_REJECTED_PERSONAL, 15).
-define(HCI_HOST_TIMEOUT, 16).
-define(HCI_UNSUPPORTED_FEATURE, 17).
-define(HCI_INVALID_PARAMETERS, 18).
-define(HCI_OE_USER_ENDED_CONNECTION, 19).
-define(HCI_OE_LOW_RESOURCES, 20).
-define(HCI_OE_POWER_OFF, 21).
-define(HCI_CONNECTION_TERMINATED, 22).
-define(HCI_REPEATED_ATTEMPTS, 23).
-define(HCI_PAIRING_NOT_ALLOWED, 24).
-define(HCI_UNKNOWN_LMP_PDU, 25).
-define(HCI_UNSUPPORTED_REMOTE_FEATURE, 26).
-define(HCI_SCO_OFFSET_REJECTED, 27).
-define(HCI_SCO_INTERVAL_REJECTED, 28).
-define(HCI_AIR_MODE_REJECTED, 29).
-define(HCI_INVALID_LMP_PARAMETERS, 30).
-define(HCI_UNSPECIFIED_ERROR, 31).
-define(HCI_UNSUPPORTED_LMP_PARAMETER_VALUE, 32).
-define(HCI_ROLE_CHANGE_NOT_ALLOWED, 33).
-define(HCI_LMP_RESPONSE_TIMEOUT, 34).
-define(HCI_LMP_ERROR_TRANSACTION_COLLISION, 35).
-define(HCI_LMP_PDU_NOT_ALLOWED, 36).
-define(HCI_ENCRYPTION_MODE_NOT_ACCEPTED, 37).
-define(HCI_UNIT_LINK_KEY_USED, 38).
-define(HCI_QOS_NOT_SUPPORTED, 39).
-define(HCI_INSTANT_PASSED, 40).
-define(HCI_PAIRING_NOT_SUPPORTED, 41).
-define(HCI_TRANSACTION_COLLISION, 42).
-define(HCI_QOS_UNACCEPTABLE_PARAMETER, 44).
-define(HCI_QOS_REJECTED, 45).
-define(HCI_CLASSIFICATION_NOT_SUPPORTED, 46).
-define(HCI_INSUFFICIENT_SECURITY, 47).
-define(HCI_PARAMETER_OUT_OF_RANGE, 48).
-define(HCI_ROLE_SWITCH_PENDING, 50).
-define(HCI_SLOT_VIOLATION, 52).
-define(HCI_ROLE_SWITCH_FAILED, 53).
-define(HCI_EIR_TOO_LARGE, 54).
-define(HCI_SIMPLE_PAIRING_NOT_SUPPORTED, 55).
-define(HCI_HOST_BUSY_PAIRING, 56).
-define(OGF_LINK_CTL, 1).
-define(OCF_INQUIRY, 1).
-record(inquiry_cp, {
  lap,
  length,
  num_rsp
}).
-define(inquiry_cp_bin(Lap,Length,Num_rsp),Lap:3/unit:8-binary,Length:1/unsigned-unit:8,Num_rsp:1/unsigned-unit:8).
-define(INQUIRY_CP_SIZE, 5).
-record(status_bdaddr_rp, {
  status,
  bdaddr
}).
-define(status_bdaddr_rp_bin(Status,Bdaddr),Status:1/unsigned-unit:8,Bdaddr:6/unit:8-binary).
-define(STATUS_BDADDR_RP_SIZE, 7).
-define(OCF_INQUIRY_CANCEL, 2).
-define(OCF_PERIODIC_INQUIRY, 3).
-record(periodic_inquiry_cp, {
  max_period,
  min_period,
  lap,
  length,
  num_rsp
}).
-define(periodic_inquiry_cp_bin(Max_period,Min_period,Lap,Length,Num_rsp),Max_period:1/little-unsigned-unit:16,Min_period:1/little-unsigned-unit:16,Lap:3/unit:8-binary,Length:1/unsigned-unit:8,Num_rsp:1/unsigned-unit:8).
-define(PERIODIC_INQUIRY_CP_SIZE, 9).
-define(OCF_EXIT_PERIODIC_INQUIRY, 4).
-define(OCF_CREATE_CONN, 5).
-record(create_conn_cp, {
  bdaddr,
  pkt_type,
  pscan_rep_mode,
  pscan_mode,
  clock_offset,
  role_switch
}).
-define(create_conn_cp_bin(Bdaddr,Pkt_type,Pscan_rep_mode,Pscan_mode,Clock_offset,Role_switch),Bdaddr:6/unit:8-binary,Pkt_type:1/little-unsigned-unit:16,Pscan_rep_mode:1/unsigned-unit:8,Pscan_mode:1/unsigned-unit:8,Clock_offset:1/little-unsigned-unit:16,Role_switch:1/unsigned-unit:8).
-define(CREATE_CONN_CP_SIZE, 13).
-define(OCF_DISCONNECT, 6).
-record(disconnect_cp, {
  handle,
  reason
}).
-define(disconnect_cp_bin(Handle,Reason),Handle:1/little-unsigned-unit:16,Reason:1/unsigned-unit:8).
-define(DISCONNECT_CP_SIZE, 3).
-define(OCF_ADD_SCO, 7).
-record(add_sco_cp, {
  handle,
  pkt_type
}).
-define(add_sco_cp_bin(Handle,Pkt_type),Handle:1/little-unsigned-unit:16,Pkt_type:1/little-unsigned-unit:16).
-define(ADD_SCO_CP_SIZE, 4).
-define(OCF_CREATE_CONN_CANCEL, 8).
-record(create_conn_cancel_cp, {
  bdaddr
}).
-define(create_conn_cancel_cp_bin(Bdaddr),Bdaddr:6/unit:8-binary).
-define(CREATE_CONN_CANCEL_CP_SIZE, 6).
-define(OCF_ACCEPT_CONN_REQ, 9).
-record(accept_conn_req_cp, {
  bdaddr,
  role
}).
-define(accept_conn_req_cp_bin(Bdaddr,Role),Bdaddr:6/unit:8-binary,Role:1/unsigned-unit:8).
-define(ACCEPT_CONN_REQ_CP_SIZE, 7).
-define(OCF_REJECT_CONN_REQ, 10).
-record(reject_conn_req_cp, {
  bdaddr,
  reason
}).
-define(reject_conn_req_cp_bin(Bdaddr,Reason),Bdaddr:6/unit:8-binary,Reason:1/unsigned-unit:8).
-define(REJECT_CONN_REQ_CP_SIZE, 7).
-define(OCF_LINK_KEY_REPLY, 11).
-record(link_key_reply_cp, {
  bdaddr,
  link_key
}).
-define(link_key_reply_cp_bin(Bdaddr,Link_key),Bdaddr:6/unit:8-binary,Link_key:16/unit:8-binary).
-define(LINK_KEY_REPLY_CP_SIZE, 22).
-define(OCF_LINK_KEY_NEG_REPLY, 12).
-record(link_key_neg_reply_cp, {
  bdaddr
}).
-define(link_key_neg_reply_cp_bin(Bdaddr),Bdaddr:6/unit:8-binary).
-define(LINK_KEY_NEG_REPLY_CP_SIZE, 6).
-define(OCF_PIN_CODE_REPLY, 13).
-record(pin_code_reply_cp, {
  bdaddr,
  pin_len,
  pin_code
}).
-define(pin_code_reply_cp_bin(Bdaddr,Pin_len,Pin_code),Bdaddr:6/unit:8-binary,Pin_len:1/unsigned-unit:8,Pin_code:16/unit:8-binary).
-define(PIN_CODE_REPLY_CP_SIZE, 23).
-define(OCF_PIN_CODE_NEG_REPLY, 14).
-define(OCF_SET_CONN_PTYPE, 15).
-record(set_conn_ptype_cp, {
  handle,
  pkt_type
}).
-define(set_conn_ptype_cp_bin(Handle,Pkt_type),Handle:1/little-unsigned-unit:16,Pkt_type:1/little-unsigned-unit:16).
-define(SET_CONN_PTYPE_CP_SIZE, 4).
-define(OCF_AUTH_REQUESTED, 17).
-record(auth_requested_cp, {
  handle
}).
-define(auth_requested_cp_bin(Handle),Handle:1/little-unsigned-unit:16).
-define(AUTH_REQUESTED_CP_SIZE, 2).
-define(OCF_SET_CONN_ENCRYPT, 19).
-record(set_conn_encrypt_cp, {
  handle,
  encrypt
}).
-define(set_conn_encrypt_cp_bin(Handle,Encrypt),Handle:1/little-unsigned-unit:16,Encrypt:1/unsigned-unit:8).
-define(SET_CONN_ENCRYPT_CP_SIZE, 3).
-define(OCF_CHANGE_CONN_LINK_KEY, 21).
-record(change_conn_link_key_cp, {
  handle
}).
-define(change_conn_link_key_cp_bin(Handle),Handle:1/little-unsigned-unit:16).
-define(CHANGE_CONN_LINK_KEY_CP_SIZE, 2).
-define(OCF_MASTER_LINK_KEY, 23).
-record(master_link_key_cp, {
  key_flag
}).
-define(master_link_key_cp_bin(Key_flag),Key_flag:1/unsigned-unit:8).
-define(MASTER_LINK_KEY_CP_SIZE, 1).
-define(OCF_REMOTE_NAME_REQ, 25).
-record(remote_name_req_cp, {
  bdaddr,
  pscan_rep_mode,
  pscan_mode,
  clock_offset
}).
-define(remote_name_req_cp_bin(Bdaddr,Pscan_rep_mode,Pscan_mode,Clock_offset),Bdaddr:6/unit:8-binary,Pscan_rep_mode:1/unsigned-unit:8,Pscan_mode:1/unsigned-unit:8,Clock_offset:1/little-unsigned-unit:16).
-define(REMOTE_NAME_REQ_CP_SIZE, 10).
-define(OCF_REMOTE_NAME_REQ_CANCEL, 26).
-record(remote_name_req_cancel_cp, {
  bdaddr
}).
-define(remote_name_req_cancel_cp_bin(Bdaddr),Bdaddr:6/unit:8-binary).
-define(REMOTE_NAME_REQ_CANCEL_CP_SIZE, 6).
-define(OCF_READ_REMOTE_FEATURES, 27).
-record(read_remote_features_cp, {
  handle
}).
-define(read_remote_features_cp_bin(Handle),Handle:1/little-unsigned-unit:16).
-define(READ_REMOTE_FEATURES_CP_SIZE, 2).
-define(OCF_READ_REMOTE_EXT_FEATURES, 28).
-record(read_remote_ext_features_cp, {
  handle,
  page_num
}).
-define(read_remote_ext_features_cp_bin(Handle,Page_num),Handle:1/little-unsigned-unit:16,Page_num:1/unsigned-unit:8).
-define(READ_REMOTE_EXT_FEATURES_CP_SIZE, 3).
-define(OCF_READ_REMOTE_VERSION, 29).
-record(read_remote_version_cp, {
  handle
}).
-define(read_remote_version_cp_bin(Handle),Handle:1/little-unsigned-unit:16).
-define(READ_REMOTE_VERSION_CP_SIZE, 2).
-define(OCF_READ_CLOCK_OFFSET, 31).
-record(read_clock_offset_cp, {
  handle
}).
-define(read_clock_offset_cp_bin(Handle),Handle:1/little-unsigned-unit:16).
-define(READ_CLOCK_OFFSET_CP_SIZE, 2).
-define(OCF_READ_LMP_HANDLE, 32).
-define(OCF_SETUP_SYNC_CONN, 40).
-record(setup_sync_conn_cp, {
  handle,
  tx_bandwith,
  rx_bandwith,
  max_latency,
  voice_setting,
  retrans_effort,
  pkt_type
}).
-define(setup_sync_conn_cp_bin(Handle,Tx_bandwith,Rx_bandwith,Max_latency,Voice_setting,Retrans_effort,Pkt_type),Handle:1/little-unsigned-unit:16,Tx_bandwith:1/little-unsigned-unit:32,Rx_bandwith:1/little-unsigned-unit:32,Max_latency:1/little-unsigned-unit:16,Voice_setting:1/little-unsigned-unit:16,Retrans_effort:1/unsigned-unit:8,Pkt_type:1/little-unsigned-unit:16).
-define(SETUP_SYNC_CONN_CP_SIZE, 17).
-define(OCF_ACCEPT_SYNC_CONN_REQ, 41).
-record(accept_sync_conn_req_cp, {
  bdaddr,
  tx_bandwith,
  rx_bandwith,
  max_latency,
  voice_setting,
  retrans_effort,
  pkt_type
}).
-define(accept_sync_conn_req_cp_bin(Bdaddr,Tx_bandwith,Rx_bandwith,Max_latency,Voice_setting,Retrans_effort,Pkt_type),Bdaddr:6/unit:8-binary,Tx_bandwith:1/little-unsigned-unit:32,Rx_bandwith:1/little-unsigned-unit:32,Max_latency:1/little-unsigned-unit:16,Voice_setting:1/little-unsigned-unit:16,Retrans_effort:1/unsigned-unit:8,Pkt_type:1/little-unsigned-unit:16).
-define(ACCEPT_SYNC_CONN_REQ_CP_SIZE, 21).
-define(OCF_REJECT_SYNC_CONN_REQ, 42).
-record(reject_sync_conn_req_cp, {
  bdaddr,
  reason
}).
-define(reject_sync_conn_req_cp_bin(Bdaddr,Reason),Bdaddr:6/unit:8-binary,Reason:1/unsigned-unit:8).
-define(REJECT_SYNC_CONN_REQ_CP_SIZE, 7).
-define(OCF_IO_CAPABILITY_REPLY, 43).
-record(io_capability_reply_cp, {
  bdaddr,
  capability,
  oob_data,
  authentication
}).
-define(io_capability_reply_cp_bin(Bdaddr,Capability,Oob_data,Authentication),Bdaddr:6/unit:8-binary,Capability:1/unsigned-unit:8,Oob_data:1/unsigned-unit:8,Authentication:1/unsigned-unit:8).
-define(IO_CAPABILITY_REPLY_CP_SIZE, 9).
-define(OCF_USER_CONFIRM_REPLY, 44).
-record(user_confirm_reply_cp, {
  bdaddr
}).
-define(user_confirm_reply_cp_bin(Bdaddr),Bdaddr:6/unit:8-binary).
-define(USER_CONFIRM_REPLY_CP_SIZE, 6).
-define(OCF_USER_CONFIRM_NEG_REPLY, 45).
-define(OCF_USER_PASSKEY_REPLY, 46).
-record(user_passkey_reply_cp, {
  bdaddr,
  passkey
}).
-define(user_passkey_reply_cp_bin(Bdaddr,Passkey),Bdaddr:6/unit:8-binary,Passkey:1/little-unsigned-unit:32).
-define(USER_PASSKEY_REPLY_CP_SIZE, 10).
-define(OCF_USER_PASSKEY_NEG_REPLY, 47).
-define(OCF_REMOTE_OOB_DATA_REPLY, 48).
-record(remote_oob_data_reply_cp, {
  bdaddr,
  hash,
  randomizer
}).
-define(remote_oob_data_reply_cp_bin(Bdaddr,Hash,Randomizer),Bdaddr:6/unit:8-binary,Hash:16/unit:8-binary,Randomizer:16/unit:8-binary).
-define(REMOTE_OOB_DATA_REPLY_CP_SIZE, 38).
-define(OCF_REMOTE_OOB_DATA_NEG_REPLY, 51).
-define(OCF_IO_CAPABILITY_NEG_REPLY, 52).
-record(io_capability_neg_reply_cp, {
  bdaddr,
  reason
}).
-define(io_capability_neg_reply_cp_bin(Bdaddr,Reason),Bdaddr:6/unit:8-binary,Reason:1/unsigned-unit:8).
-define(IO_CAPABILITY_NEG_REPLY_CP_SIZE, 7).
-define(OCF_CREATE_PHYSICAL_LINK, 53).
-record(create_physical_link_cp, {
  handle,
  key_length,
  key_type,
  key
}).
-define(create_physical_link_cp_bin(Handle,Key_length,Key_type,Key),Handle:1/unsigned-unit:8,Key_length:1/unsigned-unit:8,Key_type:1/unsigned-unit:8,Key:32/unit:8-binary).
-define(CREATE_PHYSICAL_LINK_CP_SIZE, 35).
-define(OCF_ACCEPT_PHYSICAL_LINK, 54).
-define(OCF_DISCONNECT_PHYSICAL_LINK, 55).
-record(disconnect_physical_link_cp, {
  handle,
  reason
}).
-define(disconnect_physical_link_cp_bin(Handle,Reason),Handle:1/unsigned-unit:8,Reason:1/unsigned-unit:8).
-define(DISCONNECT_PHYSICAL_LINK_CP_SIZE, 2).
-define(OCF_CREATE_LOGICAL_LINK, 56).
-record(create_logical_link_cp, {
  handle,
  tx_flow,
  rx_flow
}).
-define(create_logical_link_cp_bin(Handle,Tx_flow,Rx_flow),Handle:1/unsigned-unit:8,Tx_flow:16/unit:8-binary,Rx_flow:16/unit:8-binary).
-define(CREATE_LOGICAL_LINK_CP_SIZE, 33).
-define(OCF_ACCEPT_LOGICAL_LINK, 57).
-define(OCF_DISCONNECT_LOGICAL_LINK, 58).
-record(disconnect_logical_link_cp, {
  handle
}).
-define(disconnect_logical_link_cp_bin(Handle),Handle:1/little-unsigned-unit:16).
-define(DISCONNECT_LOGICAL_LINK_CP_SIZE, 2).
-define(OCF_LOGICAL_LINK_CANCEL, 59).
-record(cancel_logical_link_cp, {
  handle,
  tx_flow_id
}).
-define(cancel_logical_link_cp_bin(Handle,Tx_flow_id),Handle:1/unsigned-unit:8,Tx_flow_id:1/unsigned-unit:8).
-define(LOGICAL_LINK_CANCEL_CP_SIZE, 2).
-record(cancel_logical_link_rp, {
  status,
  handle,
  tx_flow_id
}).
-define(cancel_logical_link_rp_bin(Status,Handle,Tx_flow_id),Status:1/unsigned-unit:8,Handle:1/unsigned-unit:8,Tx_flow_id:1/unsigned-unit:8).
-define(LOGICAL_LINK_CANCEL_RP_SIZE, 3).
-define(OCF_FLOW_SPEC_MODIFY, 60).
-define(OGF_LINK_POLICY, 2).
-define(OCF_HOLD_MODE, 1).
-record(hold_mode_cp, {
  handle,
  max_interval,
  min_interval
}).
-define(hold_mode_cp_bin(Handle,Max_interval,Min_interval),Handle:1/little-unsigned-unit:16,Max_interval:1/little-unsigned-unit:16,Min_interval:1/little-unsigned-unit:16).
-define(HOLD_MODE_CP_SIZE, 6).
-define(OCF_SNIFF_MODE, 3).
-record(sniff_mode_cp, {
  handle,
  max_interval,
  min_interval,
  attempt,
  timeout
}).
-define(sniff_mode_cp_bin(Handle,Max_interval,Min_interval,Attempt,Timeout),Handle:1/little-unsigned-unit:16,Max_interval:1/little-unsigned-unit:16,Min_interval:1/little-unsigned-unit:16,Attempt:1/little-unsigned-unit:16,Timeout:1/little-unsigned-unit:16).
-define(SNIFF_MODE_CP_SIZE, 10).
-define(OCF_EXIT_SNIFF_MODE, 4).
-record(exit_sniff_mode_cp, {
  handle
}).
-define(exit_sniff_mode_cp_bin(Handle),Handle:1/little-unsigned-unit:16).
-define(EXIT_SNIFF_MODE_CP_SIZE, 2).
-define(OCF_PARK_MODE, 5).
-record(park_mode_cp, {
  handle,
  max_interval,
  min_interval
}).
-define(park_mode_cp_bin(Handle,Max_interval,Min_interval),Handle:1/little-unsigned-unit:16,Max_interval:1/little-unsigned-unit:16,Min_interval:1/little-unsigned-unit:16).
-define(PARK_MODE_CP_SIZE, 6).
-define(OCF_EXIT_PARK_MODE, 6).
-record(exit_park_mode_cp, {
  handle
}).
-define(exit_park_mode_cp_bin(Handle),Handle:1/little-unsigned-unit:16).
-define(EXIT_PARK_MODE_CP_SIZE, 2).
-define(OCF_QOS_SETUP, 7).
-record(hci_qos, {
  service_type,
  token_rate,
  peak_bandwidth,
  latency,
  delay_variation
}).
-define(hci_qos_bin(Service_type,Token_rate,Peak_bandwidth,Latency,Delay_variation),Service_type:1/unsigned-unit:8,Token_rate:1/little-unsigned-unit:32,Peak_bandwidth:1/little-unsigned-unit:32,Latency:1/little-unsigned-unit:32,Delay_variation:1/little-unsigned-unit:32).
-define(HCI_QOS_CP_SIZE, 17).
-record(qos_setup_cp, {
  handle,
  flags,
  qos
}).
-define(qos_setup_cp_bin(Handle,Flags,Qos),Handle:1/little-unsigned-unit:16,Flags:1/unsigned-unit:8,Qos:17/unit:8-binary).
-define(QOS_SETUP_CP_SIZE, (3 + ?HCI_QOS_CP_SIZE)).
-define(OCF_ROLE_DISCOVERY, 9).
-record(role_discovery_cp, {
  handle
}).
-define(role_discovery_cp_bin(Handle),Handle:1/little-unsigned-unit:16).
-define(ROLE_DISCOVERY_CP_SIZE, 2).
-record(role_discovery_rp, {
  status,
  handle,
  role
}).
-define(role_discovery_rp_bin(Status,Handle,Role),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Role:1/unsigned-unit:8).
-define(ROLE_DISCOVERY_RP_SIZE, 4).
-define(OCF_SWITCH_ROLE, 11).
-record(switch_role_cp, {
  bdaddr,
  role
}).
-define(switch_role_cp_bin(Bdaddr,Role),Bdaddr:6/unit:8-binary,Role:1/unsigned-unit:8).
-define(SWITCH_ROLE_CP_SIZE, 7).
-define(OCF_READ_LINK_POLICY, 12).
-record(read_link_policy_cp, {
  handle
}).
-define(read_link_policy_cp_bin(Handle),Handle:1/little-unsigned-unit:16).
-define(READ_LINK_POLICY_CP_SIZE, 2).
-record(read_link_policy_rp, {
  status,
  handle,
  policy
}).
-define(read_link_policy_rp_bin(Status,Handle,Policy),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Policy:1/little-unsigned-unit:16).
-define(READ_LINK_POLICY_RP_SIZE, 5).
-define(OCF_WRITE_LINK_POLICY, 13).
-record(write_link_policy_cp, {
  handle,
  policy
}).
-define(write_link_policy_cp_bin(Handle,Policy),Handle:1/little-unsigned-unit:16,Policy:1/little-unsigned-unit:16).
-define(WRITE_LINK_POLICY_CP_SIZE, 4).
-record(write_link_policy_rp, {
  status,
  handle
}).
-define(write_link_policy_rp_bin(Status,Handle),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16).
-define(WRITE_LINK_POLICY_RP_SIZE, 3).
-define(OCF_READ_DEFAULT_LINK_POLICY, 14).
-define(OCF_WRITE_DEFAULT_LINK_POLICY, 15).
-define(OCF_FLOW_SPECIFICATION, 16).
-define(OCF_SNIFF_SUBRATING, 17).
-record(sniff_subrating_cp, {
  handle,
  max_latency,
  min_remote_timeout,
  min_local_timeout
}).
-define(sniff_subrating_cp_bin(Handle,Max_latency,Min_remote_timeout,Min_local_timeout),Handle:1/little-unsigned-unit:16,Max_latency:1/little-unsigned-unit:16,Min_remote_timeout:1/little-unsigned-unit:16,Min_local_timeout:1/little-unsigned-unit:16).
-define(SNIFF_SUBRATING_CP_SIZE, 8).
-define(OGF_HOST_CTL, 3).
-define(OCF_SET_EVENT_MASK, 1).
-record(set_event_mask_cp, {
  mask
}).
-define(set_event_mask_cp_bin(Mask),Mask:8/unit:8-binary).
-define(SET_EVENT_MASK_CP_SIZE, 8).
-define(OCF_RESET, 3).
-define(OCF_SET_EVENT_FLT, 5).
-record(set_event_flt_cp, {
  flt_type,
  cond_type,
  condition
}).
-define(set_event_flt_cp_bin(Flt_type,Cond_type,Condition),Flt_type:1/unsigned-unit:8,Cond_type:1/unsigned-unit:8,Condition:0/unit:8-binary).
-define(SET_EVENT_FLT_CP_SIZE, 2).
-define(FLT_CLEAR_ALL, 0).
-define(FLT_INQ_RESULT, 1).
-define(FLT_CONN_SETUP, 2).
-define(INQ_RESULT_RETURN_ALL, 0).
-define(INQ_RESULT_RETURN_CLASS, 1).
-define(INQ_RESULT_RETURN_BDADDR, 2).
-define(CONN_SETUP_ALLOW_ALL, 0).
-define(CONN_SETUP_ALLOW_CLASS, 1).
-define(CONN_SETUP_ALLOW_BDADDR, 2).
-define(CONN_SETUP_AUTO_OFF, 1).
-define(CONN_SETUP_AUTO_ON, 2).
-define(OCF_FLUSH, 8).
-define(OCF_READ_PIN_TYPE, 9).
-record(read_pin_type_rp, {
  status,
  pin_type
}).
-define(read_pin_type_rp_bin(Status,Pin_type),Status:1/unsigned-unit:8,Pin_type:1/unsigned-unit:8).
-define(READ_PIN_TYPE_RP_SIZE, 2).
-define(OCF_WRITE_PIN_TYPE, 10).
-record(write_pin_type_cp, {
  pin_type
}).
-define(write_pin_type_cp_bin(Pin_type),Pin_type:1/unsigned-unit:8).
-define(WRITE_PIN_TYPE_CP_SIZE, 1).
-define(OCF_CREATE_NEW_UNIT_KEY, 11).
-define(OCF_READ_STORED_LINK_KEY, 13).
-record(read_stored_link_key_cp, {
  bdaddr,
  read_all
}).
-define(read_stored_link_key_cp_bin(Bdaddr,Read_all),Bdaddr:6/unit:8-binary,Read_all:1/unsigned-unit:8).
-define(READ_STORED_LINK_KEY_CP_SIZE, 7).
-record(read_stored_link_key_rp, {
  status,
  max_keys,
  num_keys
}).
-define(read_stored_link_key_rp_bin(Status,Max_keys,Num_keys),Status:1/unsigned-unit:8,Max_keys:1/little-unsigned-unit:16,Num_keys:1/little-unsigned-unit:16).
-define(READ_STORED_LINK_KEY_RP_SIZE, 5).
-define(OCF_WRITE_STORED_LINK_KEY, 17).
-record(write_stored_link_key_cp, {
  num_keys
}).
-define(write_stored_link_key_cp_bin(Num_keys),Num_keys:1/unsigned-unit:8).
-define(WRITE_STORED_LINK_KEY_CP_SIZE, 1).
-record(write_stored_link_key_rp, {
  status,
  num_keys
}).
-define(write_stored_link_key_rp_bin(Status,Num_keys),Status:1/unsigned-unit:8,Num_keys:1/unsigned-unit:8).
-define(READ_WRITE_LINK_KEY_RP_SIZE, 2).
-define(OCF_DELETE_STORED_LINK_KEY, 18).
-record(delete_stored_link_key_cp, {
  bdaddr,
  delete_all
}).
-define(delete_stored_link_key_cp_bin(Bdaddr,Delete_all),Bdaddr:6/unit:8-binary,Delete_all:1/unsigned-unit:8).
-define(DELETE_STORED_LINK_KEY_CP_SIZE, 7).
-record(delete_stored_link_key_rp, {
  status,
  num_keys
}).
-define(delete_stored_link_key_rp_bin(Status,Num_keys),Status:1/unsigned-unit:8,Num_keys:1/little-unsigned-unit:16).
-define(DELETE_STORED_LINK_KEY_RP_SIZE, 3).
-define(HCI_MAX_NAME_LENGTH, 248).
-define(OCF_CHANGE_LOCAL_NAME, 19).
-record(change_local_name_cp, {
  name
}).
-define(change_local_name_cp_bin(Name),Name:(?HCI_MAX_NAME_LENGTH)/unit:8-binary).
-define(CHANGE_LOCAL_NAME_CP_SIZE, 248).
-define(OCF_READ_LOCAL_NAME, 20).
-record(read_local_name_rp, {
  status,
  name
}).
-define(read_local_name_rp_bin(Status,Name),Status:1/unsigned-unit:8,Name:(?HCI_MAX_NAME_LENGTH)/unit:8-binary).
-define(READ_LOCAL_NAME_RP_SIZE, 249).
-define(OCF_READ_CONN_ACCEPT_TIMEOUT, 21).
-record(read_conn_accept_timeout_rp, {
  status,
  timeout
}).
-define(read_conn_accept_timeout_rp_bin(Status,Timeout),Status:1/unsigned-unit:8,Timeout:1/little-unsigned-unit:16).
-define(READ_CONN_ACCEPT_TIMEOUT_RP_SIZE, 3).
-define(OCF_WRITE_CONN_ACCEPT_TIMEOUT, 22).
-record(write_conn_accept_timeout_cp, {
  timeout
}).
-define(write_conn_accept_timeout_cp_bin(Timeout),Timeout:1/little-unsigned-unit:16).
-define(WRITE_CONN_ACCEPT_TIMEOUT_CP_SIZE, 2).
-define(OCF_READ_PAGE_TIMEOUT, 23).
-record(read_page_timeout_rp, {
  status,
  timeout
}).
-define(read_page_timeout_rp_bin(Status,Timeout),Status:1/unsigned-unit:8,Timeout:1/little-unsigned-unit:16).
-define(READ_PAGE_TIMEOUT_RP_SIZE, 3).
-define(OCF_WRITE_PAGE_TIMEOUT, 24).
-record(write_page_timeout_cp, {
  timeout
}).
-define(write_page_timeout_cp_bin(Timeout),Timeout:1/little-unsigned-unit:16).
-define(WRITE_PAGE_TIMEOUT_CP_SIZE, 2).
-define(OCF_READ_SCAN_ENABLE, 25).
-record(read_scan_enable_rp, {
  status,
  enable
}).
-define(read_scan_enable_rp_bin(Status,Enable),Status:1/unsigned-unit:8,Enable:1/unsigned-unit:8).
-define(READ_SCAN_ENABLE_RP_SIZE, 2).
-define(OCF_WRITE_SCAN_ENABLE, 26).
-define(OCF_READ_PAGE_ACTIVITY, 27).
-record(read_page_activity_rp, {
  status,
  interval,
  window
}).
-define(read_page_activity_rp_bin(Status,Interval,Window),Status:1/unsigned-unit:8,Interval:1/little-unsigned-unit:16,Window:1/little-unsigned-unit:16).
-define(READ_PAGE_ACTIVITY_RP_SIZE, 5).
-define(OCF_WRITE_PAGE_ACTIVITY, 28).
-record(write_page_activity_cp, {
  interval,
  window
}).
-define(write_page_activity_cp_bin(Interval,Window),Interval:1/little-unsigned-unit:16,Window:1/little-unsigned-unit:16).
-define(WRITE_PAGE_ACTIVITY_CP_SIZE, 4).
-define(OCF_READ_INQ_ACTIVITY, 29).
-record(read_inq_activity_rp, {
  status,
  interval,
  window
}).
-define(read_inq_activity_rp_bin(Status,Interval,Window),Status:1/unsigned-unit:8,Interval:1/little-unsigned-unit:16,Window:1/little-unsigned-unit:16).
-define(READ_INQ_ACTIVITY_RP_SIZE, 5).
-define(OCF_WRITE_INQ_ACTIVITY, 30).
-record(write_inq_activity_cp, {
  interval,
  window
}).
-define(write_inq_activity_cp_bin(Interval,Window),Interval:1/little-unsigned-unit:16,Window:1/little-unsigned-unit:16).
-define(WRITE_INQ_ACTIVITY_CP_SIZE, 4).
-define(OCF_READ_AUTH_ENABLE, 31).
-define(OCF_WRITE_AUTH_ENABLE, 32).
-define(AUTH_DISABLED, 0).
-define(AUTH_ENABLED, 1).
-define(OCF_READ_ENCRYPT_MODE, 33).
-define(OCF_WRITE_ENCRYPT_MODE, 34).
-define(ENCRYPT_DISABLED, 0).
-define(ENCRYPT_P2P, 1).
-define(ENCRYPT_BOTH, 2).
-define(OCF_READ_CLASS_OF_DEV, 35).
-record(read_class_of_dev_rp, {
  status,
  dev_class
}).
-define(read_class_of_dev_rp_bin(Status,Dev_class),Status:1/unsigned-unit:8,Dev_class:3/unit:8-binary).
-define(READ_CLASS_OF_DEV_RP_SIZE, 4).
-define(OCF_WRITE_CLASS_OF_DEV, 36).
-record(write_class_of_dev_cp, {
  dev_class
}).
-define(write_class_of_dev_cp_bin(Dev_class),Dev_class:3/unit:8-binary).
-define(WRITE_CLASS_OF_DEV_CP_SIZE, 3).
-define(OCF_READ_VOICE_SETTING, 37).
-record(read_voice_setting_rp, {
  status,
  voice_setting
}).
-define(read_voice_setting_rp_bin(Status,Voice_setting),Status:1/unsigned-unit:8,Voice_setting:1/little-unsigned-unit:16).
-define(READ_VOICE_SETTING_RP_SIZE, 3).
-define(OCF_WRITE_VOICE_SETTING, 38).
-record(write_voice_setting_cp, {
  voice_setting
}).
-define(write_voice_setting_cp_bin(Voice_setting),Voice_setting:1/little-unsigned-unit:16).
-define(WRITE_VOICE_SETTING_CP_SIZE, 2).
-define(OCF_READ_AUTOMATIC_FLUSH_TIMEOUT, 39).
-define(OCF_WRITE_AUTOMATIC_FLUSH_TIMEOUT, 40).
-define(OCF_READ_NUM_BROADCAST_RETRANS, 41).
-define(OCF_WRITE_NUM_BROADCAST_RETRANS, 42).
-define(OCF_READ_HOLD_MODE_ACTIVITY, 43).
-define(OCF_WRITE_HOLD_MODE_ACTIVITY, 44).
-define(OCF_READ_TRANSMIT_POWER_LEVEL, 45).
-record(read_transmit_power_level_cp, {
  handle,
  type
}).
-define(read_transmit_power_level_cp_bin(Handle,Type),Handle:1/little-unsigned-unit:16,Type:1/unsigned-unit:8).
-define(READ_TRANSMIT_POWER_LEVEL_CP_SIZE, 3).
-record(read_transmit_power_level_rp, {
  status,
  handle,
  level
}).
-define(read_transmit_power_level_rp_bin(Status,Handle,Level),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Level:1/signed-unit:8).
-define(READ_TRANSMIT_POWER_LEVEL_RP_SIZE, 4).
-define(OCF_READ_SYNC_FLOW_ENABLE, 46).
-define(OCF_WRITE_SYNC_FLOW_ENABLE, 47).
-define(OCF_SET_CONTROLLER_TO_HOST_FC, 49).
-define(OCF_HOST_BUFFER_SIZE, 51).
-record(host_buffer_size_cp, {
  acl_mtu,
  sco_mtu,
  acl_max_pkt,
  sco_max_pkt
}).
-define(host_buffer_size_cp_bin(Acl_mtu,Sco_mtu,Acl_max_pkt,Sco_max_pkt),Acl_mtu:1/little-unsigned-unit:16,Sco_mtu:1/unsigned-unit:8,Acl_max_pkt:1/little-unsigned-unit:16,Sco_max_pkt:1/little-unsigned-unit:16).
-define(HOST_BUFFER_SIZE_CP_SIZE, 7).
-define(OCF_HOST_NUM_COMP_PKTS, 53).
-record(host_num_comp_pkts_cp, {
  num_hndl
}).
-define(host_num_comp_pkts_cp_bin(Num_hndl),Num_hndl:1/unsigned-unit:8).
-define(HOST_NUM_COMP_PKTS_CP_SIZE, 1).
-define(OCF_READ_LINK_SUPERVISION_TIMEOUT, 54).
-record(read_link_supervision_timeout_rp, {
  status,
  handle,
  timeout
}).
-define(read_link_supervision_timeout_rp_bin(Status,Handle,Timeout),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Timeout:1/little-unsigned-unit:16).
-define(READ_LINK_SUPERVISION_TIMEOUT_RP_SIZE, 5).
-define(OCF_WRITE_LINK_SUPERVISION_TIMEOUT, 55).
-record(write_link_supervision_timeout_cp, {
  handle,
  timeout
}).
-define(write_link_supervision_timeout_cp_bin(Handle,Timeout),Handle:1/little-unsigned-unit:16,Timeout:1/little-unsigned-unit:16).
-define(WRITE_LINK_SUPERVISION_TIMEOUT_CP_SIZE, 4).
-record(write_link_supervision_timeout_rp, {
  status,
  handle
}).
-define(write_link_supervision_timeout_rp_bin(Status,Handle),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16).
-define(WRITE_LINK_SUPERVISION_TIMEOUT_RP_SIZE, 3).
-define(OCF_READ_NUM_SUPPORTED_IAC, 56).
-define(MAX_IAC_LAP, 64).
-define(OCF_READ_CURRENT_IAC_LAP, 57).
-record(read_current_iac_lap_rp, {
  status,
  num_current_iac,
  lap
}).
-define(read_current_iac_lap_rp_bin(Status,Num_current_iac,Lap),Status:1/unsigned-unit:8,Num_current_iac:1/unsigned-unit:8,Lap:(3*?MAX_IAC_LAP)/unit:8-binary).
-define(READ_CURRENT_IAC_LAP_RP_SIZE, 2+3*?MAX_IAC_LAP).
-define(OCF_WRITE_CURRENT_IAC_LAP, 58).
-record(write_current_iac_lap_cp, {
  num_current_iac,
  lap
}).
-define(write_current_iac_lap_cp_bin(Num_current_iac,Lap),Num_current_iac:1/unsigned-unit:8,Lap:(3*?MAX_IAC_LAP)/unit:8-binary).
-define(WRITE_CURRENT_IAC_LAP_CP_SIZE, 1+3*?MAX_IAC_LAP).
-define(OCF_READ_PAGE_SCAN_PERIOD_MODE, 59).
-define(OCF_WRITE_PAGE_SCAN_PERIOD_MODE, 60).
-define(OCF_READ_PAGE_SCAN_MODE, 61).
-define(OCF_WRITE_PAGE_SCAN_MODE, 62).
-define(OCF_SET_AFH_CLASSIFICATION, 63).
-record(set_afh_classification_cp, {
  map
}).
-define(set_afh_classification_cp_bin(Map),Map:10/unit:8-binary).
-define(SET_AFH_CLASSIFICATION_CP_SIZE, 10).
-record(set_afh_classification_rp, {
  status
}).
-define(set_afh_classification_rp_bin(Status),Status:1/unsigned-unit:8).
-define(SET_AFH_CLASSIFICATION_RP_SIZE, 1).
-define(OCF_READ_INQUIRY_SCAN_TYPE, 66).
-record(read_inquiry_scan_type_rp, {
  status,
  type
}).
-define(read_inquiry_scan_type_rp_bin(Status,Type),Status:1/unsigned-unit:8,Type:1/unsigned-unit:8).
-define(READ_INQUIRY_SCAN_TYPE_RP_SIZE, 2).
-define(OCF_WRITE_INQUIRY_SCAN_TYPE, 67).
-record(write_inquiry_scan_type_cp, {
  type
}).
-define(write_inquiry_scan_type_cp_bin(Type),Type:1/unsigned-unit:8).
-define(WRITE_INQUIRY_SCAN_TYPE_CP_SIZE, 1).
-record(write_inquiry_scan_type_rp, {
  status
}).
-define(write_inquiry_scan_type_rp_bin(Status),Status:1/unsigned-unit:8).
-define(WRITE_INQUIRY_SCAN_TYPE_RP_SIZE, 1).
-define(OCF_READ_INQUIRY_MODE, 68).
-record(read_inquiry_mode_rp, {
  status,
  mode
}).
-define(read_inquiry_mode_rp_bin(Status,Mode),Status:1/unsigned-unit:8,Mode:1/unsigned-unit:8).
-define(READ_INQUIRY_MODE_RP_SIZE, 2).
-define(OCF_WRITE_INQUIRY_MODE, 69).
-record(write_inquiry_mode_cp, {
  mode
}).
-define(write_inquiry_mode_cp_bin(Mode),Mode:1/unsigned-unit:8).
-define(WRITE_INQUIRY_MODE_CP_SIZE, 1).
-record(write_inquiry_mode_rp, {
  status
}).
-define(write_inquiry_mode_rp_bin(Status),Status:1/unsigned-unit:8).
-define(WRITE_INQUIRY_MODE_RP_SIZE, 1).
-define(OCF_READ_PAGE_SCAN_TYPE, 70).
-define(OCF_WRITE_PAGE_SCAN_TYPE, 71).
-define(PAGE_SCAN_TYPE_STANDARD, 0).
-define(PAGE_SCAN_TYPE_INTERLACED, 1).
-define(OCF_READ_AFH_MODE, 72).
-record(read_afh_mode_rp, {
  status,
  mode
}).
-define(read_afh_mode_rp_bin(Status,Mode),Status:1/unsigned-unit:8,Mode:1/unsigned-unit:8).
-define(READ_AFH_MODE_RP_SIZE, 2).
-define(OCF_WRITE_AFH_MODE, 73).
-record(write_afh_mode_cp, {
  mode
}).
-define(write_afh_mode_cp_bin(Mode),Mode:1/unsigned-unit:8).
-define(WRITE_AFH_MODE_CP_SIZE, 1).
-record(write_afh_mode_rp, {
  status
}).
-define(write_afh_mode_rp_bin(Status),Status:1/unsigned-unit:8).
-define(WRITE_AFH_MODE_RP_SIZE, 1).
-define(HCI_MAX_EIR_LENGTH, 240).
-define(OCF_READ_EXT_INQUIRY_RESPONSE, 81).
-record(read_ext_inquiry_response_rp, {
  status,
  fec,
  data
}).
-define(read_ext_inquiry_response_rp_bin(Status,Fec,Data),Status:1/unsigned-unit:8,Fec:1/unsigned-unit:8,Data:(?HCI_MAX_EIR_LENGTH)/unit:8-binary).
-define(READ_EXT_INQUIRY_RESPONSE_RP_SIZE, 242).
-define(OCF_WRITE_EXT_INQUIRY_RESPONSE, 82).
-record(write_ext_inquiry_response_cp, {
  fec,
  data
}).
-define(write_ext_inquiry_response_cp_bin(Fec,Data),Fec:1/unsigned-unit:8,Data:(?HCI_MAX_EIR_LENGTH)/unit:8-binary).
-define(WRITE_EXT_INQUIRY_RESPONSE_CP_SIZE, 241).
-record(write_ext_inquiry_response_rp, {
  status
}).
-define(write_ext_inquiry_response_rp_bin(Status),Status:1/unsigned-unit:8).
-define(WRITE_EXT_INQUIRY_RESPONSE_RP_SIZE, 1).
-define(OCF_REFRESH_ENCRYPTION_KEY, 83).
-record(refresh_encryption_key_cp, {
  handle
}).
-define(refresh_encryption_key_cp_bin(Handle),Handle:1/little-unsigned-unit:16).
-define(REFRESH_ENCRYPTION_KEY_CP_SIZE, 2).
-record(refresh_encryption_key_rp, {
  status
}).
-define(refresh_encryption_key_rp_bin(Status),Status:1/unsigned-unit:8).
-define(REFRESH_ENCRYPTION_KEY_RP_SIZE, 1).
-define(OCF_READ_SIMPLE_PAIRING_MODE, 85).
-record(read_simple_pairing_mode_rp, {
  status,
  mode
}).
-define(read_simple_pairing_mode_rp_bin(Status,Mode),Status:1/unsigned-unit:8,Mode:1/unsigned-unit:8).
-define(READ_SIMPLE_PAIRING_MODE_RP_SIZE, 2).
-define(OCF_WRITE_SIMPLE_PAIRING_MODE, 86).
-record(write_simple_pairing_mode_cp, {
  mode
}).
-define(write_simple_pairing_mode_cp_bin(Mode),Mode:1/unsigned-unit:8).
-define(WRITE_SIMPLE_PAIRING_MODE_CP_SIZE, 1).
-record(write_simple_pairing_mode_rp, {
  status
}).
-define(write_simple_pairing_mode_rp_bin(Status),Status:1/unsigned-unit:8).
-define(WRITE_SIMPLE_PAIRING_MODE_RP_SIZE, 1).
-define(OCF_READ_LOCAL_OOB_DATA, 87).
-record(read_local_oob_data_rp, {
  status,
  hash,
  randomizer
}).
-define(read_local_oob_data_rp_bin(Status,Hash,Randomizer),Status:1/unsigned-unit:8,Hash:16/unit:8-binary,Randomizer:16/unit:8-binary).
-define(READ_LOCAL_OOB_DATA_RP_SIZE, 33).
-define(OCF_READ_INQ_RESPONSE_TX_POWER_LEVEL, 88).
-record(read_inq_response_tx_power_level_rp, {
  status,
  level
}).
-define(read_inq_response_tx_power_level_rp_bin(Status,Level),Status:1/unsigned-unit:8,Level:1/signed-unit:8).
-define(READ_INQ_RESPONSE_TX_POWER_LEVEL_RP_SIZE, 2).
-define(OCF_READ_INQUIRY_TRANSMIT_POWER_LEVEL, 88).
-record(read_inquiry_transmit_power_level_rp, {
  status,
  level
}).
-define(read_inquiry_transmit_power_level_rp_bin(Status,Level),Status:1/unsigned-unit:8,Level:1/signed-unit:8).
-define(READ_INQUIRY_TRANSMIT_POWER_LEVEL_RP_SIZE, 2).
-define(OCF_WRITE_INQUIRY_TRANSMIT_POWER_LEVEL, 89).
-record(write_inquiry_transmit_power_level_cp, {
  level
}).
-define(write_inquiry_transmit_power_level_cp_bin(Level),Level:1/signed-unit:8).
-define(WRITE_INQUIRY_TRANSMIT_POWER_LEVEL_CP_SIZE, 1).
-record(write_inquiry_transmit_power_level_rp, {
  status
}).
-define(write_inquiry_transmit_power_level_rp_bin(Status),Status:1/unsigned-unit:8).
-define(WRITE_INQUIRY_TRANSMIT_POWER_LEVEL_RP_SIZE, 1).
-define(OCF_READ_DEFAULT_ERROR_DATA_REPORTING, 90).
-record(read_default_error_data_reporting_rp, {
  status,
  reporting
}).
-define(read_default_error_data_reporting_rp_bin(Status,Reporting),Status:1/unsigned-unit:8,Reporting:1/unsigned-unit:8).
-define(READ_DEFAULT_ERROR_DATA_REPORTING_RP_SIZE, 2).
-define(OCF_WRITE_DEFAULT_ERROR_DATA_REPORTING, 91).
-record(write_default_error_data_reporting_cp, {
  reporting
}).
-define(write_default_error_data_reporting_cp_bin(Reporting),Reporting:1/unsigned-unit:8).
-define(WRITE_DEFAULT_ERROR_DATA_REPORTING_CP_SIZE, 1).
-record(write_default_error_data_reporting_rp, {
  status
}).
-define(write_default_error_data_reporting_rp_bin(Status),Status:1/unsigned-unit:8).
-define(WRITE_DEFAULT_ERROR_DATA_REPORTING_RP_SIZE, 1).
-define(OCF_ENHANCED_FLUSH, 95).
-record(enhanced_flush_cp, {
  handle,
  type
}).
-define(enhanced_flush_cp_bin(Handle,Type),Handle:1/little-unsigned-unit:16,Type:1/unsigned-unit:8).
-define(ENHANCED_FLUSH_CP_SIZE, 3).
-define(OCF_SEND_KEYPRESS_NOTIFY, 96).
-record(send_keypress_notify_cp, {
  bdaddr,
  type
}).
-define(send_keypress_notify_cp_bin(Bdaddr,Type),Bdaddr:6/unit:8-binary,Type:1/unsigned-unit:8).
-define(SEND_KEYPRESS_NOTIFY_CP_SIZE, 7).
-record(send_keypress_notify_rp, {
  status
}).
-define(send_keypress_notify_rp_bin(Status),Status:1/unsigned-unit:8).
-define(SEND_KEYPRESS_NOTIFY_RP_SIZE, 1).
-define(OCF_READ_LOGICAL_LINK_ACCEPT_TIMEOUT, 97).
-record(read_log_link_accept_timeout_rp, {
  status,
  timeout
}).
-define(read_log_link_accept_timeout_rp_bin(Status,Timeout),Status:1/unsigned-unit:8,Timeout:1/little-unsigned-unit:16).
-define(READ_LOGICAL_LINK_ACCEPT_TIMEOUT_RP_SIZE, 3).
-define(OCF_WRITE_LOGICAL_LINK_ACCEPT_TIMEOUT, 98).
-record(write_log_link_accept_timeout_cp, {
  timeout
}).
-define(write_log_link_accept_timeout_cp_bin(Timeout),Timeout:1/little-unsigned-unit:16).
-define(WRITE_LOGICAL_LINK_ACCEPT_TIMEOUT_CP_SIZE, 2).
-define(OCF_SET_EVENT_MASK_PAGE_2, 99).
-define(OCF_READ_LOCATION_DATA, 100).
-define(OCF_WRITE_LOCATION_DATA, 101).
-define(OCF_READ_FLOW_CONTROL_MODE, 102).
-define(OCF_WRITE_FLOW_CONTROL_MODE, 103).
-define(OCF_READ_ENHANCED_TRANSMIT_POWER_LEVEL, 104).
-record(read_enhanced_transmit_power_level_rp, {
  status,
  handle,
  level_gfsk,
  level_dqpsk,
  level_8dpsk
}).
-define(read_enhanced_transmit_power_level_rp_bin(Status,Handle,Level_gfsk,Level_dqpsk,Level_8dpsk),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Level_gfsk:1/signed-unit:8,Level_dqpsk:1/signed-unit:8,Level_8dpsk:1/signed-unit:8).
-define(READ_ENHANCED_TRANSMIT_POWER_LEVEL_RP_SIZE, 6).
-define(OCF_READ_BEST_EFFORT_FLUSH_TIMEOUT, 105).
-record(read_best_effort_flush_timeout_rp, {
  status,
  timeout
}).
-define(read_best_effort_flush_timeout_rp_bin(Status,Timeout),Status:1/unsigned-unit:8,Timeout:1/little-unsigned-unit:32).
-define(READ_BEST_EFFORT_FLUSH_TIMEOUT_RP_SIZE, 5).
-define(OCF_WRITE_BEST_EFFORT_FLUSH_TIMEOUT, 106).
-record(write_best_effort_flush_timeout_cp, {
  handle,
  timeout
}).
-define(write_best_effort_flush_timeout_cp_bin(Handle,Timeout),Handle:1/little-unsigned-unit:16,Timeout:1/little-unsigned-unit:32).
-define(WRITE_BEST_EFFORT_FLUSH_TIMEOUT_CP_SIZE, 6).
-record(write_best_effort_flush_timeout_rp, {
  status
}).
-define(write_best_effort_flush_timeout_rp_bin(Status),Status:1/unsigned-unit:8).
-define(WRITE_BEST_EFFORT_FLUSH_TIMEOUT_RP_SIZE, 1).
-define(OCF_READ_LE_HOST_SUPPORTED, 108).
-record(read_le_host_supported_rp, {
  status,
  le,
  simul
}).
-define(read_le_host_supported_rp_bin(Status,Le,Simul),Status:1/unsigned-unit:8,Le:1/unsigned-unit:8,Simul:1/unsigned-unit:8).
-define(READ_LE_HOST_SUPPORTED_RP_SIZE, 3).
-define(OCF_WRITE_LE_HOST_SUPPORTED, 109).
-record(write_le_host_supported_cp, {
  le,
  simul
}).
-define(write_le_host_supported_cp_bin(Le,Simul),Le:1/unsigned-unit:8,Simul:1/unsigned-unit:8).
-define(WRITE_LE_HOST_SUPPORTED_CP_SIZE, 2).
-define(OGF_INFO_PARAM, 4).
-define(OCF_READ_LOCAL_VERSION, 1).
-record(read_local_version_rp, {
  status,
  hci_ver,
  hci_rev,
  lmp_ver,
  manufacturer,
  lmp_subver
}).
-define(read_local_version_rp_bin(Status,Hci_ver,Hci_rev,Lmp_ver,Manufacturer,Lmp_subver),Status:1/unsigned-unit:8,Hci_ver:1/unsigned-unit:8,Hci_rev:1/little-unsigned-unit:16,Lmp_ver:1/unsigned-unit:8,Manufacturer:1/little-unsigned-unit:16,Lmp_subver:1/little-unsigned-unit:16).
-define(READ_LOCAL_VERSION_RP_SIZE, 9).
-define(OCF_READ_LOCAL_COMMANDS, 2).
-record(read_local_commands_rp, {
  status,
  commands
}).
-define(read_local_commands_rp_bin(Status,Commands),Status:1/unsigned-unit:8,Commands:64/unit:8-binary).
-define(READ_LOCAL_COMMANDS_RP_SIZE, 65).
-define(OCF_READ_LOCAL_FEATURES, 3).
-record(read_local_features_rp, {
  status,
  features
}).
-define(read_local_features_rp_bin(Status,Features),Status:1/unsigned-unit:8,Features:8/unit:8-binary).
-define(READ_LOCAL_FEATURES_RP_SIZE, 9).
-define(OCF_READ_LOCAL_EXT_FEATURES, 4).
-record(read_local_ext_features_cp, {
  page_num
}).
-define(read_local_ext_features_cp_bin(Page_num),Page_num:1/unsigned-unit:8).
-define(READ_LOCAL_EXT_FEATURES_CP_SIZE, 1).
-record(read_local_ext_features_rp, {
  status,
  page_num,
  max_page_num,
  features
}).
-define(read_local_ext_features_rp_bin(Status,Page_num,Max_page_num,Features),Status:1/unsigned-unit:8,Page_num:1/unsigned-unit:8,Max_page_num:1/unsigned-unit:8,Features:8/unit:8-binary).
-define(READ_LOCAL_EXT_FEATURES_RP_SIZE, 11).
-define(OCF_READ_BUFFER_SIZE, 5).
-record(read_buffer_size_rp, {
  status,
  acl_mtu,
  sco_mtu,
  acl_max_pkt,
  sco_max_pkt
}).
-define(read_buffer_size_rp_bin(Status,Acl_mtu,Sco_mtu,Acl_max_pkt,Sco_max_pkt),Status:1/unsigned-unit:8,Acl_mtu:1/little-unsigned-unit:16,Sco_mtu:1/unsigned-unit:8,Acl_max_pkt:1/little-unsigned-unit:16,Sco_max_pkt:1/little-unsigned-unit:16).
-define(READ_BUFFER_SIZE_RP_SIZE, 8).
-define(OCF_READ_COUNTRY_CODE, 7).
-define(OCF_READ_BD_ADDR, 9).
-record(read_bd_addr_rp, {
  status,
  bdaddr
}).
-define(read_bd_addr_rp_bin(Status,Bdaddr),Status:1/unsigned-unit:8,Bdaddr:6/unit:8-binary).
-define(READ_BD_ADDR_RP_SIZE, 7).
-define(OGF_STATUS_PARAM, 5).
-define(OCF_READ_FAILED_CONTACT_COUNTER, 1).
-record(read_failed_contact_counter_rp, {
  status,
  handle,
  counter
}).
-define(read_failed_contact_counter_rp_bin(Status,Handle,Counter),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Counter:1/unsigned-unit:8).
-define(READ_FAILED_CONTACT_COUNTER_RP_SIZE, 4).
-define(OCF_RESET_FAILED_CONTACT_COUNTER, 2).
-record(reset_failed_contact_counter_rp, {
  status,
  handle
}).
-define(reset_failed_contact_counter_rp_bin(Status,Handle),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16).
-define(RESET_FAILED_CONTACT_COUNTER_RP_SIZE, 4).
-define(OCF_READ_LINK_QUALITY, 3).
-record(read_link_quality_rp, {
  status,
  handle,
  link_quality
}).
-define(read_link_quality_rp_bin(Status,Handle,Link_quality),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Link_quality:1/unsigned-unit:8).
-define(READ_LINK_QUALITY_RP_SIZE, 4).
-define(OCF_READ_RSSI, 5).
-record(read_rssi_rp, {
  status,
  handle,
  rssi
}).
-define(read_rssi_rp_bin(Status,Handle,Rssi),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Rssi:1/signed-unit:8).
-define(READ_RSSI_RP_SIZE, 4).
-define(OCF_READ_AFH_MAP, 6).
-record(read_afh_map_rp, {
  status,
  handle,
  mode,
  map
}).
-define(read_afh_map_rp_bin(Status,Handle,Mode,Map),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Mode:1/unsigned-unit:8,Map:10/unit:8-binary).
-define(READ_AFH_MAP_RP_SIZE, 14).
-define(OCF_READ_CLOCK, 7).
-record(read_clock_cp, {
  handle,
  which_clock
}).
-define(read_clock_cp_bin(Handle,Which_clock),Handle:1/little-unsigned-unit:16,Which_clock:1/unsigned-unit:8).
-define(READ_CLOCK_CP_SIZE, 3).
-record(read_clock_rp, {
  status,
  handle,
  clock,
  accuracy
}).
-define(read_clock_rp_bin(Status,Handle,Clock,Accuracy),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Clock:1/little-unsigned-unit:32,Accuracy:1/little-unsigned-unit:16).
-define(READ_CLOCK_RP_SIZE, 9).
-define(OCF_READ_LOCAL_AMP_INFO, 9).
-record(read_local_amp_info_rp, {
  status,
  amp_status,
  total_bandwidth,
  max_guaranteed_bandwidth,
  min_latency,
  max_pdu_size,
  controller_type,
  pal_caps,
  max_amp_assoc_length,
  max_flush_timeout,
  best_effort_flush_timeout
}).
-define(read_local_amp_info_rp_bin(Status,Amp_status,Total_bandwidth,Max_guaranteed_bandwidth,Min_latency,Max_pdu_size,Controller_type,Pal_caps,Max_amp_assoc_length,Max_flush_timeout,Best_effort_flush_timeout),Status:1/unsigned-unit:8,Amp_status:1/unsigned-unit:8,Total_bandwidth:1/little-unsigned-unit:32,Max_guaranteed_bandwidth:1/little-unsigned-unit:32,Min_latency:1/little-unsigned-unit:32,Max_pdu_size:1/little-unsigned-unit:32,Controller_type:1/unsigned-unit:8,Pal_caps:1/little-unsigned-unit:16,Max_amp_assoc_length:1/little-unsigned-unit:16,Max_flush_timeout:1/little-unsigned-unit:32,Best_effort_flush_timeout:1/little-unsigned-unit:32).
-define(READ_LOCAL_AMP_INFO_RP_SIZE, 31).
-define(OCF_READ_LOCAL_AMP_ASSOC, 10).
-record(read_local_amp_assoc_cp, {
  handle,
  len_so_far,
  max_len
}).
-define(read_local_amp_assoc_cp_bin(Handle,Len_so_far,Max_len),Handle:1/unsigned-unit:8,Len_so_far:1/little-unsigned-unit:16,Max_len:1/little-unsigned-unit:16).
-record(read_local_amp_assoc_rp, {
  status,
  handle,
  rem_len,
  frag
}).
-define(read_local_amp_assoc_rp_bin(Status,Handle,Rem_len,Frag),Status:1/unsigned-unit:8,Handle:1/unsigned-unit:8,Rem_len:1/little-unsigned-unit:16,Frag:0/unit:8-binary).
-define(OCF_WRITE_REMOTE_AMP_ASSOC, 11).
-record(write_remote_amp_assoc_cp, {
  handle,
  length_so_far,
  assoc_length,
  fragment
}).
-define(write_remote_amp_assoc_cp_bin(Handle,Length_so_far,Assoc_length,Fragment),Handle:1/unsigned-unit:8,Length_so_far:1/little-unsigned-unit:16,Assoc_length:1/little-unsigned-unit:16,Fragment:(?HCI_MAX_NAME_LENGTH)/unit:8-binary).
-define(WRITE_REMOTE_AMP_ASSOC_CP_SIZE, 253).
-record(write_remote_amp_assoc_rp, {
  status,
  handle
}).
-define(write_remote_amp_assoc_rp_bin(Status,Handle),Status:1/unsigned-unit:8,Handle:1/unsigned-unit:8).
-define(WRITE_REMOTE_AMP_ASSOC_RP_SIZE, 2).
-define(OGF_TESTING_CMD, 62).
-define(OCF_READ_LOOPBACK_MODE, 1).
-define(OCF_WRITE_LOOPBACK_MODE, 2).
-define(OCF_ENABLE_DEVICE_UNDER_TEST_MODE, 3).
-define(OCF_WRITE_SIMPLE_PAIRING_DEBUG_MODE, 4).
-record(write_simple_pairing_debug_mode_cp, {
  mode
}).
-define(write_simple_pairing_debug_mode_cp_bin(Mode),Mode:1/unsigned-unit:8).
-define(WRITE_SIMPLE_PAIRING_DEBUG_MODE_CP_SIZE, 1).
-record(write_simple_pairing_debug_mode_rp, {
  status
}).
-define(write_simple_pairing_debug_mode_rp_bin(Status),Status:1/unsigned-unit:8).
-define(WRITE_SIMPLE_PAIRING_DEBUG_MODE_RP_SIZE, 1).
-define(OGF_LE_CTL, 8).
-define(OCF_LE_SET_EVENT_MASK, 1).
-record(le_set_event_mask_cp, {
  mask
}).
-define(le_set_event_mask_cp_bin(Mask),Mask:8/unit:8-binary).
-define(LE_SET_EVENT_MASK_CP_SIZE, 8).
-define(OCF_LE_READ_BUFFER_SIZE, 2).
-record(le_read_buffer_size_rp, {
  status,
  pkt_len,
  max_pkt
}).
-define(le_read_buffer_size_rp_bin(Status,Pkt_len,Max_pkt),Status:1/unsigned-unit:8,Pkt_len:1/little-unsigned-unit:16,Max_pkt:1/unsigned-unit:8).
-define(LE_READ_BUFFER_SIZE_RP_SIZE, 4).
-define(OCF_LE_READ_LOCAL_SUPPORTED_FEATURES, 3).
-record(le_read_local_supported_features_rp, {
  status,
  features
}).
-define(le_read_local_supported_features_rp_bin(Status,Features),Status:1/unsigned-unit:8,Features:8/unit:8-binary).
-define(LE_READ_LOCAL_SUPPORTED_FEATURES_RP_SIZE, 9).
-define(OCF_LE_SET_RANDOM_ADDRESS, 5).
-record(le_set_random_address_cp, {
  bdaddr
}).
-define(le_set_random_address_cp_bin(Bdaddr),Bdaddr:6/unit:8-binary).
-define(LE_SET_RANDOM_ADDRESS_CP_SIZE, 6).
-define(OCF_LE_SET_ADVERTISING_PARAMETERS, 6).
-record(le_set_advertising_parameters_cp, {
  min_interval,
  max_interval,
  advtype,
  own_bdaddr_type,
  direct_bdaddr_type,
  direct_bdaddr,
  chan_map,
  filter
}).
-define(le_set_advertising_parameters_cp_bin(Min_interval,Max_interval,Advtype,Own_bdaddr_type,Direct_bdaddr_type,Direct_bdaddr,Chan_map,Filter),Min_interval:1/little-unsigned-unit:16,Max_interval:1/little-unsigned-unit:16,Advtype:1/unsigned-unit:8,Own_bdaddr_type:1/unsigned-unit:8,Direct_bdaddr_type:1/unsigned-unit:8,Direct_bdaddr:6/unit:8-binary,Chan_map:1/unsigned-unit:8,Filter:1/unsigned-unit:8).
-define(LE_SET_ADVERTISING_PARAMETERS_CP_SIZE, 15).
-define(OCF_LE_READ_ADVERTISING_CHANNEL_TX_POWER, 7).
-record(le_read_advertising_channel_tx_power_rp, {
  status,
  level
}).
-define(le_read_advertising_channel_tx_power_rp_bin(Status,Level),Status:1/unsigned-unit:8,Level:1/unsigned-unit:8).
-define(LE_READ_ADVERTISING_CHANNEL_TX_POWER_RP_SIZE, 2).
-define(OCF_LE_SET_ADVERTISING_DATA, 8).
-record(le_set_advertising_data_cp, {
  length,
  data
}).
-define(le_set_advertising_data_cp_bin(Length,Data),Length:1/unsigned-unit:8,Data:31/unit:8-binary).
-define(LE_SET_ADVERTISING_DATA_CP_SIZE, 32).
-define(OCF_LE_SET_SCAN_RESPONSE_DATA, 9).
-record(le_set_scan_response_data_cp, {
  length,
  data
}).
-define(le_set_scan_response_data_cp_bin(Length,Data),Length:1/unsigned-unit:8,Data:31/unit:8-binary).
-define(LE_SET_SCAN_RESPONSE_DATA_CP_SIZE, 32).
-define(OCF_LE_SET_ADVERTISE_ENABLE, 10).
-record(le_set_advertise_enable_cp, {
  enable
}).
-define(le_set_advertise_enable_cp_bin(Enable),Enable:1/unsigned-unit:8).
-define(LE_SET_ADVERTISE_ENABLE_CP_SIZE, 1).
-define(OCF_LE_SET_SCAN_PARAMETERS, 11).
-record(le_set_scan_parameters_cp, {
  type,
  interval,
  window,
  own_bdaddr_type,
  filter
}).
-define(le_set_scan_parameters_cp_bin(Type,Interval,Window,Own_bdaddr_type,Filter),Type:1/unsigned-unit:8,Interval:1/little-unsigned-unit:16,Window:1/little-unsigned-unit:16,Own_bdaddr_type:1/unsigned-unit:8,Filter:1/unsigned-unit:8).
-define(LE_SET_SCAN_PARAMETERS_CP_SIZE, 7).
-define(OCF_LE_SET_SCAN_ENABLE, 12).
-record(le_set_scan_enable_cp, {
  enable,
  filter_dup
}).
-define(le_set_scan_enable_cp_bin(Enable,Filter_dup),Enable:1/unsigned-unit:8,Filter_dup:1/unsigned-unit:8).
-define(LE_SET_SCAN_ENABLE_CP_SIZE, 2).
-define(OCF_LE_CREATE_CONN, 13).
-record(le_create_connection_cp, {
  interval,
  window,
  initiator_filter,
  peer_bdaddr_type,
  peer_bdaddr,
  own_bdaddr_type,
  min_interval,
  max_interval,
  latency,
  supervision_timeout,
  min_ce_length,
  max_ce_length
}).
-define(le_create_connection_cp_bin(Interval,Window,Initiator_filter,Peer_bdaddr_type,Peer_bdaddr,Own_bdaddr_type,Min_interval,Max_interval,Latency,Supervision_timeout,Min_ce_length,Max_ce_length),Interval:1/little-unsigned-unit:16,Window:1/little-unsigned-unit:16,Initiator_filter:1/unsigned-unit:8,Peer_bdaddr_type:1/unsigned-unit:8,Peer_bdaddr:6/unit:8-binary,Own_bdaddr_type:1/unsigned-unit:8,Min_interval:1/little-unsigned-unit:16,Max_interval:1/little-unsigned-unit:16,Latency:1/little-unsigned-unit:16,Supervision_timeout:1/little-unsigned-unit:16,Min_ce_length:1/little-unsigned-unit:16,Max_ce_length:1/little-unsigned-unit:16).
-define(LE_CREATE_CONN_CP_SIZE, 25).
-define(OCF_LE_CREATE_CONN_CANCEL, 14).
-define(OCF_LE_READ_WHITE_LIST_SIZE, 15).
-record(le_read_white_list_size_rp, {
  status,
  size
}).
-define(le_read_white_list_size_rp_bin(Status,Size),Status:1/unsigned-unit:8,Size:1/unsigned-unit:8).
-define(LE_READ_WHITE_LIST_SIZE_RP_SIZE, 2).
-define(OCF_LE_CLEAR_WHITE_LIST, 16).
-define(OCF_LE_ADD_DEVICE_TO_WHITE_LIST, 17).
-record(le_add_device_to_white_list_cp, {
  bdaddr_type,
  bdaddr
}).
-define(le_add_device_to_white_list_cp_bin(Bdaddr_type,Bdaddr),Bdaddr_type:1/unsigned-unit:8,Bdaddr:6/unit:8-binary).
-define(LE_ADD_DEVICE_TO_WHITE_LIST_CP_SIZE, 7).
-define(OCF_LE_REMOVE_DEVICE_FROM_WHITE_LIST, 18).
-record(le_remove_device_from_white_list_cp, {
  bdaddr_type,
  bdaddr
}).
-define(le_remove_device_from_white_list_cp_bin(Bdaddr_type,Bdaddr),Bdaddr_type:1/unsigned-unit:8,Bdaddr:6/unit:8-binary).
-define(LE_REMOVE_DEVICE_FROM_WHITE_LIST_CP_SIZE, 7).
-define(OCF_LE_CONN_UPDATE, 19).
-record(le_connection_update_cp, {
  handle,
  min_interval,
  max_interval,
  latency,
  supervision_timeout,
  min_ce_length,
  max_ce_length
}).
-define(le_connection_update_cp_bin(Handle,Min_interval,Max_interval,Latency,Supervision_timeout,Min_ce_length,Max_ce_length),Handle:1/little-unsigned-unit:16,Min_interval:1/little-unsigned-unit:16,Max_interval:1/little-unsigned-unit:16,Latency:1/little-unsigned-unit:16,Supervision_timeout:1/little-unsigned-unit:16,Min_ce_length:1/little-unsigned-unit:16,Max_ce_length:1/little-unsigned-unit:16).
-define(LE_CONN_UPDATE_CP_SIZE, 14).
-define(OCF_LE_SET_HOST_CHANNEL_CLASSIFICATION, 20).
-record(le_set_host_channel_classification_cp, {
  map
}).
-define(le_set_host_channel_classification_cp_bin(Map),Map:5/unit:8-binary).
-define(LE_SET_HOST_CHANNEL_CLASSIFICATION_CP_SIZE, 5).
-define(OCF_LE_READ_CHANNEL_MAP, 21).
-record(le_read_channel_map_cp, {
  handle
}).
-define(le_read_channel_map_cp_bin(Handle),Handle:1/little-unsigned-unit:16).
-define(LE_READ_CHANNEL_MAP_CP_SIZE, 2).
-record(le_read_channel_map_rp, {
  status,
  handle,
  map
}).
-define(le_read_channel_map_rp_bin(Status,Handle,Map),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Map:5/unit:8-binary).
-define(LE_READ_CHANNEL_MAP_RP_SIZE, 8).
-define(OCF_LE_READ_REMOTE_USED_FEATURES, 22).
-record(le_read_remote_used_features_cp, {
  handle
}).
-define(le_read_remote_used_features_cp_bin(Handle),Handle:1/little-unsigned-unit:16).
-define(LE_READ_REMOTE_USED_FEATURES_CP_SIZE, 2).
-define(OCF_LE_ENCRYPT, 23).
-record(le_encrypt_cp, {
  key,
  plaintext
}).
-define(le_encrypt_cp_bin(Key,Plaintext),Key:16/unit:8-binary,Plaintext:16/unit:8-binary).
-define(LE_ENCRYPT_CP_SIZE, 32).
-record(le_encrypt_rp, {
  status,
  data
}).
-define(le_encrypt_rp_bin(Status,Data),Status:1/unsigned-unit:8,Data:16/unit:8-binary).
-define(LE_ENCRYPT_RP_SIZE, 17).
-define(OCF_LE_RAND, 24).
-record(le_rand_rp, {
  status,
  random
}).
-define(le_rand_rp_bin(Status,Random),Status:1/unsigned-unit:8,Random:1/little-unsigned-unit:64).
-define(LE_RAND_RP_SIZE, 9).
-define(OCF_LE_START_ENCRYPTION, 25).
-record(le_start_encryption_cp, {
  handle,
  random,
  diversifier,
  key
}).
-define(le_start_encryption_cp_bin(Handle,Random,Diversifier,Key),Handle:1/little-unsigned-unit:16,Random:1/little-unsigned-unit:64,Diversifier:1/little-unsigned-unit:16,Key:16/unit:8-binary).
-define(LE_START_ENCRYPTION_CP_SIZE, 28).
-define(OCF_LE_LTK_REPLY, 26).
-record(le_ltk_reply_cp, {
  handle,
  key
}).
-define(le_ltk_reply_cp_bin(Handle,Key),Handle:1/little-unsigned-unit:16,Key:16/unit:8-binary).
-define(LE_LTK_REPLY_CP_SIZE, 18).
-record(le_ltk_reply_rp, {
  status,
  handle
}).
-define(le_ltk_reply_rp_bin(Status,Handle),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16).
-define(LE_LTK_REPLY_RP_SIZE, 3).
-define(OCF_LE_LTK_NEG_REPLY, 27).
-record(le_ltk_neg_reply_cp, {
  handle
}).
-define(le_ltk_neg_reply_cp_bin(Handle),Handle:1/little-unsigned-unit:16).
-define(LE_LTK_NEG_REPLY_CP_SIZE, 2).
-record(le_ltk_neg_reply_rp, {
  status,
  handle
}).
-define(le_ltk_neg_reply_rp_bin(Status,Handle),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16).
-define(LE_LTK_NEG_REPLY_RP_SIZE, 3).
-define(OCF_LE_READ_SUPPORTED_STATES, 28).
-record(le_read_supported_states_rp, {
  status,
  states
}).
-define(le_read_supported_states_rp_bin(Status,States),Status:1/unsigned-unit:8,States:1/little-unsigned-unit:64).
-define(LE_READ_SUPPORTED_STATES_RP_SIZE, 9).
-define(OCF_LE_RECEIVER_TEST, 29).
-record(le_receiver_test_cp, {
  frequency
}).
-define(le_receiver_test_cp_bin(Frequency),Frequency:1/unsigned-unit:8).
-define(LE_RECEIVER_TEST_CP_SIZE, 1).
-define(OCF_LE_TRANSMITTER_TEST, 30).
-record(le_transmitter_test_cp, {
  frequency,
  length,
  payload
}).
-define(le_transmitter_test_cp_bin(Frequency,Length,Payload),Frequency:1/unsigned-unit:8,Length:1/unsigned-unit:8,Payload:1/unsigned-unit:8).
-define(LE_TRANSMITTER_TEST_CP_SIZE, 3).
-define(OCF_LE_TEST_END, 31).
-record(le_test_end_rp, {
  status,
  num_pkts
}).
-define(le_test_end_rp_bin(Status,Num_pkts),Status:1/unsigned-unit:8,Num_pkts:1/little-unsigned-unit:16).
-define(LE_TEST_END_RP_SIZE, 3).
-define(OGF_VENDOR_CMD, 63).
-define(EVT_INQUIRY_COMPLETE, 1).
-define(EVT_INQUIRY_RESULT, 2).
-record(inquiry_info, {
  bdaddr,
  pscan_rep_mode,
  pscan_period_mode,
  pscan_mode,
  dev_class,
  clock_offset
}).
-define(inquiry_info_bin(Bdaddr,Pscan_rep_mode,Pscan_period_mode,Pscan_mode,Dev_class,Clock_offset),Bdaddr:6/unit:8-binary,Pscan_rep_mode:1/unsigned-unit:8,Pscan_period_mode:1/unsigned-unit:8,Pscan_mode:1/unsigned-unit:8,Dev_class:3/unit:8-binary,Clock_offset:1/little-unsigned-unit:16).
-define(INQUIRY_INFO_SIZE, 14).
-define(EVT_CONN_COMPLETE, 3).
-record(evt_conn_complete, {
  status,
  handle,
  bdaddr,
  link_type,
  encr_mode
}).
-define(evt_conn_complete_bin(Status,Handle,Bdaddr,Link_type,Encr_mode),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Bdaddr:6/unit:8-binary,Link_type:1/unsigned-unit:8,Encr_mode:1/unsigned-unit:8).
-define(EVT_CONN_COMPLETE_SIZE, 13).
-define(EVT_CONN_REQUEST, 4).
-record(evt_conn_request, {
  bdaddr,
  dev_class,
  link_type
}).
-define(evt_conn_request_bin(Bdaddr,Dev_class,Link_type),Bdaddr:6/unit:8-binary,Dev_class:3/unit:8-binary,Link_type:1/unsigned-unit:8).
-define(EVT_CONN_REQUEST_SIZE, 10).
-define(EVT_DISCONN_COMPLETE, 5).
-record(evt_disconn_complete, {
  status,
  handle,
  reason
}).
-define(evt_disconn_complete_bin(Status,Handle,Reason),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Reason:1/unsigned-unit:8).
-define(EVT_DISCONN_COMPLETE_SIZE, 4).
-define(EVT_AUTH_COMPLETE, 6).
-record(evt_auth_complete, {
  status,
  handle
}).
-define(evt_auth_complete_bin(Status,Handle),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16).
-define(EVT_AUTH_COMPLETE_SIZE, 3).
-define(EVT_REMOTE_NAME_REQ_COMPLETE, 7).
-record(evt_remote_name_req_complete, {
  status,
  bdaddr,
  name
}).
-define(evt_remote_name_req_complete_bin(Status,Bdaddr,Name),Status:1/unsigned-unit:8,Bdaddr:6/unit:8-binary,Name:(?HCI_MAX_NAME_LENGTH)/unit:8-binary).
-define(EVT_REMOTE_NAME_REQ_COMPLETE_SIZE, 255).
-define(EVT_ENCRYPT_CHANGE, 8).
-record(evt_encrypt_change, {
  status,
  handle,
  encrypt
}).
-define(evt_encrypt_change_bin(Status,Handle,Encrypt),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Encrypt:1/unsigned-unit:8).
-define(EVT_ENCRYPT_CHANGE_SIZE, 5).
-define(EVT_CHANGE_CONN_LINK_KEY_COMPLETE, 9).
-record(evt_change_conn_link_key_complete, {
  status,
  handle
}).
-define(evt_change_conn_link_key_complete_bin(Status,Handle),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16).
-define(EVT_CHANGE_CONN_LINK_KEY_COMPLETE_SIZE, 3).
-define(EVT_MASTER_LINK_KEY_COMPLETE, 10).
-record(evt_master_link_key_complete, {
  status,
  handle,
  key_flag
}).
-define(evt_master_link_key_complete_bin(Status,Handle,Key_flag),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Key_flag:1/unsigned-unit:8).
-define(EVT_MASTER_LINK_KEY_COMPLETE_SIZE, 4).
-define(EVT_READ_REMOTE_FEATURES_COMPLETE, 11).
-record(evt_read_remote_features_complete, {
  status,
  handle,
  features
}).
-define(evt_read_remote_features_complete_bin(Status,Handle,Features),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Features:8/unit:8-binary).
-define(EVT_READ_REMOTE_FEATURES_COMPLETE_SIZE, 11).
-define(EVT_READ_REMOTE_VERSION_COMPLETE, 12).
-record(evt_read_remote_version_complete, {
  status,
  handle,
  lmp_ver,
  manufacturer,
  lmp_subver
}).
-define(evt_read_remote_version_complete_bin(Status,Handle,Lmp_ver,Manufacturer,Lmp_subver),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Lmp_ver:1/unsigned-unit:8,Manufacturer:1/little-unsigned-unit:16,Lmp_subver:1/little-unsigned-unit:16).
-define(EVT_READ_REMOTE_VERSION_COMPLETE_SIZE, 8).
-define(EVT_QOS_SETUP_COMPLETE, 13).
-record(evt_qos_setup_complete, {
  status,
  handle,
  flags,
  qos
}).
-define(evt_qos_setup_complete_bin(Status,Handle,Flags,Qos),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Flags:1/unsigned-unit:8,Qos:17/unit:8-binary).
-define(EVT_QOS_SETUP_COMPLETE_SIZE, (4 + ?HCI_QOS_CP_SIZE)).
-define(EVT_CMD_COMPLETE, 14).
-record(evt_cmd_complete, {
  ncmd,
  opcode
}).
-define(evt_cmd_complete_bin(Ncmd,Opcode),Ncmd:1/unsigned-unit:8,Opcode:1/little-unsigned-unit:16).
-define(EVT_CMD_COMPLETE_SIZE, 3).
-define(EVT_CMD_STATUS, 15).
-record(evt_cmd_status, {
  status,
  ncmd,
  opcode
}).
-define(evt_cmd_status_bin(Status,Ncmd,Opcode),Status:1/unsigned-unit:8,Ncmd:1/unsigned-unit:8,Opcode:1/little-unsigned-unit:16).
-define(EVT_CMD_STATUS_SIZE, 4).
-define(EVT_HARDWARE_ERROR, 16).
-record(evt_hardware_error, {
  code
}).
-define(evt_hardware_error_bin(Code),Code:1/unsigned-unit:8).
-define(EVT_HARDWARE_ERROR_SIZE, 1).
-define(EVT_FLUSH_OCCURRED, 17).
-record(evt_flush_occured, {
  handle
}).
-define(evt_flush_occured_bin(Handle),Handle:1/little-unsigned-unit:16).
-define(EVT_FLUSH_OCCURRED_SIZE, 2).
-define(EVT_ROLE_CHANGE, 18).
-record(evt_role_change, {
  status,
  bdaddr,
  role
}).
-define(evt_role_change_bin(Status,Bdaddr,Role),Status:1/unsigned-unit:8,Bdaddr:6/unit:8-binary,Role:1/unsigned-unit:8).
-define(EVT_ROLE_CHANGE_SIZE, 8).
-define(EVT_NUM_COMP_PKTS, 19).
-record(evt_num_comp_pkts, {
  num_hndl
}).
-define(evt_num_comp_pkts_bin(Num_hndl),Num_hndl:1/unsigned-unit:8).
-define(EVT_NUM_COMP_PKTS_SIZE, 1).
-define(EVT_MODE_CHANGE, 20).
-record(evt_mode_change, {
  status,
  handle,
  mode,
  interval
}).
-define(evt_mode_change_bin(Status,Handle,Mode,Interval),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Mode:1/unsigned-unit:8,Interval:1/little-unsigned-unit:16).
-define(EVT_MODE_CHANGE_SIZE, 6).
-define(EVT_RETURN_LINK_KEYS, 21).
-record(evt_return_link_keys, {
  num_keys
}).
-define(evt_return_link_keys_bin(Num_keys),Num_keys:1/unsigned-unit:8).
-define(EVT_RETURN_LINK_KEYS_SIZE, 1).
-define(EVT_PIN_CODE_REQ, 22).
-record(evt_pin_code_req, {
  bdaddr
}).
-define(evt_pin_code_req_bin(Bdaddr),Bdaddr:6/unit:8-binary).
-define(EVT_PIN_CODE_REQ_SIZE, 6).
-define(EVT_LINK_KEY_REQ, 23).
-record(evt_link_key_req, {
  bdaddr
}).
-define(evt_link_key_req_bin(Bdaddr),Bdaddr:6/unit:8-binary).
-define(EVT_LINK_KEY_REQ_SIZE, 6).
-define(EVT_LINK_KEY_NOTIFY, 24).
-record(evt_link_key_notify, {
  bdaddr,
  link_key,
  key_type
}).
-define(evt_link_key_notify_bin(Bdaddr,Link_key,Key_type),Bdaddr:6/unit:8-binary,Link_key:16/unit:8-binary,Key_type:1/unsigned-unit:8).
-define(EVT_LINK_KEY_NOTIFY_SIZE, 23).
-define(EVT_LOOPBACK_COMMAND, 25).
-define(EVT_DATA_BUFFER_OVERFLOW, 26).
-record(evt_data_buffer_overflow, {
  link_type
}).
-define(evt_data_buffer_overflow_bin(Link_type),Link_type:1/unsigned-unit:8).
-define(EVT_DATA_BUFFER_OVERFLOW_SIZE, 1).
-define(EVT_MAX_SLOTS_CHANGE, 27).
-record(evt_max_slots_change, {
  handle,
  max_slots
}).
-define(evt_max_slots_change_bin(Handle,Max_slots),Handle:1/little-unsigned-unit:16,Max_slots:1/unsigned-unit:8).
-define(EVT_MAX_SLOTS_CHANGE_SIZE, 3).
-define(EVT_READ_CLOCK_OFFSET_COMPLETE, 28).
-record(evt_read_clock_offset_complete, {
  status,
  handle,
  clock_offset
}).
-define(evt_read_clock_offset_complete_bin(Status,Handle,Clock_offset),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Clock_offset:1/little-unsigned-unit:16).
-define(EVT_READ_CLOCK_OFFSET_COMPLETE_SIZE, 5).
-define(EVT_CONN_PTYPE_CHANGED, 29).
-record(evt_conn_ptype_changed, {
  status,
  handle,
  ptype
}).
-define(evt_conn_ptype_changed_bin(Status,Handle,Ptype),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Ptype:1/little-unsigned-unit:16).
-define(EVT_CONN_PTYPE_CHANGED_SIZE, 5).
-define(EVT_QOS_VIOLATION, 30).
-record(evt_qos_violation, {
  handle
}).
-define(evt_qos_violation_bin(Handle),Handle:1/little-unsigned-unit:16).
-define(EVT_QOS_VIOLATION_SIZE, 2).
-define(EVT_PSCAN_REP_MODE_CHANGE, 32).
-record(evt_pscan_rep_mode_change, {
  bdaddr,
  pscan_rep_mode
}).
-define(evt_pscan_rep_mode_change_bin(Bdaddr,Pscan_rep_mode),Bdaddr:6/unit:8-binary,Pscan_rep_mode:1/unsigned-unit:8).
-define(EVT_PSCAN_REP_MODE_CHANGE_SIZE, 7).
-define(EVT_FLOW_SPEC_COMPLETE, 33).
-record(evt_flow_spec_complete, {
  status,
  handle,
  flags,
  direction,
  qos
}).
-define(evt_flow_spec_complete_bin(Status,Handle,Flags,Direction,Qos),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Flags:1/unsigned-unit:8,Direction:1/unsigned-unit:8,Qos:17/unit:8-binary).
-define(EVT_FLOW_SPEC_COMPLETE_SIZE, (5 + ?HCI_QOS_CP_SIZE)).
-define(EVT_INQUIRY_RESULT_WITH_RSSI, 34).
-record(inquiry_info_with_rssi, {
  bdaddr,
  pscan_rep_mode,
  pscan_period_mode,
  dev_class,
  clock_offset,
  rssi
}).
-define(inquiry_info_with_rssi_bin(Bdaddr,Pscan_rep_mode,Pscan_period_mode,Dev_class,Clock_offset,Rssi),Bdaddr:6/unit:8-binary,Pscan_rep_mode:1/unsigned-unit:8,Pscan_period_mode:1/unsigned-unit:8,Dev_class:3/unit:8-binary,Clock_offset:1/little-unsigned-unit:16,Rssi:1/signed-unit:8).
-define(INQUIRY_INFO_WITH_RSSI_SIZE, 14).
-record(inquiry_info_with_rssi_and_pscan_mode, {
  bdaddr,
  pscan_rep_mode,
  pscan_period_mode,
  pscan_mode,
  dev_class,
  clock_offset,
  rssi
}).
-define(inquiry_info_with_rssi_and_pscan_mode_bin(Bdaddr,Pscan_rep_mode,Pscan_period_mode,Pscan_mode,Dev_class,Clock_offset,Rssi),Bdaddr:6/unit:8-binary,Pscan_rep_mode:1/unsigned-unit:8,Pscan_period_mode:1/unsigned-unit:8,Pscan_mode:1/unsigned-unit:8,Dev_class:3/unit:8-binary,Clock_offset:1/little-unsigned-unit:16,Rssi:1/signed-unit:8).
-define(INQUIRY_INFO_WITH_RSSI_AND_PSCAN_MODE_SIZE, 15).
-define(EVT_READ_REMOTE_EXT_FEATURES_COMPLETE, 35).
-record(evt_read_remote_ext_features_complete, {
  status,
  handle,
  page_num,
  max_page_num,
  features
}).
-define(evt_read_remote_ext_features_complete_bin(Status,Handle,Page_num,Max_page_num,Features),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Page_num:1/unsigned-unit:8,Max_page_num:1/unsigned-unit:8,Features:8/unit:8-binary).
-define(EVT_READ_REMOTE_EXT_FEATURES_COMPLETE_SIZE, 13).
-define(EVT_SYNC_CONN_COMPLETE, 44).
-record(evt_sync_conn_complete, {
  status,
  handle,
  bdaddr,
  link_type,
  trans_interval,
  retrans_window,
  rx_pkt_len,
  tx_pkt_len,
  air_mode
}).
-define(evt_sync_conn_complete_bin(Status,Handle,Bdaddr,Link_type,Trans_interval,Retrans_window,Rx_pkt_len,Tx_pkt_len,Air_mode),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Bdaddr:6/unit:8-binary,Link_type:1/unsigned-unit:8,Trans_interval:1/unsigned-unit:8,Retrans_window:1/unsigned-unit:8,Rx_pkt_len:1/little-unsigned-unit:16,Tx_pkt_len:1/little-unsigned-unit:16,Air_mode:1/unsigned-unit:8).
-define(EVT_SYNC_CONN_COMPLETE_SIZE, 17).
-define(EVT_SYNC_CONN_CHANGED, 45).
-record(evt_sync_conn_changed, {
  status,
  handle,
  trans_interval,
  retrans_window,
  rx_pkt_len,
  tx_pkt_len
}).
-define(evt_sync_conn_changed_bin(Status,Handle,Trans_interval,Retrans_window,Rx_pkt_len,Tx_pkt_len),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Trans_interval:1/unsigned-unit:8,Retrans_window:1/unsigned-unit:8,Rx_pkt_len:1/little-unsigned-unit:16,Tx_pkt_len:1/little-unsigned-unit:16).
-define(EVT_SYNC_CONN_CHANGED_SIZE, 9).
-define(EVT_SNIFF_SUBRATING, 46).
-record(evt_sniff_subrating, {
  status,
  handle,
  max_tx_latency,
  max_rx_latency,
  min_remote_timeout,
  min_local_timeout
}).
-define(evt_sniff_subrating_bin(Status,Handle,Max_tx_latency,Max_rx_latency,Min_remote_timeout,Min_local_timeout),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Max_tx_latency:1/little-unsigned-unit:16,Max_rx_latency:1/little-unsigned-unit:16,Min_remote_timeout:1/little-unsigned-unit:16,Min_local_timeout:1/little-unsigned-unit:16).
-define(EVT_SNIFF_SUBRATING_SIZE, 11).
-define(EVT_EXTENDED_INQUIRY_RESULT, 47).
-record(extended_inquiry_info, {
  bdaddr,
  pscan_rep_mode,
  pscan_period_mode,
  dev_class,
  clock_offset,
  rssi,
  data
}).
-define(extended_inquiry_info_bin(Bdaddr,Pscan_rep_mode,Pscan_period_mode,Dev_class,Clock_offset,Rssi,Data),Bdaddr:6/unit:8-binary,Pscan_rep_mode:1/unsigned-unit:8,Pscan_period_mode:1/unsigned-unit:8,Dev_class:3/unit:8-binary,Clock_offset:1/little-unsigned-unit:16,Rssi:1/signed-unit:8,Data:(?HCI_MAX_EIR_LENGTH)/unit:8-binary).
-define(EXTENDED_INQUIRY_INFO_SIZE, 254).
-define(EVT_ENCRYPTION_KEY_REFRESH_COMPLETE, 48).
-record(evt_encryption_key_refresh_complete, {
  status,
  handle
}).
-define(evt_encryption_key_refresh_complete_bin(Status,Handle),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16).
-define(EVT_ENCRYPTION_KEY_REFRESH_COMPLETE_SIZE, 3).
-define(EVT_IO_CAPABILITY_REQUEST, 49).
-record(evt_io_capability_request, {
  bdaddr
}).
-define(evt_io_capability_request_bin(Bdaddr),Bdaddr:6/unit:8-binary).
-define(EVT_IO_CAPABILITY_REQUEST_SIZE, 6).
-define(EVT_IO_CAPABILITY_RESPONSE, 50).
-record(evt_io_capability_response, {
  bdaddr,
  capability,
  oob_data,
  authentication
}).
-define(evt_io_capability_response_bin(Bdaddr,Capability,Oob_data,Authentication),Bdaddr:6/unit:8-binary,Capability:1/unsigned-unit:8,Oob_data:1/unsigned-unit:8,Authentication:1/unsigned-unit:8).
-define(EVT_IO_CAPABILITY_RESPONSE_SIZE, 9).
-define(EVT_USER_CONFIRM_REQUEST, 51).
-record(evt_user_confirm_request, {
  bdaddr,
  passkey
}).
-define(evt_user_confirm_request_bin(Bdaddr,Passkey),Bdaddr:6/unit:8-binary,Passkey:1/little-unsigned-unit:32).
-define(EVT_USER_CONFIRM_REQUEST_SIZE, 10).
-define(EVT_USER_PASSKEY_REQUEST, 52).
-record(evt_user_passkey_request, {
  bdaddr
}).
-define(evt_user_passkey_request_bin(Bdaddr),Bdaddr:6/unit:8-binary).
-define(EVT_USER_PASSKEY_REQUEST_SIZE, 6).
-define(EVT_REMOTE_OOB_DATA_REQUEST, 53).
-record(evt_remote_oob_data_request, {
  bdaddr
}).
-define(evt_remote_oob_data_request_bin(Bdaddr),Bdaddr:6/unit:8-binary).
-define(EVT_REMOTE_OOB_DATA_REQUEST_SIZE, 6).
-define(EVT_SIMPLE_PAIRING_COMPLETE, 54).
-record(evt_simple_pairing_complete, {
  status,
  bdaddr
}).
-define(evt_simple_pairing_complete_bin(Status,Bdaddr),Status:1/unsigned-unit:8,Bdaddr:6/unit:8-binary).
-define(EVT_SIMPLE_PAIRING_COMPLETE_SIZE, 7).
-define(EVT_LINK_SUPERVISION_TIMEOUT_CHANGED, 56).
-record(evt_link_supervision_timeout_changed, {
  handle,
  timeout
}).
-define(evt_link_supervision_timeout_changed_bin(Handle,Timeout),Handle:1/little-unsigned-unit:16,Timeout:1/little-unsigned-unit:16).
-define(EVT_LINK_SUPERVISION_TIMEOUT_CHANGED_SIZE, 4).
-define(EVT_ENHANCED_FLUSH_COMPLETE, 57).
-record(evt_enhanced_flush_complete, {
  handle
}).
-define(evt_enhanced_flush_complete_bin(Handle),Handle:1/little-unsigned-unit:16).
-define(EVT_ENHANCED_FLUSH_COMPLETE_SIZE, 2).
-define(EVT_USER_PASSKEY_NOTIFY, 59).
-record(evt_user_passkey_notify, {
  bdaddr,
  passkey,
  entered
}).
-define(evt_user_passkey_notify_bin(Bdaddr,Passkey,Entered),Bdaddr:6/unit:8-binary,Passkey:1/little-unsigned-unit:32,Entered:1/unsigned-unit:8).
-define(EVT_USER_PASSKEY_NOTIFY_SIZE, 11).
-define(EVT_KEYPRESS_NOTIFY, 60).
-record(evt_keypress_notify, {
  bdaddr,
  type
}).
-define(evt_keypress_notify_bin(Bdaddr,Type),Bdaddr:6/unit:8-binary,Type:1/unsigned-unit:8).
-define(EVT_KEYPRESS_NOTIFY_SIZE, 7).
-define(EVT_REMOTE_HOST_FEATURES_NOTIFY, 61).
-record(evt_remote_host_features_notify, {
  bdaddr,
  features
}).
-define(evt_remote_host_features_notify_bin(Bdaddr,Features),Bdaddr:6/unit:8-binary,Features:8/unit:8-binary).
-define(EVT_REMOTE_HOST_FEATURES_NOTIFY_SIZE, 14).
-define(EVT_LE_META_EVENT, 62).
-record(evt_le_meta_event, {
  subevent,
  data
}).
-define(evt_le_meta_event_bin(Subevent,Data),Subevent:1/unsigned-unit:8,Data:0/unit:8-binary).
-define(EVT_LE_META_EVENT_SIZE, 1).
-define(EVT_LE_CONN_COMPLETE, 1).
-record(evt_le_connection_complete, {
  status,
  handle,
  role,
  peer_bdaddr_type,
  peer_bdaddr,
  interval,
  latency,
  supervision_timeout,
  master_clock_accuracy
}).
-define(evt_le_connection_complete_bin(Status,Handle,Role,Peer_bdaddr_type,Peer_bdaddr,Interval,Latency,Supervision_timeout,Master_clock_accuracy),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Role:1/unsigned-unit:8,Peer_bdaddr_type:1/unsigned-unit:8,Peer_bdaddr:6/unit:8-binary,Interval:1/little-unsigned-unit:16,Latency:1/little-unsigned-unit:16,Supervision_timeout:1/little-unsigned-unit:16,Master_clock_accuracy:1/unsigned-unit:8).
-define(EVT_LE_CONN_COMPLETE_SIZE, 18).
-define(EVT_LE_ADVERTISING_REPORT, 2).
-record(le_advertising_info, {
  evt_type,
  bdaddr_type,
  bdaddr,
  length,
  data
}).
-define(le_advertising_info_bin(Evt_type,Bdaddr_type,Bdaddr,Length,Data),Evt_type:1/unsigned-unit:8,Bdaddr_type:1/unsigned-unit:8,Bdaddr:6/unit:8-binary,Length:1/unsigned-unit:8,Data:0/unit:8-binary).
-define(LE_ADVERTISING_INFO_SIZE, 9).
-define(EVT_LE_CONN_UPDATE_COMPLETE, 3).
-record(evt_le_connection_update_complete, {
  status,
  handle,
  interval,
  latency,
  supervision_timeout
}).
-define(evt_le_connection_update_complete_bin(Status,Handle,Interval,Latency,Supervision_timeout),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Interval:1/little-unsigned-unit:16,Latency:1/little-unsigned-unit:16,Supervision_timeout:1/little-unsigned-unit:16).
-define(EVT_LE_CONN_UPDATE_COMPLETE_SIZE, 9).
-define(EVT_LE_READ_REMOTE_USED_FEATURES_COMPLETE, 4).
-record(evt_le_read_remote_used_features_complete, {
  status,
  handle,
  features
}).
-define(evt_le_read_remote_used_features_complete_bin(Status,Handle,Features),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16,Features:8/unit:8-binary).
-define(EVT_LE_READ_REMOTE_USED_FEATURES_COMPLETE_SIZE, 11).
-define(EVT_LE_LTK_REQUEST, 5).
-record(evt_le_long_term_key_request, {
  handle,
  random,
  diversifier
}).
-define(evt_le_long_term_key_request_bin(Handle,Random,Diversifier),Handle:1/little-unsigned-unit:16,Random:1/little-unsigned-unit:64,Diversifier:1/little-unsigned-unit:16).
-define(EVT_LE_LTK_REQUEST_SIZE, 12).
-define(EVT_PHYSICAL_LINK_COMPLETE, 64).
-record(evt_physical_link_complete, {
  status,
  handle
}).
-define(evt_physical_link_complete_bin(Status,Handle),Status:1/unsigned-unit:8,Handle:1/unsigned-unit:8).
-define(EVT_PHYSICAL_LINK_COMPLETE_SIZE, 2).
-define(EVT_CHANNEL_SELECTED, 65).
-define(EVT_DISCONNECT_PHYSICAL_LINK_COMPLETE, 66).
-record(evt_disconn_physical_link_complete, {
  status,
  handle,
  reason
}).
-define(evt_disconn_physical_link_complete_bin(Status,Handle,Reason),Status:1/unsigned-unit:8,Handle:1/unsigned-unit:8,Reason:1/unsigned-unit:8).
-define(EVT_DISCONNECT_PHYSICAL_LINK_COMPLETE_SIZE, 3).
-define(EVT_PHYSICAL_LINK_LOSS_EARLY_WARNING, 67).
-record(evt_physical_link_loss_warning, {
  handle,
  reason
}).
-define(evt_physical_link_loss_warning_bin(Handle,Reason),Handle:1/unsigned-unit:8,Reason:1/unsigned-unit:8).
-define(EVT_PHYSICAL_LINK_LOSS_WARNING_SIZE, 2).
-define(EVT_PHYSICAL_LINK_RECOVERY, 68).
-record(evt_physical_link_recovery, {
  handle
}).
-define(evt_physical_link_recovery_bin(Handle),Handle:1/unsigned-unit:8).
-define(EVT_PHYSICAL_LINK_RECOVERY_SIZE, 1).
-define(EVT_LOGICAL_LINK_COMPLETE, 69).
-record(evt_logical_link_complete, {
  status,
  log_handle,
  handle,
  tx_flow_id
}).
-define(evt_logical_link_complete_bin(Status,Log_handle,Handle,Tx_flow_id),Status:1/unsigned-unit:8,Log_handle:1/little-unsigned-unit:16,Handle:1/unsigned-unit:8,Tx_flow_id:1/unsigned-unit:8).
-define(EVT_LOGICAL_LINK_COMPLETE_SIZE, 5).
-define(EVT_DISCONNECT_LOGICAL_LINK_COMPLETE, 70).
-define(EVT_FLOW_SPEC_MODIFY_COMPLETE, 71).
-record(evt_flow_spec_modify_complete, {
  status,
  handle
}).
-define(evt_flow_spec_modify_complete_bin(Status,Handle),Status:1/unsigned-unit:8,Handle:1/little-unsigned-unit:16).
-define(EVT_FLOW_SPEC_MODIFY_COMPLETE_SIZE, 3).
-define(EVT_NUMBER_COMPLETED_BLOCKS, 72).
-define(EVT_AMP_STATUS_CHANGE, 77).
-record(evt_amp_status_change, {
  status,
  amp_status
}).
-define(evt_amp_status_change_bin(Status,Amp_status),Status:1/unsigned-unit:8,Amp_status:1/unsigned-unit:8).
-define(EVT_AMP_STATUS_CHANGE_SIZE, 2).
-define(EVT_TESTING, 254).
-define(EVT_VENDOR, 255).
-define(EVT_STACK_INTERNAL, 253).
-record(evt_stack_internal, {
  type,
  data
}).
-define(evt_stack_internal_bin(Type,Data),Type:1/little-unsigned-unit:16,Data:0/unit:8-binary).
-define(EVT_STACK_INTERNAL_SIZE, 2).
-define(EVT_SI_DEVICE, 1).
-record(evt_si_device, {
  event,
  dev_id
}).
-define(evt_si_device_bin(Event,Dev_id),Event:1/little-unsigned-unit:16,Dev_id:1/little-unsigned-unit:16).
-define(EVT_SI_DEVICE_SIZE, 4).
-endif.
