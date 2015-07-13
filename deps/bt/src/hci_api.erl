%% -*- erlang -*-
-module(hci_api).
-compile(export_all).
-include("hci_api.hrl").


cname(<<0,_/binary>>) -> [];
cname(<<C,Cs/binary>>) -> [C|cname(Cs)];
cname(<<>>) -> [].


decode_status_bdaddr_rp(_Data) ->
  case _Data of
    <<?status_bdaddr_rp_bin(Status,Bdaddr)>> ->
      #status_bdaddr_rp { status = Status,bdaddr = Bdaddr }
  end.

decode_cancel_logical_link_rp(_Data) ->
  case _Data of
    <<?cancel_logical_link_rp_bin(Status,Handle,Tx_flow_id)>> ->
      #cancel_logical_link_rp { status = Status,handle = Handle,tx_flow_id = Tx_flow_id }
  end.

decode_role_discovery_rp(_Data) ->
  case _Data of
    <<?role_discovery_rp_bin(Status,Handle,Role)>> ->
      #role_discovery_rp { status = Status,handle = Handle,role = Role }
  end.

decode_read_link_policy_rp(_Data) ->
  case _Data of
    <<?read_link_policy_rp_bin(Status,Handle,Policy)>> ->
      #read_link_policy_rp { status = Status,handle = Handle,policy = Policy }
  end.

decode_write_link_policy_rp(_Data) ->
  case _Data of
    <<?write_link_policy_rp_bin(Status,Handle)>> ->
      #write_link_policy_rp { status = Status,handle = Handle }
  end.

decode_read_pin_type_rp(_Data) ->
  case _Data of
    <<?read_pin_type_rp_bin(Status,Pin_type)>> ->
      #read_pin_type_rp { status = Status,pin_type = Pin_type }
  end.

decode_read_stored_link_key_rp(_Data) ->
  case _Data of
    <<?read_stored_link_key_rp_bin(Status,Max_keys,Num_keys)>> ->
      #read_stored_link_key_rp { status = Status,max_keys = Max_keys,num_keys = Num_keys }
  end.

decode_write_stored_link_key_rp(_Data) ->
  case _Data of
    <<?write_stored_link_key_rp_bin(Status,Num_keys)>> ->
      #write_stored_link_key_rp { status = Status,num_keys = Num_keys }
  end.

decode_delete_stored_link_key_rp(_Data) ->
  case _Data of
    <<?delete_stored_link_key_rp_bin(Status,Num_keys)>> ->
      #delete_stored_link_key_rp { status = Status,num_keys = Num_keys }
  end.

decode_read_local_name_rp(_Data) ->
  case _Data of
    <<?read_local_name_rp_bin(Status,Name)>> ->
      #read_local_name_rp { status = Status,name = Name }
  end.

decode_read_conn_accept_timeout_rp(_Data) ->
  case _Data of
    <<?read_conn_accept_timeout_rp_bin(Status,Timeout)>> ->
      #read_conn_accept_timeout_rp { status = Status,timeout = Timeout }
  end.

decode_read_page_timeout_rp(_Data) ->
  case _Data of
    <<?read_page_timeout_rp_bin(Status,Timeout)>> ->
      #read_page_timeout_rp { status = Status,timeout = Timeout }
  end.

decode_read_scan_enable_rp(_Data) ->
  case _Data of
    <<?read_scan_enable_rp_bin(Status,Enable)>> ->
      #read_scan_enable_rp { status = Status,enable = Enable }
  end.

decode_read_page_activity_rp(_Data) ->
  case _Data of
    <<?read_page_activity_rp_bin(Status,Interval,Window)>> ->
      #read_page_activity_rp { status = Status,interval = Interval,window = Window }
  end.

decode_read_inq_activity_rp(_Data) ->
  case _Data of
    <<?read_inq_activity_rp_bin(Status,Interval,Window)>> ->
      #read_inq_activity_rp { status = Status,interval = Interval,window = Window }
  end.

decode_read_class_of_dev_rp(_Data) ->
  case _Data of
    <<?read_class_of_dev_rp_bin(Status,Dev_class)>> ->
      #read_class_of_dev_rp { status = Status,dev_class = Dev_class }
  end.

decode_read_voice_setting_rp(_Data) ->
  case _Data of
    <<?read_voice_setting_rp_bin(Status,Voice_setting)>> ->
      #read_voice_setting_rp { status = Status,voice_setting = Voice_setting }
  end.

decode_read_transmit_power_level_rp(_Data) ->
  case _Data of
    <<?read_transmit_power_level_rp_bin(Status,Handle,Level)>> ->
      #read_transmit_power_level_rp { status = Status,handle = Handle,level = Level }
  end.

decode_read_link_supervision_timeout_rp(_Data) ->
  case _Data of
    <<?read_link_supervision_timeout_rp_bin(Status,Handle,Timeout)>> ->
      #read_link_supervision_timeout_rp { status = Status,handle = Handle,timeout = Timeout }
  end.

decode_write_link_supervision_timeout_rp(_Data) ->
  case _Data of
    <<?write_link_supervision_timeout_rp_bin(Status,Handle)>> ->
      #write_link_supervision_timeout_rp { status = Status,handle = Handle }
  end.

decode_read_current_iac_lap_rp(_Data) ->
  case _Data of
    <<?read_current_iac_lap_rp_bin(Status,Num_current_iac,Lap)>> ->
      #read_current_iac_lap_rp { status = Status,num_current_iac = Num_current_iac,lap = Lap }
  end.

decode_set_afh_classification_rp(_Data) ->
  case _Data of
    <<?set_afh_classification_rp_bin(Status)>> ->
      #set_afh_classification_rp { status = Status }
  end.

decode_read_inquiry_scan_type_rp(_Data) ->
  case _Data of
    <<?read_inquiry_scan_type_rp_bin(Status,Type)>> ->
      #read_inquiry_scan_type_rp { status = Status,type = Type }
  end.

decode_write_inquiry_scan_type_rp(_Data) ->
  case _Data of
    <<?write_inquiry_scan_type_rp_bin(Status)>> ->
      #write_inquiry_scan_type_rp { status = Status }
  end.

decode_read_inquiry_mode_rp(_Data) ->
  case _Data of
    <<?read_inquiry_mode_rp_bin(Status,Mode)>> ->
      #read_inquiry_mode_rp { status = Status,mode = Mode }
  end.

decode_write_inquiry_mode_rp(_Data) ->
  case _Data of
    <<?write_inquiry_mode_rp_bin(Status)>> ->
      #write_inquiry_mode_rp { status = Status }
  end.

decode_read_afh_mode_rp(_Data) ->
  case _Data of
    <<?read_afh_mode_rp_bin(Status,Mode)>> ->
      #read_afh_mode_rp { status = Status,mode = Mode }
  end.

decode_write_afh_mode_rp(_Data) ->
  case _Data of
    <<?write_afh_mode_rp_bin(Status)>> ->
      #write_afh_mode_rp { status = Status }
  end.

decode_read_ext_inquiry_response_rp(_Data) ->
  case _Data of
    <<?read_ext_inquiry_response_rp_bin(Status,Fec,Data)>> ->
      #read_ext_inquiry_response_rp { status = Status,fec = Fec,data = Data }
  end.

decode_write_ext_inquiry_response_rp(_Data) ->
  case _Data of
    <<?write_ext_inquiry_response_rp_bin(Status)>> ->
      #write_ext_inquiry_response_rp { status = Status }
  end.

decode_refresh_encryption_key_rp(_Data) ->
  case _Data of
    <<?refresh_encryption_key_rp_bin(Status)>> ->
      #refresh_encryption_key_rp { status = Status }
  end.

decode_read_simple_pairing_mode_rp(_Data) ->
  case _Data of
    <<?read_simple_pairing_mode_rp_bin(Status,Mode)>> ->
      #read_simple_pairing_mode_rp { status = Status,mode = Mode }
  end.

decode_write_simple_pairing_mode_rp(_Data) ->
  case _Data of
    <<?write_simple_pairing_mode_rp_bin(Status)>> ->
      #write_simple_pairing_mode_rp { status = Status }
  end.

decode_read_local_oob_data_rp(_Data) ->
  case _Data of
    <<?read_local_oob_data_rp_bin(Status,Hash,Randomizer)>> ->
      #read_local_oob_data_rp { status = Status,hash = Hash,randomizer = Randomizer }
  end.

decode_read_inq_response_tx_power_level_rp(_Data) ->
  case _Data of
    <<?read_inq_response_tx_power_level_rp_bin(Status,Level)>> ->
      #read_inq_response_tx_power_level_rp { status = Status,level = Level }
  end.

decode_read_inquiry_transmit_power_level_rp(_Data) ->
  case _Data of
    <<?read_inquiry_transmit_power_level_rp_bin(Status,Level)>> ->
      #read_inquiry_transmit_power_level_rp { status = Status,level = Level }
  end.

decode_write_inquiry_transmit_power_level_rp(_Data) ->
  case _Data of
    <<?write_inquiry_transmit_power_level_rp_bin(Status)>> ->
      #write_inquiry_transmit_power_level_rp { status = Status }
  end.

decode_read_default_error_data_reporting_rp(_Data) ->
  case _Data of
    <<?read_default_error_data_reporting_rp_bin(Status,Reporting)>> ->
      #read_default_error_data_reporting_rp { status = Status,reporting = Reporting }
  end.

decode_write_default_error_data_reporting_rp(_Data) ->
  case _Data of
    <<?write_default_error_data_reporting_rp_bin(Status)>> ->
      #write_default_error_data_reporting_rp { status = Status }
  end.

decode_send_keypress_notify_rp(_Data) ->
  case _Data of
    <<?send_keypress_notify_rp_bin(Status)>> ->
      #send_keypress_notify_rp { status = Status }
  end.

decode_read_log_link_accept_timeout_rp(_Data) ->
  case _Data of
    <<?read_log_link_accept_timeout_rp_bin(Status,Timeout)>> ->
      #read_log_link_accept_timeout_rp { status = Status,timeout = Timeout }
  end.

decode_read_enhanced_transmit_power_level_rp(_Data) ->
  case _Data of
    <<?read_enhanced_transmit_power_level_rp_bin(Status,Handle,Level_gfsk,Level_dqpsk,Level_8dpsk)>> ->
      #read_enhanced_transmit_power_level_rp { status = Status,handle = Handle,level_gfsk = Level_gfsk,level_dqpsk = Level_dqpsk,level_8dpsk = Level_8dpsk }
  end.

decode_read_best_effort_flush_timeout_rp(_Data) ->
  case _Data of
    <<?read_best_effort_flush_timeout_rp_bin(Status,Timeout)>> ->
      #read_best_effort_flush_timeout_rp { status = Status,timeout = Timeout }
  end.

decode_write_best_effort_flush_timeout_rp(_Data) ->
  case _Data of
    <<?write_best_effort_flush_timeout_rp_bin(Status)>> ->
      #write_best_effort_flush_timeout_rp { status = Status }
  end.

decode_read_le_host_supported_rp(_Data) ->
  case _Data of
    <<?read_le_host_supported_rp_bin(Status,Le,Simul)>> ->
      #read_le_host_supported_rp { status = Status,le = Le,simul = Simul }
  end.

decode_read_local_version_rp(_Data) ->
  case _Data of
    <<?read_local_version_rp_bin(Status,Hci_ver,Hci_rev,Lmp_ver,Manufacturer,Lmp_subver)>> ->
      #read_local_version_rp { status = Status,hci_ver = Hci_ver,hci_rev = Hci_rev,lmp_ver = Lmp_ver,manufacturer = Manufacturer,lmp_subver = Lmp_subver }
  end.

decode_read_local_commands_rp(_Data) ->
  case _Data of
    <<?read_local_commands_rp_bin(Status,Commands)>> ->
      #read_local_commands_rp { status = Status,commands = Commands }
  end.

decode_read_local_features_rp(_Data) ->
  case _Data of
    <<?read_local_features_rp_bin(Status,Features)>> ->
      #read_local_features_rp { status = Status,features = Features }
  end.

decode_read_local_ext_features_rp(_Data) ->
  case _Data of
    <<?read_local_ext_features_rp_bin(Status,Page_num,Max_page_num,Features)>> ->
      #read_local_ext_features_rp { status = Status,page_num = Page_num,max_page_num = Max_page_num,features = Features }
  end.

decode_read_buffer_size_rp(_Data) ->
  case _Data of
    <<?read_buffer_size_rp_bin(Status,Acl_mtu,Sco_mtu,Acl_max_pkt,Sco_max_pkt)>> ->
      #read_buffer_size_rp { status = Status,acl_mtu = Acl_mtu,sco_mtu = Sco_mtu,acl_max_pkt = Acl_max_pkt,sco_max_pkt = Sco_max_pkt }
  end.

decode_read_bd_addr_rp(_Data) ->
  case _Data of
    <<?read_bd_addr_rp_bin(Status,Bdaddr)>> ->
      #read_bd_addr_rp { status = Status,bdaddr = Bdaddr }
  end.

decode_read_failed_contact_counter_rp(_Data) ->
  case _Data of
    <<?read_failed_contact_counter_rp_bin(Status,Handle,Counter)>> ->
      #read_failed_contact_counter_rp { status = Status,handle = Handle,counter = Counter }
  end.

decode_reset_failed_contact_counter_rp(_Data) ->
  case _Data of
    <<?reset_failed_contact_counter_rp_bin(Status,Handle)>> ->
      #reset_failed_contact_counter_rp { status = Status,handle = Handle }
  end.

decode_read_link_quality_rp(_Data) ->
  case _Data of
    <<?read_link_quality_rp_bin(Status,Handle,Link_quality)>> ->
      #read_link_quality_rp { status = Status,handle = Handle,link_quality = Link_quality }
  end.

decode_read_rssi_rp(_Data) ->
  case _Data of
    <<?read_rssi_rp_bin(Status,Handle,Rssi)>> ->
      #read_rssi_rp { status = Status,handle = Handle,rssi = Rssi }
  end.

decode_read_afh_map_rp(_Data) ->
  case _Data of
    <<?read_afh_map_rp_bin(Status,Handle,Mode,Map)>> ->
      #read_afh_map_rp { status = Status,handle = Handle,mode = Mode,map = Map }
  end.

decode_read_clock_rp(_Data) ->
  case _Data of
    <<?read_clock_rp_bin(Status,Handle,Clock,Accuracy)>> ->
      #read_clock_rp { status = Status,handle = Handle,clock = Clock,accuracy = Accuracy }
  end.

decode_read_local_amp_info_rp(_Data) ->
  case _Data of
    <<?read_local_amp_info_rp_bin(Status,Amp_status,Total_bandwidth,Max_guaranteed_bandwidth,Min_latency,Max_pdu_size,Controller_type,Pal_caps,Max_amp_assoc_length,Max_flush_timeout,Best_effort_flush_timeout)>> ->
      #read_local_amp_info_rp { status = Status,amp_status = Amp_status,total_bandwidth = Total_bandwidth,max_guaranteed_bandwidth = Max_guaranteed_bandwidth,min_latency = Min_latency,max_pdu_size = Max_pdu_size,controller_type = Controller_type,pal_caps = Pal_caps,max_amp_assoc_length = Max_amp_assoc_length,max_flush_timeout = Max_flush_timeout,best_effort_flush_timeout = Best_effort_flush_timeout }
  end.

decode_read_local_amp_assoc_rp(_Data) ->
  case _Data of
    <<?read_local_amp_assoc_rp_bin(Status,Handle,Rem_len,Frag)>> ->
      #read_local_amp_assoc_rp { status = Status,handle = Handle,rem_len = Rem_len,frag = Frag }
  end.

decode_write_remote_amp_assoc_rp(_Data) ->
  case _Data of
    <<?write_remote_amp_assoc_rp_bin(Status,Handle)>> ->
      #write_remote_amp_assoc_rp { status = Status,handle = Handle }
  end.

decode_write_simple_pairing_debug_mode_rp(_Data) ->
  case _Data of
    <<?write_simple_pairing_debug_mode_rp_bin(Status)>> ->
      #write_simple_pairing_debug_mode_rp { status = Status }
  end.

decode_le_read_buffer_size_rp(_Data) ->
  case _Data of
    <<?le_read_buffer_size_rp_bin(Status,Pkt_len,Max_pkt)>> ->
      #le_read_buffer_size_rp { status = Status,pkt_len = Pkt_len,max_pkt = Max_pkt }
  end.

decode_le_read_local_supported_features_rp(_Data) ->
  case _Data of
    <<?le_read_local_supported_features_rp_bin(Status,Features)>> ->
      #le_read_local_supported_features_rp { status = Status,features = Features }
  end.

decode_le_read_advertising_channel_tx_power_rp(_Data) ->
  case _Data of
    <<?le_read_advertising_channel_tx_power_rp_bin(Status,Level)>> ->
      #le_read_advertising_channel_tx_power_rp { status = Status,level = Level }
  end.

decode_le_read_white_list_size_rp(_Data) ->
  case _Data of
    <<?le_read_white_list_size_rp_bin(Status,Size)>> ->
      #le_read_white_list_size_rp { status = Status,size = Size }
  end.

decode_le_read_channel_map_rp(_Data) ->
  case _Data of
    <<?le_read_channel_map_rp_bin(Status,Handle,Map)>> ->
      #le_read_channel_map_rp { status = Status,handle = Handle,map = Map }
  end.

decode_le_encrypt_rp(_Data) ->
  case _Data of
    <<?le_encrypt_rp_bin(Status,Data)>> ->
      #le_encrypt_rp { status = Status,data = Data }
  end.

decode_le_rand_rp(_Data) ->
  case _Data of
    <<?le_rand_rp_bin(Status,Random)>> ->
      #le_rand_rp { status = Status,random = Random }
  end.

decode_le_ltk_reply_rp(_Data) ->
  case _Data of
    <<?le_ltk_reply_rp_bin(Status,Handle)>> ->
      #le_ltk_reply_rp { status = Status,handle = Handle }
  end.

decode_le_ltk_neg_reply_rp(_Data) ->
  case _Data of
    <<?le_ltk_neg_reply_rp_bin(Status,Handle)>> ->
      #le_ltk_neg_reply_rp { status = Status,handle = Handle }
  end.

decode_le_read_supported_states_rp(_Data) ->
  case _Data of
    <<?le_read_supported_states_rp_bin(Status,States)>> ->
      #le_read_supported_states_rp { status = Status,states = States }
  end.

decode_le_test_end_rp(_Data) ->
  case _Data of
    <<?le_test_end_rp_bin(Status,Num_pkts)>> ->
      #le_test_end_rp { status = Status,num_pkts = Num_pkts }
  end.

decode_inquiry_info(_Data) ->
  case _Data of
    <<?inquiry_info_bin(Bdaddr,Pscan_rep_mode,Pscan_period_mode,Pscan_mode,Dev_class,Clock_offset)>> ->
      #inquiry_info { bdaddr = Bdaddr,pscan_rep_mode = Pscan_rep_mode,pscan_period_mode = Pscan_period_mode,pscan_mode = Pscan_mode,dev_class = Dev_class,clock_offset = Clock_offset }
  end.

decode_evt_conn_complete(_Data) ->
  case _Data of
    <<?evt_conn_complete_bin(Status,Handle,Bdaddr,Link_type,Encr_mode)>> ->
      #evt_conn_complete { status = Status,handle = Handle,bdaddr = Bdaddr,link_type = Link_type,encr_mode = Encr_mode }
  end.

decode_evt_conn_request(_Data) ->
  case _Data of
    <<?evt_conn_request_bin(Bdaddr,Dev_class,Link_type)>> ->
      #evt_conn_request { bdaddr = Bdaddr,dev_class = Dev_class,link_type = Link_type }
  end.

decode_evt_disconn_complete(_Data) ->
  case _Data of
    <<?evt_disconn_complete_bin(Status,Handle,Reason)>> ->
      #evt_disconn_complete { status = Status,handle = Handle,reason = Reason }
  end.

decode_evt_auth_complete(_Data) ->
  case _Data of
    <<?evt_auth_complete_bin(Status,Handle)>> ->
      #evt_auth_complete { status = Status,handle = Handle }
  end.

decode_evt_remote_name_req_complete(_Data) ->
  case _Data of
    <<?evt_remote_name_req_complete_bin(Status,Bdaddr,Name)>> ->
      #evt_remote_name_req_complete { status = Status,bdaddr = Bdaddr,name = Name }
  end.

decode_evt_encrypt_change(_Data) ->
  case _Data of
    <<?evt_encrypt_change_bin(Status,Handle,Encrypt)>> ->
      #evt_encrypt_change { status = Status,handle = Handle,encrypt = Encrypt }
  end.

decode_evt_change_conn_link_key_complete(_Data) ->
  case _Data of
    <<?evt_change_conn_link_key_complete_bin(Status,Handle)>> ->
      #evt_change_conn_link_key_complete { status = Status,handle = Handle }
  end.

decode_evt_master_link_key_complete(_Data) ->
  case _Data of
    <<?evt_master_link_key_complete_bin(Status,Handle,Key_flag)>> ->
      #evt_master_link_key_complete { status = Status,handle = Handle,key_flag = Key_flag }
  end.

decode_evt_read_remote_features_complete(_Data) ->
  case _Data of
    <<?evt_read_remote_features_complete_bin(Status,Handle,Features)>> ->
      #evt_read_remote_features_complete { status = Status,handle = Handle,features = Features }
  end.

decode_evt_read_remote_version_complete(_Data) ->
  case _Data of
    <<?evt_read_remote_version_complete_bin(Status,Handle,Lmp_ver,Manufacturer,Lmp_subver)>> ->
      #evt_read_remote_version_complete { status = Status,handle = Handle,lmp_ver = Lmp_ver,manufacturer = Manufacturer,lmp_subver = Lmp_subver }
  end.

decode_evt_qos_setup_complete(_Data) ->
  case _Data of
    <<?evt_qos_setup_complete_bin(Status,Handle,Flags,Qos)>> ->
      #evt_qos_setup_complete { status = Status,handle = Handle,flags = Flags,qos = Qos }
  end.

decode_evt_cmd_complete(_Data) ->
  case _Data of
    <<?evt_cmd_complete_bin(Ncmd,Opcode)>> ->
      #evt_cmd_complete { ncmd = Ncmd,opcode = Opcode }
  end.

decode_evt_cmd_status(_Data) ->
  case _Data of
    <<?evt_cmd_status_bin(Status,Ncmd,Opcode)>> ->
      #evt_cmd_status { status = Status,ncmd = Ncmd,opcode = Opcode }
  end.

decode_evt_hardware_error(_Data) ->
  case _Data of
    <<?evt_hardware_error_bin(Code)>> ->
      #evt_hardware_error { code = Code }
  end.

decode_evt_flush_occured(_Data) ->
  case _Data of
    <<?evt_flush_occured_bin(Handle)>> ->
      #evt_flush_occured { handle = Handle }
  end.

decode_evt_role_change(_Data) ->
  case _Data of
    <<?evt_role_change_bin(Status,Bdaddr,Role)>> ->
      #evt_role_change { status = Status,bdaddr = Bdaddr,role = Role }
  end.

decode_evt_num_comp_pkts(_Data) ->
  case _Data of
    <<?evt_num_comp_pkts_bin(Num_hndl)>> ->
      #evt_num_comp_pkts { num_hndl = Num_hndl }
  end.

decode_evt_mode_change(_Data) ->
  case _Data of
    <<?evt_mode_change_bin(Status,Handle,Mode,Interval)>> ->
      #evt_mode_change { status = Status,handle = Handle,mode = Mode,interval = Interval }
  end.

decode_evt_return_link_keys(_Data) ->
  case _Data of
    <<?evt_return_link_keys_bin(Num_keys)>> ->
      #evt_return_link_keys { num_keys = Num_keys }
  end.

decode_evt_pin_code_req(_Data) ->
  case _Data of
    <<?evt_pin_code_req_bin(Bdaddr)>> ->
      #evt_pin_code_req { bdaddr = Bdaddr }
  end.

decode_evt_link_key_req(_Data) ->
  case _Data of
    <<?evt_link_key_req_bin(Bdaddr)>> ->
      #evt_link_key_req { bdaddr = Bdaddr }
  end.

decode_evt_link_key_notify(_Data) ->
  case _Data of
    <<?evt_link_key_notify_bin(Bdaddr,Link_key,Key_type)>> ->
      #evt_link_key_notify { bdaddr = Bdaddr,link_key = Link_key,key_type = Key_type }
  end.

decode_evt_data_buffer_overflow(_Data) ->
  case _Data of
    <<?evt_data_buffer_overflow_bin(Link_type)>> ->
      #evt_data_buffer_overflow { link_type = Link_type }
  end.

decode_evt_max_slots_change(_Data) ->
  case _Data of
    <<?evt_max_slots_change_bin(Handle,Max_slots)>> ->
      #evt_max_slots_change { handle = Handle,max_slots = Max_slots }
  end.

decode_evt_read_clock_offset_complete(_Data) ->
  case _Data of
    <<?evt_read_clock_offset_complete_bin(Status,Handle,Clock_offset)>> ->
      #evt_read_clock_offset_complete { status = Status,handle = Handle,clock_offset = Clock_offset }
  end.

decode_evt_conn_ptype_changed(_Data) ->
  case _Data of
    <<?evt_conn_ptype_changed_bin(Status,Handle,Ptype)>> ->
      #evt_conn_ptype_changed { status = Status,handle = Handle,ptype = Ptype }
  end.

decode_evt_qos_violation(_Data) ->
  case _Data of
    <<?evt_qos_violation_bin(Handle)>> ->
      #evt_qos_violation { handle = Handle }
  end.

decode_evt_pscan_rep_mode_change(_Data) ->
  case _Data of
    <<?evt_pscan_rep_mode_change_bin(Bdaddr,Pscan_rep_mode)>> ->
      #evt_pscan_rep_mode_change { bdaddr = Bdaddr,pscan_rep_mode = Pscan_rep_mode }
  end.

decode_evt_flow_spec_complete(_Data) ->
  case _Data of
    <<?evt_flow_spec_complete_bin(Status,Handle,Flags,Direction,Qos)>> ->
      #evt_flow_spec_complete { status = Status,handle = Handle,flags = Flags,direction = Direction,qos = Qos }
  end.

decode_evt_read_remote_ext_features_complete(_Data) ->
  case _Data of
    <<?evt_read_remote_ext_features_complete_bin(Status,Handle,Page_num,Max_page_num,Features)>> ->
      #evt_read_remote_ext_features_complete { status = Status,handle = Handle,page_num = Page_num,max_page_num = Max_page_num,features = Features }
  end.

decode_evt_sync_conn_complete(_Data) ->
  case _Data of
    <<?evt_sync_conn_complete_bin(Status,Handle,Bdaddr,Link_type,Trans_interval,Retrans_window,Rx_pkt_len,Tx_pkt_len,Air_mode)>> ->
      #evt_sync_conn_complete { status = Status,handle = Handle,bdaddr = Bdaddr,link_type = Link_type,trans_interval = Trans_interval,retrans_window = Retrans_window,rx_pkt_len = Rx_pkt_len,tx_pkt_len = Tx_pkt_len,air_mode = Air_mode }
  end.

decode_evt_sync_conn_changed(_Data) ->
  case _Data of
    <<?evt_sync_conn_changed_bin(Status,Handle,Trans_interval,Retrans_window,Rx_pkt_len,Tx_pkt_len)>> ->
      #evt_sync_conn_changed { status = Status,handle = Handle,trans_interval = Trans_interval,retrans_window = Retrans_window,rx_pkt_len = Rx_pkt_len,tx_pkt_len = Tx_pkt_len }
  end.

decode_evt_sniff_subrating(_Data) ->
  case _Data of
    <<?evt_sniff_subrating_bin(Status,Handle,Max_tx_latency,Max_rx_latency,Min_remote_timeout,Min_local_timeout)>> ->
      #evt_sniff_subrating { status = Status,handle = Handle,max_tx_latency = Max_tx_latency,max_rx_latency = Max_rx_latency,min_remote_timeout = Min_remote_timeout,min_local_timeout = Min_local_timeout }
  end.

decode_evt_encryption_key_refresh_complete(_Data) ->
  case _Data of
    <<?evt_encryption_key_refresh_complete_bin(Status,Handle)>> ->
      #evt_encryption_key_refresh_complete { status = Status,handle = Handle }
  end.

decode_evt_io_capability_request(_Data) ->
  case _Data of
    <<?evt_io_capability_request_bin(Bdaddr)>> ->
      #evt_io_capability_request { bdaddr = Bdaddr }
  end.

decode_evt_io_capability_response(_Data) ->
  case _Data of
    <<?evt_io_capability_response_bin(Bdaddr,Capability,Oob_data,Authentication)>> ->
      #evt_io_capability_response { bdaddr = Bdaddr,capability = Capability,oob_data = Oob_data,authentication = Authentication }
  end.

decode_evt_user_confirm_request(_Data) ->
  case _Data of
    <<?evt_user_confirm_request_bin(Bdaddr,Passkey)>> ->
      #evt_user_confirm_request { bdaddr = Bdaddr,passkey = Passkey }
  end.

decode_evt_user_passkey_request(_Data) ->
  case _Data of
    <<?evt_user_passkey_request_bin(Bdaddr)>> ->
      #evt_user_passkey_request { bdaddr = Bdaddr }
  end.

decode_evt_remote_oob_data_request(_Data) ->
  case _Data of
    <<?evt_remote_oob_data_request_bin(Bdaddr)>> ->
      #evt_remote_oob_data_request { bdaddr = Bdaddr }
  end.

decode_evt_simple_pairing_complete(_Data) ->
  case _Data of
    <<?evt_simple_pairing_complete_bin(Status,Bdaddr)>> ->
      #evt_simple_pairing_complete { status = Status,bdaddr = Bdaddr }
  end.

decode_evt_link_supervision_timeout_changed(_Data) ->
  case _Data of
    <<?evt_link_supervision_timeout_changed_bin(Handle,Timeout)>> ->
      #evt_link_supervision_timeout_changed { handle = Handle,timeout = Timeout }
  end.

decode_evt_enhanced_flush_complete(_Data) ->
  case _Data of
    <<?evt_enhanced_flush_complete_bin(Handle)>> ->
      #evt_enhanced_flush_complete { handle = Handle }
  end.

decode_evt_user_passkey_notify(_Data) ->
  case _Data of
    <<?evt_user_passkey_notify_bin(Bdaddr,Passkey,Entered)>> ->
      #evt_user_passkey_notify { bdaddr = Bdaddr,passkey = Passkey,entered = Entered }
  end.

decode_evt_keypress_notify(_Data) ->
  case _Data of
    <<?evt_keypress_notify_bin(Bdaddr,Type)>> ->
      #evt_keypress_notify { bdaddr = Bdaddr,type = Type }
  end.

decode_evt_remote_host_features_notify(_Data) ->
  case _Data of
    <<?evt_remote_host_features_notify_bin(Bdaddr,Features)>> ->
      #evt_remote_host_features_notify { bdaddr = Bdaddr,features = Features }
  end.

decode_evt_le_meta_event(_Data) ->
  case _Data of
    <<?evt_le_meta_event_bin(Subevent,Data)>> ->
      #evt_le_meta_event { subevent = Subevent,data = Data }
  end.

decode_evt_le_connection_complete(_Data) ->
  case _Data of
    <<?evt_le_connection_complete_bin(Status,Handle,Role,Peer_bdaddr_type,Peer_bdaddr,Interval,Latency,Supervision_timeout,Master_clock_accuracy)>> ->
      #evt_le_connection_complete { status = Status,handle = Handle,role = Role,peer_bdaddr_type = Peer_bdaddr_type,peer_bdaddr = Peer_bdaddr,interval = Interval,latency = Latency,supervision_timeout = Supervision_timeout,master_clock_accuracy = Master_clock_accuracy }
  end.

decode_evt_le_connection_update_complete(_Data) ->
  case _Data of
    <<?evt_le_connection_update_complete_bin(Status,Handle,Interval,Latency,Supervision_timeout)>> ->
      #evt_le_connection_update_complete { status = Status,handle = Handle,interval = Interval,latency = Latency,supervision_timeout = Supervision_timeout }
  end.

decode_evt_le_read_remote_used_features_complete(_Data) ->
  case _Data of
    <<?evt_le_read_remote_used_features_complete_bin(Status,Handle,Features)>> ->
      #evt_le_read_remote_used_features_complete { status = Status,handle = Handle,features = Features }
  end.

decode_evt_le_long_term_key_request(_Data) ->
  case _Data of
    <<?evt_le_long_term_key_request_bin(Handle,Random,Diversifier)>> ->
      #evt_le_long_term_key_request { handle = Handle,random = Random,diversifier = Diversifier }
  end.

decode_evt_physical_link_complete(_Data) ->
  case _Data of
    <<?evt_physical_link_complete_bin(Status,Handle)>> ->
      #evt_physical_link_complete { status = Status,handle = Handle }
  end.

decode_evt_disconn_physical_link_complete(_Data) ->
  case _Data of
    <<?evt_disconn_physical_link_complete_bin(Status,Handle,Reason)>> ->
      #evt_disconn_physical_link_complete { status = Status,handle = Handle,reason = Reason }
  end.

decode_evt_physical_link_loss_warning(_Data) ->
  case _Data of
    <<?evt_physical_link_loss_warning_bin(Handle,Reason)>> ->
      #evt_physical_link_loss_warning { handle = Handle,reason = Reason }
  end.

decode_evt_physical_link_recovery(_Data) ->
  case _Data of
    <<?evt_physical_link_recovery_bin(Handle)>> ->
      #evt_physical_link_recovery { handle = Handle }
  end.

decode_evt_logical_link_complete(_Data) ->
  case _Data of
    <<?evt_logical_link_complete_bin(Status,Log_handle,Handle,Tx_flow_id)>> ->
      #evt_logical_link_complete { status = Status,log_handle = Log_handle,handle = Handle,tx_flow_id = Tx_flow_id }
  end.

decode_evt_flow_spec_modify_complete(_Data) ->
  case _Data of
    <<?evt_flow_spec_modify_complete_bin(Status,Handle)>> ->
      #evt_flow_spec_modify_complete { status = Status,handle = Handle }
  end.

decode_evt_amp_status_change(_Data) ->
  case _Data of
    <<?evt_amp_status_change_bin(Status,Amp_status)>> ->
      #evt_amp_status_change { status = Status,amp_status = Amp_status }
  end.

decode_evt_stack_internal(_Data) ->
  case _Data of
    <<?evt_stack_internal_bin(Type,Data)>> ->
      #evt_stack_internal { type = Type,data = Data }
  end.

decode_evt_si_device(_Data) ->
  case _Data of
    <<?evt_si_device_bin(Event,Dev_id)>> ->
      #evt_si_device { event = Event,dev_id = Dev_id }
  end.

decode(Evt,_Data) ->
  case Evt of
    ?EVT_STACK_INTERNAL -> decode_evt_stack_internal(_Data);
    ?EVT_NUMBER_COMPLETED_BLOCKS -> decode_evt_amp_status_change(_Data);
    ?EVT_AMP_STATUS_CHANGE -> decode_evt_amp_status_change(_Data);
    ?EVT_DISCONNECT_LOGICAL_LINK_COMPLETE -> decode_evt_flow_spec_modify_complete(_Data);
    ?EVT_FLOW_SPEC_MODIFY_COMPLETE -> decode_evt_flow_spec_modify_complete(_Data);
    ?EVT_LOGICAL_LINK_COMPLETE -> decode_evt_logical_link_complete(_Data);
    ?EVT_PHYSICAL_LINK_RECOVERY -> decode_evt_physical_link_recovery(_Data);
    ?EVT_PHYSICAL_LINK_LOSS_EARLY_WARNING -> decode_evt_physical_link_loss_warning(_Data);
    ?EVT_CHANNEL_SELECTED -> decode_evt_disconn_physical_link_complete(_Data);
    ?EVT_DISCONNECT_PHYSICAL_LINK_COMPLETE -> decode_evt_disconn_physical_link_complete(_Data);
    ?EVT_PHYSICAL_LINK_COMPLETE -> decode_evt_physical_link_complete(_Data);
    ?EVT_LE_META_EVENT -> decode_evt_le_meta_event(_Data);
    ?EVT_REMOTE_HOST_FEATURES_NOTIFY -> decode_evt_remote_host_features_notify(_Data);
    ?EVT_KEYPRESS_NOTIFY -> decode_evt_keypress_notify(_Data);
    ?EVT_USER_PASSKEY_NOTIFY -> decode_evt_user_passkey_notify(_Data);
    ?EVT_ENHANCED_FLUSH_COMPLETE -> decode_evt_enhanced_flush_complete(_Data);
    ?EVT_LINK_SUPERVISION_TIMEOUT_CHANGED -> decode_evt_link_supervision_timeout_changed(_Data);
    ?EVT_SIMPLE_PAIRING_COMPLETE -> decode_evt_simple_pairing_complete(_Data);
    ?EVT_REMOTE_OOB_DATA_REQUEST -> decode_evt_remote_oob_data_request(_Data);
    ?EVT_USER_PASSKEY_REQUEST -> decode_evt_user_passkey_request(_Data);
    ?EVT_USER_CONFIRM_REQUEST -> decode_evt_user_confirm_request(_Data);
    ?EVT_IO_CAPABILITY_RESPONSE -> decode_evt_io_capability_response(_Data);
    ?EVT_IO_CAPABILITY_REQUEST -> decode_evt_io_capability_request(_Data);
    ?EVT_EXTENDED_INQUIRY_RESULT -> decode_evt_encryption_key_refresh_complete(_Data);
    ?EVT_ENCRYPTION_KEY_REFRESH_COMPLETE -> decode_evt_encryption_key_refresh_complete(_Data);
    ?EVT_SNIFF_SUBRATING -> decode_evt_sniff_subrating(_Data);
    ?EVT_SYNC_CONN_CHANGED -> decode_evt_sync_conn_changed(_Data);
    ?EVT_SYNC_CONN_COMPLETE -> decode_evt_sync_conn_complete(_Data);
    ?EVT_INQUIRY_RESULT_WITH_RSSI -> decode_evt_read_remote_ext_features_complete(_Data);
    ?EVT_READ_REMOTE_EXT_FEATURES_COMPLETE -> decode_evt_read_remote_ext_features_complete(_Data);
    ?EVT_FLOW_SPEC_COMPLETE -> decode_evt_flow_spec_complete(_Data);
    ?EVT_PSCAN_REP_MODE_CHANGE -> decode_evt_pscan_rep_mode_change(_Data);
    ?EVT_QOS_VIOLATION -> decode_evt_qos_violation(_Data);
    ?EVT_CONN_PTYPE_CHANGED -> decode_evt_conn_ptype_changed(_Data);
    ?EVT_READ_CLOCK_OFFSET_COMPLETE -> decode_evt_read_clock_offset_complete(_Data);
    ?EVT_MAX_SLOTS_CHANGE -> decode_evt_max_slots_change(_Data);
    ?EVT_LOOPBACK_COMMAND -> decode_evt_data_buffer_overflow(_Data);
    ?EVT_DATA_BUFFER_OVERFLOW -> decode_evt_data_buffer_overflow(_Data);
    ?EVT_LINK_KEY_NOTIFY -> decode_evt_link_key_notify(_Data);
    ?EVT_LINK_KEY_REQ -> decode_evt_link_key_req(_Data);
    ?EVT_PIN_CODE_REQ -> decode_evt_pin_code_req(_Data);
    ?EVT_RETURN_LINK_KEYS -> decode_evt_return_link_keys(_Data);
    ?EVT_MODE_CHANGE -> decode_evt_mode_change(_Data);
    ?EVT_NUM_COMP_PKTS -> decode_evt_num_comp_pkts(_Data);
    ?EVT_ROLE_CHANGE -> decode_evt_role_change(_Data);
    ?EVT_FLUSH_OCCURRED -> decode_evt_flush_occured(_Data);
    ?EVT_HARDWARE_ERROR -> decode_evt_hardware_error(_Data);
    ?EVT_CMD_STATUS -> decode_evt_cmd_status(_Data);
    ?EVT_CMD_COMPLETE -> decode_evt_cmd_complete(_Data);
    ?EVT_QOS_SETUP_COMPLETE -> decode_evt_qos_setup_complete(_Data);
    ?EVT_READ_REMOTE_VERSION_COMPLETE -> decode_evt_read_remote_version_complete(_Data);
    ?EVT_READ_REMOTE_FEATURES_COMPLETE -> decode_evt_read_remote_features_complete(_Data);
    ?EVT_MASTER_LINK_KEY_COMPLETE -> decode_evt_master_link_key_complete(_Data);
    ?EVT_CHANGE_CONN_LINK_KEY_COMPLETE -> decode_evt_change_conn_link_key_complete(_Data);
    ?EVT_ENCRYPT_CHANGE -> decode_evt_encrypt_change(_Data);
    ?EVT_REMOTE_NAME_REQ_COMPLETE -> decode_evt_remote_name_req_complete(_Data);
    ?EVT_AUTH_COMPLETE -> decode_evt_auth_complete(_Data);
    ?EVT_DISCONN_COMPLETE -> decode_evt_disconn_complete(_Data);
    ?EVT_CONN_REQUEST -> decode_evt_conn_request(_Data);
    ?EVT_INQUIRY_COMPLETE -> decode_inquiry_info(_Data);
    ?EVT_INQUIRY_RESULT -> decode_inquiry_info(_Data);
    ?EVT_CONN_COMPLETE -> decode_evt_conn_complete(_Data);
    _ -> erlang:error(bad_event)
  end.

decode_le(Evt,_Data) ->
  case Evt of
    ?EVT_LE_LTK_REQUEST -> decode_evt_le_long_term_key_request(_Data);
    ?EVT_LE_READ_REMOTE_USED_FEATURES_COMPLETE -> decode_evt_le_read_remote_used_features_complete(_Data);
    ?EVT_LE_ADVERTISING_REPORT -> decode_evt_le_connection_update_complete(_Data);
    ?EVT_LE_CONN_UPDATE_COMPLETE -> decode_evt_le_connection_update_complete(_Data);
    ?EVT_LE_CONN_COMPLETE -> decode_evt_le_connection_complete(_Data);
    _ -> erlang:error(bad_event)
  end.

le_test_end(Socket) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_TEST_END,<<>>,fun decode_le_test_end_rp/1).

le_transmitter_test(Socket,Frequency,Length,Payload) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_TRANSMITTER_TEST,<<?le_transmitter_test_cp_bin(Frequency,Length,Payload)>>,undefined).

le_receiver_test(Socket,Frequency) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_RECEIVER_TEST,<<?le_receiver_test_cp_bin(Frequency)>>,undefined).

le_read_supported_states(Socket) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_READ_SUPPORTED_STATES,<<>>,fun decode_le_read_supported_states_rp/1).

le_ltk_neg_reply(Socket,Handle) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_LTK_NEG_REPLY,<<?le_ltk_neg_reply_cp_bin(Handle)>>,fun decode_le_ltk_neg_reply_rp/1).

le_ltk_reply(Socket,Handle,Key) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_LTK_REPLY,<<?le_ltk_reply_cp_bin(Handle,Key)>>,fun decode_le_ltk_reply_rp/1).

le_start_encryption(Socket,Handle,Random,Diversifier,Key) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_START_ENCRYPTION,<<?le_start_encryption_cp_bin(Handle,Random,Diversifier,Key)>>,undefined).

le_rand(Socket) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_RAND,<<>>,fun decode_le_rand_rp/1).

le_encrypt(Socket,Key,Plaintext) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_ENCRYPT,<<?le_encrypt_cp_bin(Key,Plaintext)>>,fun decode_le_encrypt_rp/1).

le_read_remote_used_features(Socket,Handle) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_READ_REMOTE_USED_FEATURES,<<?le_read_remote_used_features_cp_bin(Handle)>>,undefined).

le_read_channel_map(Socket,Handle) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_READ_CHANNEL_MAP,<<?le_read_channel_map_cp_bin(Handle)>>,fun decode_le_read_channel_map_rp/1).

le_set_host_channel_classification(Socket,Map) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_SET_HOST_CHANNEL_CLASSIFICATION,<<?le_set_host_channel_classification_cp_bin(Map)>>,undefined).

le_conn_update(Socket) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_CONN_UPDATE,<<>>,undefined).

le_remove_device_from_white_list(Socket,Bdaddr_type,Bdaddr) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_REMOVE_DEVICE_FROM_WHITE_LIST,<<?le_remove_device_from_white_list_cp_bin(Bdaddr_type,Bdaddr)>>,undefined).

le_add_device_to_white_list(Socket,Bdaddr_type,Bdaddr) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_ADD_DEVICE_TO_WHITE_LIST,<<?le_add_device_to_white_list_cp_bin(Bdaddr_type,Bdaddr)>>,undefined).

le_clear_white_list(Socket) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_CLEAR_WHITE_LIST,<<>>,undefined).

le_read_white_list_size(Socket) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_READ_WHITE_LIST_SIZE,<<>>,fun decode_le_read_white_list_size_rp/1).

le_create_conn_cancel(Socket) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_CREATE_CONN_CANCEL,<<>>,undefined).

le_create_conn(Socket) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_CREATE_CONN,<<>>,undefined).

le_set_scan_enable(Socket,Enable,Filter_dup) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_SET_SCAN_ENABLE,<<?le_set_scan_enable_cp_bin(Enable,Filter_dup)>>,undefined).

le_set_scan_parameters(Socket,Type,Interval,Window,Own_bdaddr_type,Filter) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_SET_SCAN_PARAMETERS,<<?le_set_scan_parameters_cp_bin(Type,Interval,Window,Own_bdaddr_type,Filter)>>,undefined).

le_set_advertise_enable(Socket,Enable) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_SET_ADVERTISE_ENABLE,<<?le_set_advertise_enable_cp_bin(Enable)>>,undefined).

le_set_scan_response_data(Socket,Length,Data) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_SET_SCAN_RESPONSE_DATA,<<?le_set_scan_response_data_cp_bin(Length,Data)>>,undefined).

le_set_advertising_data(Socket,Length,Data) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_SET_ADVERTISING_DATA,<<?le_set_advertising_data_cp_bin(Length,Data)>>,undefined).

le_read_advertising_channel_tx_power(Socket) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_READ_ADVERTISING_CHANNEL_TX_POWER,<<>>,fun decode_le_read_advertising_channel_tx_power_rp/1).

le_set_advertising_parameters(Socket,Min_interval,Max_interval,Advtype,Own_bdaddr_type,Direct_bdaddr_type,Direct_bdaddr,Chan_map,Filter) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_SET_ADVERTISING_PARAMETERS,<<?le_set_advertising_parameters_cp_bin(Min_interval,Max_interval,Advtype,Own_bdaddr_type,Direct_bdaddr_type,Direct_bdaddr,Chan_map,Filter)>>,undefined).

le_set_random_address(Socket,Bdaddr) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_SET_RANDOM_ADDRESS,<<?le_set_random_address_cp_bin(Bdaddr)>>,undefined).

le_read_local_supported_features(Socket) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_READ_LOCAL_SUPPORTED_FEATURES,<<>>,fun decode_le_read_local_supported_features_rp/1).

le_read_buffer_size(Socket) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_READ_BUFFER_SIZE,<<>>,fun decode_le_read_buffer_size_rp/1).

le_set_event_mask(Socket,Mask) ->
  hci_socket:call(Socket,?OGF_LE_CTL,?OCF_LE_SET_EVENT_MASK,<<?le_set_event_mask_cp_bin(Mask)>>,undefined).

write_simple_pairing_debug_mode(Socket,Mode) ->
  hci_socket:call(Socket,?OGF_TESTING_CMD,?OCF_WRITE_SIMPLE_PAIRING_DEBUG_MODE,<<?write_simple_pairing_debug_mode_cp_bin(Mode)>>,fun decode_write_simple_pairing_debug_mode_rp/1).

enable_device_under_test_mode(Socket) ->
  hci_socket:call(Socket,?OGF_TESTING_CMD,?OCF_ENABLE_DEVICE_UNDER_TEST_MODE,<<>>,undefined).

write_loopback_mode(Socket) ->
  hci_socket:call(Socket,?OGF_TESTING_CMD,?OCF_WRITE_LOOPBACK_MODE,<<>>,undefined).

read_loopback_mode(Socket) ->
  hci_socket:call(Socket,?OGF_TESTING_CMD,?OCF_READ_LOOPBACK_MODE,<<>>,undefined).

write_remote_amp_assoc(Socket,Handle,Length_so_far,Assoc_length,Fragment) ->
  hci_socket:call(Socket,?OGF_STATUS_PARAM,?OCF_WRITE_REMOTE_AMP_ASSOC,<<?write_remote_amp_assoc_cp_bin(Handle,Length_so_far,Assoc_length,Fragment)>>,fun decode_write_remote_amp_assoc_rp/1).

read_local_amp_assoc(Socket,Handle,Len_so_far,Max_len) ->
  hci_socket:call(Socket,?OGF_STATUS_PARAM,?OCF_READ_LOCAL_AMP_ASSOC,<<?read_local_amp_assoc_cp_bin(Handle,Len_so_far,Max_len)>>,fun decode_read_local_amp_assoc_rp/1).

read_local_amp_info(Socket) ->
  hci_socket:call(Socket,?OGF_STATUS_PARAM,?OCF_READ_LOCAL_AMP_INFO,<<>>,fun decode_read_local_amp_info_rp/1).

read_clock(Socket,Handle,Which_clock) ->
  hci_socket:call(Socket,?OGF_STATUS_PARAM,?OCF_READ_CLOCK,<<?read_clock_cp_bin(Handle,Which_clock)>>,fun decode_read_clock_rp/1).

read_afh_map(Socket) ->
  hci_socket:call(Socket,?OGF_STATUS_PARAM,?OCF_READ_AFH_MAP,<<>>,fun decode_read_afh_map_rp/1).

read_rssi(Socket) ->
  hci_socket:call(Socket,?OGF_STATUS_PARAM,?OCF_READ_RSSI,<<>>,fun decode_read_rssi_rp/1).

read_link_quality(Socket) ->
  hci_socket:call(Socket,?OGF_STATUS_PARAM,?OCF_READ_LINK_QUALITY,<<>>,fun decode_read_link_quality_rp/1).

reset_failed_contact_counter(Socket) ->
  hci_socket:call(Socket,?OGF_STATUS_PARAM,?OCF_RESET_FAILED_CONTACT_COUNTER,<<>>,fun decode_reset_failed_contact_counter_rp/1).

read_failed_contact_counter(Socket) ->
  hci_socket:call(Socket,?OGF_STATUS_PARAM,?OCF_READ_FAILED_CONTACT_COUNTER,<<>>,fun decode_read_failed_contact_counter_rp/1).

read_bd_addr(Socket) ->
  hci_socket:call(Socket,?OGF_INFO_PARAM,?OCF_READ_BD_ADDR,<<>>,fun decode_read_bd_addr_rp/1).

read_country_code(Socket) ->
  hci_socket:call(Socket,?OGF_INFO_PARAM,?OCF_READ_COUNTRY_CODE,<<>>,undefined).

read_buffer_size(Socket) ->
  hci_socket:call(Socket,?OGF_INFO_PARAM,?OCF_READ_BUFFER_SIZE,<<>>,fun decode_read_buffer_size_rp/1).

read_local_ext_features(Socket,Page_num) ->
  hci_socket:call(Socket,?OGF_INFO_PARAM,?OCF_READ_LOCAL_EXT_FEATURES,<<?read_local_ext_features_cp_bin(Page_num)>>,fun decode_read_local_ext_features_rp/1).

read_local_features(Socket) ->
  hci_socket:call(Socket,?OGF_INFO_PARAM,?OCF_READ_LOCAL_FEATURES,<<>>,fun decode_read_local_features_rp/1).

read_local_commands(Socket) ->
  hci_socket:call(Socket,?OGF_INFO_PARAM,?OCF_READ_LOCAL_COMMANDS,<<>>,fun decode_read_local_commands_rp/1).

read_local_version(Socket) ->
  hci_socket:call(Socket,?OGF_INFO_PARAM,?OCF_READ_LOCAL_VERSION,<<>>,fun decode_read_local_version_rp/1).

write_le_host_supported(Socket,Le,Simul) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_LE_HOST_SUPPORTED,<<?write_le_host_supported_cp_bin(Le,Simul)>>,undefined).

read_le_host_supported(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_LE_HOST_SUPPORTED,<<>>,fun decode_read_le_host_supported_rp/1).

write_best_effort_flush_timeout(Socket,Handle,Timeout) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_BEST_EFFORT_FLUSH_TIMEOUT,<<?write_best_effort_flush_timeout_cp_bin(Handle,Timeout)>>,fun decode_write_best_effort_flush_timeout_rp/1).

read_best_effort_flush_timeout(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_BEST_EFFORT_FLUSH_TIMEOUT,<<>>,fun decode_read_best_effort_flush_timeout_rp/1).

read_enhanced_transmit_power_level(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_ENHANCED_TRANSMIT_POWER_LEVEL,<<>>,fun decode_read_enhanced_transmit_power_level_rp/1).

write_flow_control_mode(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_FLOW_CONTROL_MODE,<<>>,undefined).

read_flow_control_mode(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_FLOW_CONTROL_MODE,<<>>,undefined).

write_location_data(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_LOCATION_DATA,<<>>,undefined).

read_location_data(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_LOCATION_DATA,<<>>,undefined).

set_event_mask_page_2(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_SET_EVENT_MASK_PAGE_2,<<>>,undefined).

write_logical_link_accept_timeout(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_LOGICAL_LINK_ACCEPT_TIMEOUT,<<>>,undefined).

read_logical_link_accept_timeout(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_LOGICAL_LINK_ACCEPT_TIMEOUT,<<>>,undefined).

send_keypress_notify(Socket,Bdaddr,Type) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_SEND_KEYPRESS_NOTIFY,<<?send_keypress_notify_cp_bin(Bdaddr,Type)>>,fun decode_send_keypress_notify_rp/1).

enhanced_flush(Socket,Handle,Type) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_ENHANCED_FLUSH,<<?enhanced_flush_cp_bin(Handle,Type)>>,undefined).

write_default_error_data_reporting(Socket,Reporting) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_DEFAULT_ERROR_DATA_REPORTING,<<?write_default_error_data_reporting_cp_bin(Reporting)>>,fun decode_write_default_error_data_reporting_rp/1).

read_default_error_data_reporting(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_DEFAULT_ERROR_DATA_REPORTING,<<>>,fun decode_read_default_error_data_reporting_rp/1).

write_inquiry_transmit_power_level(Socket,Level) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_INQUIRY_TRANSMIT_POWER_LEVEL,<<?write_inquiry_transmit_power_level_cp_bin(Level)>>,fun decode_write_inquiry_transmit_power_level_rp/1).

read_inquiry_transmit_power_level(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_INQUIRY_TRANSMIT_POWER_LEVEL,<<>>,fun decode_read_inquiry_transmit_power_level_rp/1).

read_inq_response_tx_power_level(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_INQ_RESPONSE_TX_POWER_LEVEL,<<>>,fun decode_read_inq_response_tx_power_level_rp/1).

read_local_oob_data(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_LOCAL_OOB_DATA,<<>>,fun decode_read_local_oob_data_rp/1).

write_simple_pairing_mode(Socket,Mode) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_SIMPLE_PAIRING_MODE,<<?write_simple_pairing_mode_cp_bin(Mode)>>,fun decode_write_simple_pairing_mode_rp/1).

read_simple_pairing_mode(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_SIMPLE_PAIRING_MODE,<<>>,fun decode_read_simple_pairing_mode_rp/1).

refresh_encryption_key(Socket,Handle) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_REFRESH_ENCRYPTION_KEY,<<?refresh_encryption_key_cp_bin(Handle)>>,fun decode_refresh_encryption_key_rp/1).

write_ext_inquiry_response(Socket,Fec,Data) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_EXT_INQUIRY_RESPONSE,<<?write_ext_inquiry_response_cp_bin(Fec,Data)>>,fun decode_write_ext_inquiry_response_rp/1).

read_ext_inquiry_response(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_EXT_INQUIRY_RESPONSE,<<>>,fun decode_read_ext_inquiry_response_rp/1).

write_afh_mode(Socket,Mode) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_AFH_MODE,<<?write_afh_mode_cp_bin(Mode)>>,fun decode_write_afh_mode_rp/1).

read_afh_mode(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_AFH_MODE,<<>>,fun decode_read_afh_mode_rp/1).

write_page_scan_type(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_PAGE_SCAN_TYPE,<<>>,undefined).

read_page_scan_type(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_PAGE_SCAN_TYPE,<<>>,undefined).

write_inquiry_mode(Socket,Mode) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_INQUIRY_MODE,<<?write_inquiry_mode_cp_bin(Mode)>>,fun decode_write_inquiry_mode_rp/1).

read_inquiry_mode(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_INQUIRY_MODE,<<>>,fun decode_read_inquiry_mode_rp/1).

write_inquiry_scan_type(Socket,Type) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_INQUIRY_SCAN_TYPE,<<?write_inquiry_scan_type_cp_bin(Type)>>,fun decode_write_inquiry_scan_type_rp/1).

read_inquiry_scan_type(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_INQUIRY_SCAN_TYPE,<<>>,fun decode_read_inquiry_scan_type_rp/1).

set_afh_classification(Socket,Map) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_SET_AFH_CLASSIFICATION,<<?set_afh_classification_cp_bin(Map)>>,fun decode_set_afh_classification_rp/1).

write_page_scan_mode(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_PAGE_SCAN_MODE,<<>>,undefined).

read_page_scan_mode(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_PAGE_SCAN_MODE,<<>>,undefined).

write_page_scan_period_mode(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_PAGE_SCAN_PERIOD_MODE,<<>>,undefined).

read_page_scan_period_mode(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_PAGE_SCAN_PERIOD_MODE,<<>>,undefined).

write_current_iac_lap(Socket,Num_current_iac,Lap) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_CURRENT_IAC_LAP,<<?write_current_iac_lap_cp_bin(Num_current_iac,Lap)>>,undefined).

read_current_iac_lap(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_CURRENT_IAC_LAP,<<>>,fun decode_read_current_iac_lap_rp/1).

read_num_supported_iac(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_NUM_SUPPORTED_IAC,<<>>,undefined).

write_link_supervision_timeout(Socket,Handle,Timeout) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_LINK_SUPERVISION_TIMEOUT,<<?write_link_supervision_timeout_cp_bin(Handle,Timeout)>>,fun decode_write_link_supervision_timeout_rp/1).

read_link_supervision_timeout(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_LINK_SUPERVISION_TIMEOUT,<<>>,fun decode_read_link_supervision_timeout_rp/1).

host_num_comp_pkts(Socket,Num_hndl) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_HOST_NUM_COMP_PKTS,<<?host_num_comp_pkts_cp_bin(Num_hndl)>>,undefined).

host_buffer_size(Socket,Acl_mtu,Sco_mtu,Acl_max_pkt,Sco_max_pkt) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_HOST_BUFFER_SIZE,<<?host_buffer_size_cp_bin(Acl_mtu,Sco_mtu,Acl_max_pkt,Sco_max_pkt)>>,undefined).

set_controller_to_host_fc(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_SET_CONTROLLER_TO_HOST_FC,<<>>,undefined).

write_sync_flow_enable(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_SYNC_FLOW_ENABLE,<<>>,undefined).

read_sync_flow_enable(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_SYNC_FLOW_ENABLE,<<>>,undefined).

read_transmit_power_level(Socket,Handle,Type) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_TRANSMIT_POWER_LEVEL,<<?read_transmit_power_level_cp_bin(Handle,Type)>>,fun decode_read_transmit_power_level_rp/1).

write_hold_mode_activity(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_HOLD_MODE_ACTIVITY,<<>>,undefined).

read_hold_mode_activity(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_HOLD_MODE_ACTIVITY,<<>>,undefined).

write_num_broadcast_retrans(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_NUM_BROADCAST_RETRANS,<<>>,undefined).

read_num_broadcast_retrans(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_NUM_BROADCAST_RETRANS,<<>>,undefined).

write_automatic_flush_timeout(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_AUTOMATIC_FLUSH_TIMEOUT,<<>>,undefined).

read_automatic_flush_timeout(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_AUTOMATIC_FLUSH_TIMEOUT,<<>>,undefined).

write_voice_setting(Socket,Voice_setting) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_VOICE_SETTING,<<?write_voice_setting_cp_bin(Voice_setting)>>,undefined).

read_voice_setting(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_VOICE_SETTING,<<>>,fun decode_read_voice_setting_rp/1).

write_class_of_dev(Socket,Dev_class) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_CLASS_OF_DEV,<<?write_class_of_dev_cp_bin(Dev_class)>>,undefined).

read_class_of_dev(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_CLASS_OF_DEV,<<>>,fun decode_read_class_of_dev_rp/1).

write_encrypt_mode(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_ENCRYPT_MODE,<<>>,undefined).

read_encrypt_mode(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_ENCRYPT_MODE,<<>>,undefined).

write_auth_enable(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_AUTH_ENABLE,<<>>,undefined).

read_auth_enable(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_AUTH_ENABLE,<<>>,undefined).

write_inq_activity(Socket,Interval,Window) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_INQ_ACTIVITY,<<?write_inq_activity_cp_bin(Interval,Window)>>,undefined).

read_inq_activity(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_INQ_ACTIVITY,<<>>,fun decode_read_inq_activity_rp/1).

write_page_activity(Socket,Interval,Window) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_PAGE_ACTIVITY,<<?write_page_activity_cp_bin(Interval,Window)>>,undefined).

read_page_activity(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_PAGE_ACTIVITY,<<>>,fun decode_read_page_activity_rp/1).

write_scan_enable(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_SCAN_ENABLE,<<>>,undefined).

read_scan_enable(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_SCAN_ENABLE,<<>>,fun decode_read_scan_enable_rp/1).

write_page_timeout(Socket,Timeout) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_PAGE_TIMEOUT,<<?write_page_timeout_cp_bin(Timeout)>>,undefined).

read_page_timeout(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_PAGE_TIMEOUT,<<>>,fun decode_read_page_timeout_rp/1).

write_conn_accept_timeout(Socket,Timeout) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_CONN_ACCEPT_TIMEOUT,<<?write_conn_accept_timeout_cp_bin(Timeout)>>,undefined).

read_conn_accept_timeout(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_CONN_ACCEPT_TIMEOUT,<<>>,fun decode_read_conn_accept_timeout_rp/1).

read_local_name(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_LOCAL_NAME,<<>>,fun decode_read_local_name_rp/1).

change_local_name(Socket,Name) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_CHANGE_LOCAL_NAME,<<?change_local_name_cp_bin(Name)>>,undefined).

delete_stored_link_key(Socket,Bdaddr,Delete_all) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_DELETE_STORED_LINK_KEY,<<?delete_stored_link_key_cp_bin(Bdaddr,Delete_all)>>,fun decode_delete_stored_link_key_rp/1).

write_stored_link_key(Socket,Num_keys) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_STORED_LINK_KEY,<<?write_stored_link_key_cp_bin(Num_keys)>>,fun decode_write_stored_link_key_rp/1).

read_stored_link_key(Socket,Bdaddr,Read_all) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_STORED_LINK_KEY,<<?read_stored_link_key_cp_bin(Bdaddr,Read_all)>>,fun decode_read_stored_link_key_rp/1).

create_new_unit_key(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_CREATE_NEW_UNIT_KEY,<<>>,undefined).

write_pin_type(Socket,Pin_type) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_WRITE_PIN_TYPE,<<?write_pin_type_cp_bin(Pin_type)>>,undefined).

read_pin_type(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_READ_PIN_TYPE,<<>>,fun decode_read_pin_type_rp/1).

flush(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_FLUSH,<<>>,undefined).

set_event_flt(Socket,Flt_type,Cond_type,Condition) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_SET_EVENT_FLT,<<?set_event_flt_cp_bin(Flt_type,Cond_type,Condition)>>,undefined).

reset(Socket) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_RESET,<<>>,undefined).

set_event_mask(Socket,Mask) ->
  hci_socket:call(Socket,?OGF_HOST_CTL,?OCF_SET_EVENT_MASK,<<?set_event_mask_cp_bin(Mask)>>,undefined).

sniff_subrating(Socket,Handle,Max_latency,Min_remote_timeout,Min_local_timeout) ->
  hci_socket:call(Socket,?OGF_LINK_POLICY,?OCF_SNIFF_SUBRATING,<<?sniff_subrating_cp_bin(Handle,Max_latency,Min_remote_timeout,Min_local_timeout)>>,undefined).

flow_specification(Socket) ->
  hci_socket:call(Socket,?OGF_LINK_POLICY,?OCF_FLOW_SPECIFICATION,<<>>,undefined).

write_default_link_policy(Socket) ->
  hci_socket:call(Socket,?OGF_LINK_POLICY,?OCF_WRITE_DEFAULT_LINK_POLICY,<<>>,undefined).

read_default_link_policy(Socket) ->
  hci_socket:call(Socket,?OGF_LINK_POLICY,?OCF_READ_DEFAULT_LINK_POLICY,<<>>,undefined).

write_link_policy(Socket,Handle,Policy) ->
  hci_socket:call(Socket,?OGF_LINK_POLICY,?OCF_WRITE_LINK_POLICY,<<?write_link_policy_cp_bin(Handle,Policy)>>,fun decode_write_link_policy_rp/1).

read_link_policy(Socket,Handle) ->
  hci_socket:call(Socket,?OGF_LINK_POLICY,?OCF_READ_LINK_POLICY,<<?read_link_policy_cp_bin(Handle)>>,fun decode_read_link_policy_rp/1).

switch_role(Socket,Bdaddr,Role) ->
  hci_socket:call(Socket,?OGF_LINK_POLICY,?OCF_SWITCH_ROLE,<<?switch_role_cp_bin(Bdaddr,Role)>>,undefined).

role_discovery(Socket,Handle) ->
  hci_socket:call(Socket,?OGF_LINK_POLICY,?OCF_ROLE_DISCOVERY,<<?role_discovery_cp_bin(Handle)>>,fun decode_role_discovery_rp/1).

qos_setup(Socket,Handle,Flags,Qos) ->
  hci_socket:call(Socket,?OGF_LINK_POLICY,?OCF_QOS_SETUP,<<?qos_setup_cp_bin(Handle,Flags,Qos)>>,undefined).

exit_park_mode(Socket,Handle) ->
  hci_socket:call(Socket,?OGF_LINK_POLICY,?OCF_EXIT_PARK_MODE,<<?exit_park_mode_cp_bin(Handle)>>,undefined).

park_mode(Socket,Handle,Max_interval,Min_interval) ->
  hci_socket:call(Socket,?OGF_LINK_POLICY,?OCF_PARK_MODE,<<?park_mode_cp_bin(Handle,Max_interval,Min_interval)>>,undefined).

exit_sniff_mode(Socket,Handle) ->
  hci_socket:call(Socket,?OGF_LINK_POLICY,?OCF_EXIT_SNIFF_MODE,<<?exit_sniff_mode_cp_bin(Handle)>>,undefined).

sniff_mode(Socket,Handle,Max_interval,Min_interval,Attempt,Timeout) ->
  hci_socket:call(Socket,?OGF_LINK_POLICY,?OCF_SNIFF_MODE,<<?sniff_mode_cp_bin(Handle,Max_interval,Min_interval,Attempt,Timeout)>>,undefined).

hold_mode(Socket,Handle,Max_interval,Min_interval) ->
  hci_socket:call(Socket,?OGF_LINK_POLICY,?OCF_HOLD_MODE,<<?hold_mode_cp_bin(Handle,Max_interval,Min_interval)>>,undefined).

flow_spec_modify(Socket) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_FLOW_SPEC_MODIFY,<<>>,undefined).

logical_link_cancel(Socket) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_LOGICAL_LINK_CANCEL,<<>>,undefined).

disconnect_logical_link(Socket,Handle) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_DISCONNECT_LOGICAL_LINK,<<?disconnect_logical_link_cp_bin(Handle)>>,undefined).

accept_logical_link(Socket) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_ACCEPT_LOGICAL_LINK,<<>>,undefined).

create_logical_link(Socket,Handle,Tx_flow,Rx_flow) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_CREATE_LOGICAL_LINK,<<?create_logical_link_cp_bin(Handle,Tx_flow,Rx_flow)>>,undefined).

disconnect_physical_link(Socket,Handle,Reason) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_DISCONNECT_PHYSICAL_LINK,<<?disconnect_physical_link_cp_bin(Handle,Reason)>>,undefined).

accept_physical_link(Socket) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_ACCEPT_PHYSICAL_LINK,<<>>,undefined).

create_physical_link(Socket,Handle,Key_length,Key_type,Key) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_CREATE_PHYSICAL_LINK,<<?create_physical_link_cp_bin(Handle,Key_length,Key_type,Key)>>,undefined).

io_capability_neg_reply(Socket,Bdaddr,Reason) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_IO_CAPABILITY_NEG_REPLY,<<?io_capability_neg_reply_cp_bin(Bdaddr,Reason)>>,undefined).

remote_oob_data_neg_reply(Socket) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_REMOTE_OOB_DATA_NEG_REPLY,<<>>,undefined).

remote_oob_data_reply(Socket,Bdaddr,Hash,Randomizer) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_REMOTE_OOB_DATA_REPLY,<<?remote_oob_data_reply_cp_bin(Bdaddr,Hash,Randomizer)>>,undefined).

user_passkey_neg_reply(Socket) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_USER_PASSKEY_NEG_REPLY,<<>>,undefined).

user_passkey_reply(Socket,Bdaddr,Passkey) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_USER_PASSKEY_REPLY,<<?user_passkey_reply_cp_bin(Bdaddr,Passkey)>>,undefined).

user_confirm_neg_reply(Socket) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_USER_CONFIRM_NEG_REPLY,<<>>,undefined).

user_confirm_reply(Socket,Bdaddr) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_USER_CONFIRM_REPLY,<<?user_confirm_reply_cp_bin(Bdaddr)>>,undefined).

io_capability_reply(Socket,Bdaddr,Capability,Oob_data,Authentication) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_IO_CAPABILITY_REPLY,<<?io_capability_reply_cp_bin(Bdaddr,Capability,Oob_data,Authentication)>>,undefined).

reject_sync_conn_req(Socket,Bdaddr,Reason) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_REJECT_SYNC_CONN_REQ,<<?reject_sync_conn_req_cp_bin(Bdaddr,Reason)>>,undefined).

accept_sync_conn_req(Socket,Bdaddr,Tx_bandwith,Rx_bandwith,Max_latency,Voice_setting,Retrans_effort,Pkt_type) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_ACCEPT_SYNC_CONN_REQ,<<?accept_sync_conn_req_cp_bin(Bdaddr,Tx_bandwith,Rx_bandwith,Max_latency,Voice_setting,Retrans_effort,Pkt_type)>>,undefined).

setup_sync_conn(Socket,Handle,Tx_bandwith,Rx_bandwith,Max_latency,Voice_setting,Retrans_effort,Pkt_type) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_SETUP_SYNC_CONN,<<?setup_sync_conn_cp_bin(Handle,Tx_bandwith,Rx_bandwith,Max_latency,Voice_setting,Retrans_effort,Pkt_type)>>,undefined).

read_lmp_handle(Socket) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_READ_LMP_HANDLE,<<>>,undefined).

read_clock_offset(Socket,Handle) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_READ_CLOCK_OFFSET,<<?read_clock_offset_cp_bin(Handle)>>,undefined).

read_remote_version(Socket,Handle) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_READ_REMOTE_VERSION,<<?read_remote_version_cp_bin(Handle)>>,undefined).

read_remote_ext_features(Socket,Handle,Page_num) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_READ_REMOTE_EXT_FEATURES,<<?read_remote_ext_features_cp_bin(Handle,Page_num)>>,undefined).

read_remote_features(Socket,Handle) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_READ_REMOTE_FEATURES,<<?read_remote_features_cp_bin(Handle)>>,undefined).

remote_name_req_cancel(Socket,Bdaddr) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_REMOTE_NAME_REQ_CANCEL,<<?remote_name_req_cancel_cp_bin(Bdaddr)>>,undefined).

remote_name_req(Socket,Bdaddr,Pscan_rep_mode,Pscan_mode,Clock_offset) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_REMOTE_NAME_REQ,<<?remote_name_req_cp_bin(Bdaddr,Pscan_rep_mode,Pscan_mode,Clock_offset)>>,undefined).

master_link_key(Socket,Key_flag) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_MASTER_LINK_KEY,<<?master_link_key_cp_bin(Key_flag)>>,undefined).

change_conn_link_key(Socket,Handle) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_CHANGE_CONN_LINK_KEY,<<?change_conn_link_key_cp_bin(Handle)>>,undefined).

set_conn_encrypt(Socket,Handle,Encrypt) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_SET_CONN_ENCRYPT,<<?set_conn_encrypt_cp_bin(Handle,Encrypt)>>,undefined).

auth_requested(Socket,Handle) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_AUTH_REQUESTED,<<?auth_requested_cp_bin(Handle)>>,undefined).

set_conn_ptype(Socket,Handle,Pkt_type) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_SET_CONN_PTYPE,<<?set_conn_ptype_cp_bin(Handle,Pkt_type)>>,undefined).

pin_code_neg_reply(Socket) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_PIN_CODE_NEG_REPLY,<<>>,undefined).

pin_code_reply(Socket,Bdaddr,Pin_len,Pin_code) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_PIN_CODE_REPLY,<<?pin_code_reply_cp_bin(Bdaddr,Pin_len,Pin_code)>>,undefined).

link_key_neg_reply(Socket,Bdaddr) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_LINK_KEY_NEG_REPLY,<<?link_key_neg_reply_cp_bin(Bdaddr)>>,undefined).

link_key_reply(Socket,Bdaddr,Link_key) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_LINK_KEY_REPLY,<<?link_key_reply_cp_bin(Bdaddr,Link_key)>>,undefined).

reject_conn_req(Socket,Bdaddr,Reason) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_REJECT_CONN_REQ,<<?reject_conn_req_cp_bin(Bdaddr,Reason)>>,undefined).

accept_conn_req(Socket,Bdaddr,Role) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_ACCEPT_CONN_REQ,<<?accept_conn_req_cp_bin(Bdaddr,Role)>>,undefined).

create_conn_cancel(Socket,Bdaddr) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_CREATE_CONN_CANCEL,<<?create_conn_cancel_cp_bin(Bdaddr)>>,undefined).

add_sco(Socket,Handle,Pkt_type) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_ADD_SCO,<<?add_sco_cp_bin(Handle,Pkt_type)>>,undefined).

disconnect(Socket,Handle,Reason) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_DISCONNECT,<<?disconnect_cp_bin(Handle,Reason)>>,undefined).

create_conn(Socket,Bdaddr,Pkt_type,Pscan_rep_mode,Pscan_mode,Clock_offset,Role_switch) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_CREATE_CONN,<<?create_conn_cp_bin(Bdaddr,Pkt_type,Pscan_rep_mode,Pscan_mode,Clock_offset,Role_switch)>>,undefined).

exit_periodic_inquiry(Socket) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_EXIT_PERIODIC_INQUIRY,<<>>,undefined).

periodic_inquiry(Socket,Max_period,Min_period,Lap,Length,Num_rsp) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_PERIODIC_INQUIRY,<<?periodic_inquiry_cp_bin(Max_period,Min_period,Lap,Length,Num_rsp)>>,undefined).

inquiry_cancel(Socket) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_INQUIRY_CANCEL,<<>>,undefined).

inquiry(Socket,Lap,Length,Num_rsp) ->
  hci_socket:call(Socket,?OGF_LINK_CTL,?OCF_INQUIRY,<<?inquiry_cp_bin(Lap,Length,Num_rsp)>>,undefined).

