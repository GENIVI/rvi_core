%%
%% Utils used by HCI
%%
-module(hci_util).
-compile(export_all).

-include("include/hci_drv.hrl").


c_string(Data) when is_binary(Data) ->
    c_string_(binary_to_list(Data));
c_string(Data) when is_list(Data) ->
    c_string_(Data).

c_string_([0|_]) -> [];
c_string_([H|T]) -> [H|c_string_(T)].


find_enum_name(Value, Flags, Default) when is_integer(Value) ->
    case lists:keyfind(Value, 2, Flags) of
	false -> Default;
	{Name, _Value} -> Name
    end.

find_enum_name(Value, Flags) ->
    case find_enum_name(Value, Flags, undefined) of
	undefined -> erlang:error(badarg);
	Name -> Name
    end.

find_enum_value(Name, Flags, Default) when is_atom(Name) ->
    case lists:keyfind(atom_to_list(Name), 1, Flags) of
	false -> Default;
	{_, Value} -> Value
    end;
find_enum_value(Name, Flags, Default) when is_list(Name) ->
    case lists:keyfind(Name, 1, Flags) of
	false -> Default;
	{_, Value} -> Value
    end.

find_enum_value(Name, Flags) ->
    case find_enum_value(Name, Flags, undefined) of
	undefined -> erlang:error(badarg);
	Value -> Value
    end.


format_enum(Value, Kv) ->
    case lists:keyfind(Value, 2, Kv) of
        false -> "";
        {Name, Value} -> Name
    end.

format_bits(Value, KvList) ->
    format_bits_(Value, KvList, []).

format_bits_(0, _Kv, Acc) ->
    string:join(lists:reverse(Acc), ",");
format_bits_(Value, [{Key,Bits}|Kv], Acc) ->
    if Value band Bits =:= Bits ->
            format_bits_(Value band (bnot Bits),Kv,[Key|Acc]);
       true ->
            format_bits_(Value, Kv, Acc)
    end;
format_bits_(Value, [], Acc) ->
    Acc1 = [integer_to_list(Value) | Acc],
    string:join(lists:reverse(Acc1), ",").

format_hci_dev_info(#hci_dev_info {
		      dev_id = _Dev_id,
		      name = Name,
		      bdaddr = BdAddr,
		      flags = Flags,
		      type = Type,
		      features = _Features,
		      pkt_type = _Pkt_type,
		      link_policy = _Link_policy,
		      link_mode = _Link_mode,
		      acl_mtu = Acl_mtu,
		      acl_pkts = Acl_pkts,
		      sco_mtu = Sco_mtu,
		      sco_pkts = Sco_pkts,
		      stat = S}) ->
    Bus = find_enum_name(Type band 16#0f,
			 kv_dev_bus(), "UNKNOWN"), 
    Type1 = find_enum_name((Type band 16#30) bsr 4,
			   kv_dev_type(), "UNKNOWN"),
    io_lib:format(
      "~s:	Type: ~s  Bus: ~s\n"
      "\tBD Address: ~s  ACL MTU: ~w:~w  SCO MTU: ~w:~w\n"
      "\t~s\n"
      "~s\n",
      [Name, Type1, Bus,
       bt:format_address(BdAddr),
       Acl_mtu, Acl_pkts, Sco_mtu, Sco_pkts,
       format_bits(Flags, kv_hci_dev_info_flags()),
       format_hci_dev_stats(S)]).
    
format_hci_dev_stats(#hci_dev_stats {
			 err_rx = Err_rx,
			 err_tx = Err_tx,
			 cmd_tx = Cmd_tx,
			 evt_rx = Evt_rx,
			 acl_tx = Acl_tx,
			 acl_rx = Acl_rx,
			 sco_tx = Sco_tx,
			 sco_rx = Sco_rx,
			 byte_rx = Byte_rx,
			 byte_tx = Byte_tx }) ->
    io_lib:format(
      "\tRX bytes:~w acl:~w sco:~w events:~w errors:~w\n"
      "\tTX bytes:~w acl:~w sco:~w commands:~w errors:~w",
      [Byte_rx, Acl_rx, Sco_rx, Evt_rx, Err_rx,
       Byte_tx, Acl_tx, Sco_tx, Cmd_tx, Err_tx]).

bdaddr({A,B,C,D,E,F}) ->
    {ok,<<F,E,D,C,B,A>>};
bdaddr(Bin = <<_,_,_,_,_,_>>) -> {ok,Bin};  %% assume binary format is internal!
bdaddr(Addr) when is_list(Addr) ->
    case bt:getaddr(Addr) of
	{ok,{A,B,C,D,E,F}} ->
	    {ok,<<F,E,D,C,B,A>>};
	Error ->
	    Error
    end.

kv_dev_type() ->
[
 {"BR/EDR",      ?HCI_BREDR},
 {"AMP",         ?HCI_AMP}
].

kv_dev_bus() ->
[
 {"VIRTUAL",?HCI_VIRTUAL},
 {"USB", ?HCI_USB },
 {"PCCARD", ?HCI_PCCARD },
 {"UART", ?HCI_UART },
 {"RS232",?HCI_RS232},
 {"PCI", ?HCI_PCI},
 {"SDIO", ?HCI_SDIO}
].
    

%% scan modes
kv_scan() ->
[
 { "OFF",       ?SCAN_DISABLED },
 { "ISCAN",     ?SCAN_INQUIRY },
 { "PSCAN",     ?SCAN_PAGE },
 { "PISCAN",    ?SCAN_PAGE bor ?SCAN_INQUIRY }
].

%% HCI device flags
kv_hci_dev_info_flags() ->
[
 { "UP",      (1 bsl ?HCI_UP) },
 { "INIT",    (1 bsl ?HCI_INIT) },
 { "RUNNING", (1 bsl ?HCI_RUNNING) },
 { "PSCAN",   (1 bsl ?HCI_PSCAN) },
 { "ISCAN",   (1 bsl ?HCI_ISCAN) },
 { "AUTH",    (1 bsl ?HCI_AUTH) },
 { "ENCRYPT", (1 bsl ?HCI_ENCRYPT) },
 { "INQUIRY", (1 bsl ?HCI_INQUIRY) },
 { "RAW",     (1 bsl ?HCI_RAW) }
].

kv_pkt_ptype() ->
[
 { "DM1",   ?HCI_DM1  },
 { "DM3",   ?HCI_DM3  },
 { "DM5",   ?HCI_DM5  },
 { "DH1",   ?HCI_DH1  },
 { "DH3",   ?HCI_DH3  },
 { "DH5",   ?HCI_DH5  },
 { "HV1",   ?HCI_HV1  },
 { "HV2",   ?HCI_HV2  },
 { "HV3",   ?HCI_HV3  },
 { "2-DH1", ?HCI_2DH1 },
 { "2-DH3", ?HCI_2DH3 },
 { "2-DH5", ?HCI_2DH5 },
 { "3-DH1", ?HCI_3DH1 },
 { "3-DH3", ?HCI_3DH3 },
 { "3-DH5", ?HCI_3DH5 }
].

kv_sco_ptype() ->
[
 { "HV1",   16#0001   },
 { "HV2",   16#0002   },
 { "HV3",   16#0004   },
 { "EV3",   ?HCI_EV3  },
 { "EV4",   ?HCI_EV4  },
 { "EV5",   ?HCI_EV5  },
 { "2-EV3", ?HCI_2EV3 },
 { "2-EV5", ?HCI_2EV5 },
 { "3-EV3", ?HCI_3EV3 },
 { "3-EV5", ?HCI_3EV5 }
].

kv_link_policy() ->
[
 { "NONE",       0               },
 { "RSWITCH",    ?HCI_LP_RSWITCH  },
 { "HOLD",       ?HCI_LP_HOLD     },
 { "SNIFF",      ?HCI_LP_SNIFF    },
 { "PARK",       ?HCI_LP_PARK     }
].

kv_link_mode() ->
[
 { "NONE",       0               },
 { "ACCEPT",     ?HCI_LM_ACCEPT   },
 { "MASTER",     ?HCI_LM_MASTER   },
 { "AUTH",       ?HCI_LM_AUTH     },
 { "ENCRYPT",    ?HCI_LM_ENCRYPT  },
 { "TRUSTED",    ?HCI_LM_TRUSTED  },
 { "RELIABLE",   ?HCI_LM_RELIABLE },
 { "SECURE",     ?HCI_LM_SECURE   }
].
