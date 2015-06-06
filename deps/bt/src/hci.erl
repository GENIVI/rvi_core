%%% @author Tony Rogvall <tony@up13>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    HCI commands
%%% @end
%%% Created : 18 Apr 2015 by Tony Rogvall <tony@up13>

-module(hci).

-compile(export_all).

-include("../include/hci_drv.hrl").
-include("hci_api.hrl").

-define(DEFAULT_TIMEOUT, 5000).
-define(INQUIRY_TIMEOUT, 10000).

open(DevID) ->
    Hci = hci_drv:open(),
    case hci_drv:bind(Hci, DevID) of
	ok -> 
	    hci_drv:activate(Hci),
	    {ok,Hci};
	Error -> Error
    end.

open() ->
    Hci = hci_drv:open(),
    case hci_drv:get_dev_list(Hci) of
	{ok,[]} -> {error, enoent};
	{ok,[{DevID,_}|_]} ->
	    case hci_drv:bind(Hci, DevID) of
		ok ->
		    hci_drv:activate(Hci),
		    {ok,Hci};
		Error -> Error
	    end;
	Error ->
	    Error
    end.

close(Hci) ->
    hci_drv:close(Hci).

%% dump information about bluetooth devices
i() ->
    Hci = hci_drv:open(),
    case hci_drv:get_dev_list(Hci) of
	{ok,Devs} ->
	    lists:foreach(
	      fun({DevID,_DevFlags}) ->
		      case hci_drv:get_dev_info(Hci, DevID) of
			  {ok, Info} ->
			      io:format("~s\n", 
					[hci_util:format_hci_dev_info(Info)]);
			  Error ->
			      io:format("error: ~p\n", [Error])
		      end
	      end, Devs);
	Error ->
	    Error
    end.

scan() ->    
    scan(?INQUIRY_TIMEOUT).

scan(Timeout) ->
    {ok,Hci} = open(),
    case hci_drv:inquiry(Hci, Timeout, 10, 0, 0) of
	{ok,Is} ->
	    lists:foreach(
	      fun(I) ->
		      io:format("~s: dev_class=~w, clock_offset=~w\n",
				[bt:format_address(I#inquiry_info.bdaddr),
				 I#inquiry_info.dev_class,
				 I#inquiry_info.clock_offset]),
		      io:format("     pscan_rep_mode:~w\n"
				"     pscan_period_mode:~w\n"
				"     pscan_mode:~w\n",
				[ I#inquiry_info.pscan_rep_mode,
				  I#inquiry_info.pscan_period_mode,
				  I#inquiry_info.pscan_mode]),
		      case read_remote_name(Hci, I, ?DEFAULT_TIMEOUT) of
			  {ok,Name} ->
			      io:format("    Name: ~s\n", [Name]);
			  _Error ->
			      ok
		      end
	      end, Is);
	Error ->
	    Error
    end.

    
create_connection(Hci, Bdaddr, Pkt_type, Clock_offset, Role_switch, Timeout) ->
    Pscan_rep_mode = 16#02,
    Pscan_mode = 0,
    case call(Hci,?OGF_LINK_CTL,?OCF_CREATE_CONN,
	      <<?create_conn_cp_bin(Bdaddr,Pkt_type,Pscan_rep_mode,Pscan_mode,Clock_offset,Role_switch)>>,
	      ?EVT_CONN_COMPLETE, Timeout) of
	{ok, <<?evt_conn_complete_bin(0,Handle,_Bdaddr,_Link_type,_Encr_mode)>>} ->
	    {ok, Handle};
	{ok, <<?evt_conn_complete_bin(Status,_Handle,_Bdaddr,_Link_type,_Encr_mode)>>} ->
	    %% fixme: decode status
	    {error, Status};
	Error ->
	    Error
    end.

disconnect(Hci, Handle, Reason, Timeout) ->
    case call(Hci, ?OGF_LINK_CTL, ?OCF_DISCONNECT, 
	      <<?disconnect_cp_bin(Handle,Reason)>>,
	      ?EVT_DISCONN_COMPLETE, Timeout) of
	{ok, <<?evt_disconn_complete_bin(0,Handle,Reason)>>} ->
	    ok;
	{ok, <<?evt_disconn_complete_bin(_Status,Handle,Reason)>>} ->
	    {error,eio};
	Error ->
	    Error
    end.

read_remote_name(Bdaddr) ->
    with_socket(fun(Hci) -> read_remote_name(Hci,Bdaddr,?DEFAULT_TIMEOUT) end).
			

read_remote_name(Hci,
		 #inquiry_info {
		    bdaddr=Bdaddr,
		    pscan_rep_mode=Pscan_rep_mode,
		    pscan_mode=Pscan_mode,
		    clock_offset=Clock_offset }, Timeout) ->
    read_remote_name(Hci,Bdaddr,Pscan_rep_mode,Pscan_mode,
		     Clock_offset bor 16#8000,Timeout);
read_remote_name(Hci, Bdaddr, Timeout) when is_binary(Bdaddr) ->
    read_remote_name(Hci,Bdaddr,16#02,16#00,16#0000,Timeout);
read_remote_name(Hci, Bdaddr, Timeout) when is_list(Bdaddr) ->
    case bt:getaddr(Bdaddr) of
	{ok,{A,B,C,D,E,F}} ->
	    read_remote_name(Hci, <<F,E,D,C,B,A>>, Timeout);
	Error ->
	    Error
    end.
    
read_remote_name(Hci,Bdaddr,Pscan_rep_mode,Pscan_mode,Clock_offset,Timeout) ->
    case call(Hci,?OGF_LINK_CTL,?OCF_REMOTE_NAME_REQ,
	      <<?remote_name_req_cp_bin(Bdaddr,Pscan_rep_mode,Pscan_mode,
					Clock_offset)>>,
	      ?EVT_REMOTE_NAME_REQ_COMPLETE,Timeout) of
	{ok, <<?evt_remote_name_req_complete_bin(0,_Bdaddr,Name)>>} ->
	    {ok,hci_util:c_string(Name)};
	{ok, <<?evt_remote_name_req_complete_bin(_Status,_Bdaddr,_Name)>>} ->
	    {error, eio};
	Error -> Error
    end.

read_local_name() ->
    with_socket(fun(Hci) -> read_local_name(Hci, ?DEFAULT_TIMEOUT) end).

read_local_name(Hci, Timeout) ->
    case call(Hci,?OGF_HOST_CTL,?OCF_READ_LOCAL_NAME,<<>>,0,Timeout) of
	{ok, <<?read_local_name_rp_bin(0,Name)>>} ->
	    {ok, hci_util:c_string(Name)};
	{ok, <<?read_local_name_rp_bin(_Status,_Name)>>} ->
	    {error, eio};
	Error ->
	    Error
    end.


%% sending data and calling procedures over HCI socket

send(Hci,Opcode,Data) ->
    Pkt = <<?HCI_COMMAND_PKT,Opcode:16/little,
	    (byte_size(Data)):8,Data/binary>>,
    R = hci_drv:send(Hci,Pkt),
    io:format("send ~p = ~p\n", [Pkt,R]),
    R.

send(Hci, OGF, OCF, Data) ->
    send(Hci, ?cmd_opcode_pack(OGF,OCF), Data).

call(Hci,OGF,OCF,Data,Event,Timeout) ->
    Opcode = ?cmd_opcode_pack(OGF,OCF),
    {ok,OldFilter} = hci_drv:get_filter(Hci),
    %% io:format("call: saved_filter = ~p\n", [OldFilter]),
    NewFilter = hci_drv:make_filter(Opcode,
				    [?HCI_EVENT_PKT],
				    [?EVT_CMD_STATUS,
				     ?EVT_CMD_COMPLETE,
				     ?EVT_LE_META_EVENT,
				     Event]),
    %% io:format("call: new_filter = ~p\n", [NewFilter]),
    case hci_drv:set_filter(Hci, NewFilter) of
	ok ->
	    true = send(Hci,Opcode,Data),
	    Reply = wait(Hci,Opcode,Event,10,Timeout),
	    hci_drv:set_filter(Hci, OldFilter),
	    Reply;
	Error ->
	    Error
    end.

wait(Hci,Opcode,Event,Try,Timeout) ->
    TRef = start_timer(Timeout),
    Result = wait_(Hci,Opcode,Event,Try,TRef),
    case Result of
	timeout ->
	    cancel_timer(TRef),
	    {error, timedout};
	Result ->
	    Result
    end.

wait_(_Hci,_Opcode,_Event,0,TRef) ->
    cancel_timer(TRef),
    {error, timedout};

wait_(Hci,Opcode,Event,Try,TRef) ->
    receive
	{Hci,Data0={data,<<_:8,Evt:8,Plen:8,Packet:Plen/binary,_/binary>>}} ->
	    io:format("Got data ~p\n", [Data0]),
	    case Evt of
		?EVT_CMD_STATUS ->
		    io:format("got cmd_status\n", []),
		    case Packet of
			<<?evt_cmd_status_bin(0,_Ncmd,Opcode),R/binary>> ->
			    if Evt =/= Event ->
				    wait_(Hci,Opcode,Event,Try-1,TRef);
			       true ->
				    {ok,R}
			    end;
			<<?evt_cmd_status_bin(_Status,_Ncmd,Opcode),_R/binary>> ->
			    {error, eio};  %% other status?
			<<?evt_cmd_status_bin(_S,_Ncmd,_Opcode),_R/binary>> ->
			    wait_(Hci,Opcode,Event,Try-1,TRef)
		    end;
		?EVT_CMD_COMPLETE ->
		    io:format("got cmd_complete\n", []),
		    case Packet of
			<<?evt_cmd_complete_bin(_Ncmd,Opcode),R/binary>> ->
			    {ok,R};
			<<?evt_cmd_complete_bin(_Ncmd,_Opcode),_R/binary>> ->
			    wait_(Hci,Opcode,Event,Try-1,TRef)
		    end;

		?EVT_REMOTE_NAME_REQ_COMPLETE when Evt =:= Event ->
		    io:format("got remote_name_req_complete\n", []),
		    case Packet of
			<<?evt_remote_name_req_complete_bin(_Status,_Bdaddr,_Name)>> ->
			    %% fixme: check Bdaddr!
			    {ok,Packet}
		    end;
		?EVT_LE_META_EVENT ->
		    io:format("got evt_le_meta_event\n", []),
		    case Packet of
			<<?evt_le_meta_event_bin(SEvt,_D1),LePacket/binary>> ->
			    if SEvt =:= Event ->
				    {ok,LePacket};
			       true ->
				    wait_(Hci,Opcode,Event,Try-1,TRef)
			    end
		    end;
		_ ->
		    io:format("got event ~p\n", [Evt]),
		    {ok,Packet}
	    end;

	{timeout,TRef,_} ->
	    timeout
    end.

rev_bin(Bin) ->
    list_to_binary(lists:reverse(binary_to_list(Bin))).

start_timer(0) -> undefined;
start_timer(Timeout) -> erlang:start_timer(Timeout, self(), timeout).

cancel_timer(undefined) -> false;
cancel_timer(TRef) ->
    erlang:cancel_timer(TRef),
    receive {timeout,TRef,_} -> ok
    after 0 -> ok
    end.

with_socket(Fun) ->
    {ok,S} = open(),
    try Fun(S) of
	Result ->
	    Result
    catch
	error:Error ->
	    io:format("hci: with_socket crash: ~p\n", [Error]),
	    lists:foreach(fun(Item) -> print_item(Item) end,
			  erlang:get_stacktrace()),
	    {error,Error}
    after
	close(S)
    end.

print_item({Module,Function,ArityArgs,Location}) ->
    File = proplists:get_value(file,Location,""),
    Line = proplists:get_value(line,Location,0),
    {Arity,Args} = 
	if is_list(ArityArgs) -> {length(ArityArgs),ArityArgs};
	   is_integer(ArityArgs) -> {ArityArgs, []}
	end,
    io:format("  ~s:~w: ~s:~s/~w ~p\n", [File,Line,Module,Function,Arity,Args]).
