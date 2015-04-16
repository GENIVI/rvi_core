%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Enumerate devices on win32
%%% @end
%%% Created :  5 Apr 2013 by Tony Rogvall <tony@rogvall.se>

-module(uart_win32_devices).

-compile(export_all).

%% HKLM\ENUM
%%   |-BIOS
%%     |-*PNP0501
%%       |-0D (or any other value, this is not important for us)
%%         · CLASS=        "Ports"
%%         · PORTNAME=     "COM1"
%%         · FRIENDLYNAME= "Communications Port (COM1)"
%% 

%% Win2000/XP
%%   \\HKEY_LOCAL_MACHINE\\System\\CurrentControlSet\\Enum
%%
%% WinNT4.0
%%   \\HKEY_LOCAL_MACHINE\\Hardware\\DEVICEMAP\\SERIALCOMM
%%
scan_enum_tree() ->
    scan_enum_tree("\\hkey_local_machine\\System\\CurrentControlSet\\Enum").
    
scan_enum_tree(Base) ->
    {ok,W} = win32reg:open([read]),
    ok = win32reg:change_key(W, Base),
    {ok, Types} = win32reg:sub_keys(W),
    %% FIXME: remove keys that can not lead to serial devices!
    Types1 = Types -- ["DISPLAY", "IDE", "STORAGE", "LPTENUM", "SCSI", "USBSTOR", "USBPRINT" ],
    scan_types(W, Base, Types1, []).

scan_types(W, Base, [Key|Ks], Acc) ->
    Base1 = Base ++ "\\" ++ Key,
    ok = win32reg:change_key(W, Base1),
    {ok, Devices} = win32reg:sub_keys(W),
    Acc1 = scan_devices(W, Base1, Devices, Acc),
    scan_types(W, Base, Ks, Acc1);
scan_types(_W, _Base, [], Acc) ->
    Acc.

%% keys like: "VID_0403+PID_6001+FTFBXORBA"
scan_devices(W, Base, [Key|Ks], Acc) ->
    Base1 = Base ++ "\\" ++ Key,
    ok = win32reg:change_key(W, Base1),
    {ok, Instances} = win32reg:sub_keys(W),
    Acc1 = scan_instances(W, Base1, Instances, Acc),
    scan_devices(W, Base, Ks, Acc1);
scan_devices(_W, _Base, [], Acc) ->
    Acc.

%% keys like: "0000"
scan_instances(W, Base, [Key|Ks], Acc) ->
    Base1 = Base ++ "\\" ++ Key,
    ok = win32reg:change_key(W, Base1),
    {ok,KVs} = win32reg:values(W),
    case is_serial_port(KVs) of
	true ->
	    DVs =
		case win32reg:change_key(W, Base1++"\\"++"Device Parameters") of
		    ok ->
			{ok,DVs0} = win32reg:values(W), 
			DVs0;
		   _ -> []
	       end,
	    case port_name(KVs,DVs) of
		Name="COM"++_ ->
		    io:format("Serial: ~s\n", [Name]),
		    io:format("Serial Params\n"),
		    lists:foreach(
		      fun({K,V}) ->
			      io:format("    ~s : ~p\n", [K,V])
		      end, KVs),
		    io:format("Device params\n"),
		    lists:foreach(
		      fun({K,V}) ->
			      io:format("    ~s : ~p\n", [K,V])
		      end, DVs),
		    scan_instances(W, Base, Ks, [{Name,KVs,DVs}|Acc]);
		_ ->
		    scan_instances(W, Base, Ks, Acc)
	    end;
	false ->
	    scan_instances(W, Base, Ks, Acc)
    end;
scan_instances(_W, _Base, [], Acc) ->
    Acc.

is_serial_port(KVs) ->
    strcaseeq(get_value("Class", KVs),"PORTS")
	orelse
    case get_value("ClassGUID", KVs) of
	"{4D36E978-E325-11CE-BFC1-08002BE10318}" -> true;
	_ -> false
    end.

port_name(KVs,DVs) ->
    case get_value("PORTNAME", KVs) of
	undefined ->
	    get_value("PORTNAME", DVs);
	Name -> Name
    end.

get_value(K, KVs) ->
    get_value(K, KVs, undefined).

get_value(K, [{K,V}|_], _D) -> V;
get_value(K, [{K0,V}|KVs], D) ->
    case strcaseeq(K,K0) of
	true -> V;
	false -> get_value(K, KVs, D)
    end;
get_value(_K, [], D) -> D.

strcaseeq([A|As], [B|Bs]) -> 
    A1 = string:to_upper(A),
    B1 = string:to_upper(B),
    if A1 =:= B1 -> strcaseeq(As,Bs);
       true -> false
    end;
strcaseeq([], []) -> 
    true;
strcaseeq(_, _) -> 
    false.
