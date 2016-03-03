%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    launch netlink_drv
%%% @end
%%% Created : 30 Nov 2011 by Tony Rogvall <tony@rogvall.se>

-module(netlink_drv).
-export([open/1, close/1, send/2]).
-export([add_membership/2, 
	 drop_membership/2,
	 set_rcvbuf/2, set_sndbuf/2,
	 get_rcvbuf/1, get_sndbuf/1,
	 get_sizeof/1,
	 deactivate/1,
	 activate/1, 
	 activate/2,
	 debug/2]).

%% deugging
-compile(export_all).

-include("netlink.hrl").

-define(CMD_ADD_MEMBERSHIP,   1).
-define(CMD_DROP_MEMBERSHIP,  2).
-define(CMD_ACTIVE,           3).
-define(CMD_DEBUG,            4).
-define(CMD_SET_RCVBUF,       5).
-define(CMD_SET_SNDBUF,       6).
-define(CMD_GET_RCVBUF,       7).
-define(CMD_GET_SNDBUF,       8).
-define(CMD_GET_SIZEOF,       9).

-define(DLOG_DEBUG,     7).
-define(DLOG_INFO,      6).
-define(DLOG_NOTICE,    5).
-define(DLOG_WARNING,   4).
-define(DLOG_ERROR,     3).
-define(DLOG_CRITICAL,  2).
-define(DLOG_ALERT,     1).
-define(DLOG_EMERGENCY, 0).
-define(DLOG_NONE,     -1).

add_membership(Port,Msg) when is_integer(Msg) ->
    port_call(Port, ?CMD_ADD_MEMBERSHIP, <<Msg:32>>).

drop_membership(Port,Msg) when is_integer(Msg) ->
    port_call(Port, ?CMD_DROP_MEMBERSHIP, <<Msg:32>>).

set_rcvbuf(Port,Size) when is_integer(Size), Size >= 0 ->
    port_call(Port, ?CMD_SET_RCVBUF, <<Size:32>>).

set_sndbuf(Port,Size) when is_integer(Size), Size >= 0 ->
    port_call(Port, ?CMD_SET_SNDBUF, <<Size:32>>).

get_rcvbuf(Port) ->
    port_call(Port, ?CMD_GET_RCVBUF, <<>>).

get_sndbuf(Port) ->
    port_call(Port, ?CMD_GET_SNDBUF, <<>>).

get_sizeof(Port) ->
    port_call(Port, ?CMD_GET_SIZEOF, <<>>).

deactivate(Port) ->
    activate(Port, 0).    

activate(Port) ->
    activate(Port, -1).

activate(Port, N) when is_integer(N), N >= -1, N < 16#7fffffff ->
    port_call(Port, ?CMD_ACTIVE, <<N:32>>).

debug(Port,Level) when is_atom(Level) ->
    L = level(Level),
    port_call(Port, ?CMD_DEBUG, <<L:32>>).

open(Protocol) when is_integer(Protocol), Protocol >= 0 ->
    Driver = "netlink_drv",
    Path = code:priv_dir(netlink),
    io:format("load_driver '~s' from: '~s'\n", [Driver, Path]),
    case erl_ddll:load_driver(Path, Driver) of
	ok ->
	    Arg = integer_to_list(Protocol),
	    erlang:open_port({spawn_driver, Driver++" "++Arg}, [binary]);
	{error,Error} ->
	    io:format("Error: ~s\n", [erl_ddll:format_error_int(Error)]),
	    erlang:error(Error)
    end.

close(Port) ->
    erlang:port_close(Port).

send(Port, Command) ->
    erlang:port_command(Port, Command).

port_call(Port, Cmd, Data) ->
    case erlang:port_control(Port, Cmd, Data) of
	<<0>> ->
	    ok;
	<<255,E/binary>> -> 
	    {error, erlang:binary_to_atom(E, latin1)};
	<<254,E/binary>> -> 
	    {error, binary_to_list(E)};
	<<1,Y:8>> -> {ok,Y};
	<<1,Y:16/native-unsigned>> -> {ok, Y};
	<<1,Y:32/native-unsigned>> -> {ok, Y};
	<<1,Y:64/native-unsigned>> -> {ok, Y};
	<<2,X:32/native-unsigned,Y:32/native-unsigned>> -> {ok,{X,Y}};
	<<3,X/binary>> -> {ok,X};
	<<4,X/binary>> -> {ok,binary_to_list(X)}
    end.
	
%% convert symbolic to numeric level
level(true)  -> ?DLOG_DEBUG;
level(false) -> ?DLOG_NONE;
level(debug) -> ?DLOG_DEBUG;
level(info)  -> ?DLOG_INFO;
level(notice) -> ?DLOG_NOTICE;
level(warning) -> ?DLOG_WARNING;
level(error) -> ?DLOG_ERROR;
level(critical) -> ?DLOG_CRITICAL;
level(alert) -> ?DLOG_ALERT;
level(emergency) -> ?DLOG_EMERGENCY;
level(none) -> ?DLOG_NONE.

