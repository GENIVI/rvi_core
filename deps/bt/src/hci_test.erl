%%% @author Tony Rogvall <tony@up13>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%     Testing HCI socket
%%% @end
%%% Created : 14 Apr 2015 by Tony Rogvall <tony@up13>

-module(hci_test).

-compile(export_all).
-include("hci_api.hrl").

inquiry() ->
    with_socket(0, fun(S) -> inquiry(S) end).

pinquiry() ->
    with_socket(0, fun(S) -> pinquiry(S) end).    

%% scan for 10*1.28 seconds, wait for max 5 replies
inquiry(Hci) ->
    Lap = <<16#33,16#8b,16#9e>>,
    hci_api:inquiry(Hci, Lap, 10, 5).

%% scan for 10*1.28 seconds, wait for max 5 replies
pinquiry(Hci) ->
    Lap = <<16#33,16#8b,16#9e>>,
    Max = 100, Min = 50, 
    hci_api:periodic_inquiry(Hci, Max, Min, Lap, 10, 2).


local_name() -> local_name(0).
local_name(DevID) ->
    with_socket(DevID, fun(S) -> hci_api:read_local_name(S) end).



with_socket(DevID, Fun) ->	      
    {ok,S} = hci_socket:open(DevID),
    %% hci_socket:debug(S, debug),
    try Fun(S) of
	Result ->
	    hci_socket:close(S),
	    Result
    catch
	error:Error ->
	    hci_socket:close(S),
	    {error,Error}
    end.
