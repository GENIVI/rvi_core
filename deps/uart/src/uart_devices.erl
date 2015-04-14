%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Server that keep track on uart/tty devices in the system
%%% @end
%%% Created : 20 Feb 2013 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(uart_devices).

-behaviour(gen_server).

-compile(export_all).

%% API
-export([start_link/0]).
-export([start/0, stop/0]).

-import(lists, [map/2]).

-include_lib("kernel/include/file.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(debug(F,A), ok).  % io:format((F),(A))

-record(uart_device,
	{
	  name :: string(),    %% device name
	  path :: string(),    %% device dir path
	  id   :: string(),    %% internal node name (info)
	  owner :: pid(),      %% owner pid
	  mon  :: reference(), %% process monitor
	  avail                %% true it device is (known to be) available
	}).

-record(state, {
	  dref         :: undefined | reference(),
	  devices = [] :: [#uart_device{}],
	  sub = []     :: [{pid(),reference()}]
	 }).

%%%===================================================================
%%% API
%%%===================================================================

alloc(Name) ->
    gen_server:call(?SERVER, {alloc,self(),Name}).

release(Name) ->
    gen_server:call(?SERVER, {release,self(),Name}).

get_list() ->
    gen_server:call(?SERVER, get_list).

get_info(Name,Keys) ->
    gen_server:call(?SERVER, {get_info,Name,Keys}).

get_fullname(Name) ->
    gen_server:call(?SERVER, {get_fullname,Name}).

subscribe() ->
    gen_server:call(?SERVER, {subscribe,self()}).

unsubscribe(Ref) ->
    gen_server:call(?SERVER, {unsubscribe,self(),Ref}).

stop() ->    
    gen_server:call(?SERVER, stop).

i() ->
    case get_list() of
	{ok,Names} ->
	    lists:foreach(
	      fun(Name) ->
		      case get_info(Name,[id,owner]) of
			  [{id,ID},{owner,Owner}] ->
			      I = if ID =:= Name -> "";
				     true -> ID
				  end,
			      O = if is_pid(Owner) -> 
					  pid_to_list(Owner);
				     true -> 
					  "none"
				  end,
			      io:format("~-25s ~-15s ~-35s\n",
					[Name, O, I]);
			  [] ->
			      ok
		      end
	      end, Names);
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link([]).

start_link(Subs) when is_list(Subs) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Subs], []).

start() ->
    start([]).
start(Subs) when is_list(Subs) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [Subs], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Subs]) ->
    DRef = watch_uart_devices(),
    Ds  = list_uart_devices(),
    State0 = #state{ devices =[], dref=DRef },
    State1 = update_all(Ds, State0),
    State2 = subscribe_all_(Subs, State1),
    {ok, State2}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_list, _From, State) ->
    {reply, {ok, [D#uart_device.name || D <- State#state.devices]}, State};
handle_call({get_info,Name,Keys}, _From, State) ->
    case lists:keyfind(Name, #uart_device.name,State#state.devices) of
	false -> {reply, [], State};
	D ->
	    L = 
		lists:foldr(
		  fun(id,Acc)    -> [{id,D#uart_device.id}|Acc];
		     (path,Acc)  -> [{path,D#uart_device.path}|Acc];
		     (owner,Acc) -> [{owner,D#uart_device.owner}|Acc];
		     (_,Acc) -> Acc
		  end, [], Keys),
	    {reply, L, State}
    end;
handle_call({get_fullname,Name}, _From, State) ->
    case lists:keyfind(Name,#uart_device.name,State#state.devices) of
	false ->
	    {reply, {error,enoent}, State};
	_D=#uart_device {path = P, name = N} ->
	    Path = filename:join(P,N),
	    {reply, {ok,Path}, State}
    end;
handle_call({subscribe,Pid}, _From, State) ->
    {Ref,State1} = subscribe_(Pid, State),
    {reply,{ok,Ref},State1};
handle_call({unsubscribe,_Pid,Ref}, _From, State) ->
    {reply,ok,unsubscribe_(Ref,State)};
handle_call({alloc,Pid,Name}, _From, State) ->
    case lists:keytake(Name,#uart_device.name,State#state.devices) of
	false ->
	    {reply, {error,enoent}, State};
	{value,D,Ds} ->
	    if not D#uart_device.avail ->
		    {reply, {error,ebusy}, State};
	       true ->
		    Mon = erlang:monitor(process, Pid),
		    D1 = D#uart_device { avail = false,
					 owner = Pid,
					 mon   = Mon },
		    ?debug("ALLOC: ~p\n", [D1]),
		    Ds1 = [D1 | Ds],
		    Path = filename:join(D#uart_device.path,D#uart_device.name),
		    {reply, {ok,Path}, State#state { devices = Ds1 }}
	    end
    end;
handle_call({release,Pid,Name}, _From, State) ->
    case lists:keytake(Name,#uart_device.name,State#state.devices) of
	false ->
	    {reply, {error,enoent}, State};
	{value,D,Ds} ->
	    if D#uart_device.avail =:= true,
	       D#uart_device.owner =:= Pid ->
		    ?debug("RELEASE: ~p\n", [D]),
		    erlang:demonitor(D#uart_device.mon, [flush]),
		    D1 = D#uart_device { avail = true,
					 owner = undefined,
					 mon   = undefined },
		    Ds1 = [D1 | Ds],
		    {reply, ok, State#state { devices = Ds1 }};
	       true ->
		    {reply, {error,eperm}, State}
	    end
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    ?debug("Unknown call: ~p\n", [_Request]),
    Reply = {error,bad_call},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(scan_serial, State) ->
    Ds  = list_uart_devices(),
    State1 = update_all(Ds, State),
    {noreply, State1};
handle_cast({update_id,Name}, State) ->
    IDs = list_serial_devices(),
    ?debug("Lookup ~s in ~p\n", [Name, IDs]),
    case lists:keyfind(Name, 1, IDs) of
	false -> false;  %% not a serial device
	{_,ID} ->
	    case lists:keytake(Name,#uart_device.name,State#state.devices) of
		false ->
		    {noreply, State};
		{value,D,Ds} ->
		    D1 = D#uart_device { id = ID },
		    {noreply, State#state{ devices=[D1|Ds]}}
	    end
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_F={fevent,Ref,[create],Path,Name}, State) 
  when Ref =:= State#state.dref ->
    ?debug("fevent: ~p\n", [_F]),
    case is_uart_device(Path,Name) of
	true ->
	    Ds0 = State#state.devices,
	    case lists:keyfind(Name,#uart_device.name, Ds0) of
		false ->
		    case lookup_device_id(Name) of
			false ->
			    {noreply,State};
			ID ->
			    D = #uart_device { name = Name, path = Path, 
					       id   = ID, avail=true },
			    State1 = add(D, State),
			    {noreply, State1}
		    end;
		_D ->
		    {noreply, State}
	    end;
	false ->
	    case os:type() of
		{unix,linux} when Name =:= "serial" ->
		    %% postpone scanning for /dev/serial/by-ids
		    %% until the directory is create
		    cast_when_created("/dev/serial", "by-id",
				      scan_serial, 5000),
		    {noreply, State};
		_ ->
		    {noreply, State}
	    end
    end;
handle_info(_F={fevent,Ref,[delete],_Path,Name}, State) 
  when Ref =:= State#state.dref ->
    ?debug("fevent: ~p\n", [_F]),
    State1 = removed_by_name(Name, State),
    {noreply, State1};

handle_info({'DOWN',Ref,process,Pid,_Reason}, State) ->
    case lists:keytake(Ref, #uart_device.mon, State#state.devices) of
	false ->
	    case lists:keytake(Ref,2,State#state.sub) of
		{value,{Pid,Ref},Sub} ->
		    {noreply,State#state { sub = Sub }};
		_ ->
		    {noreply,State}
	    end;
	{value,D,Ds} ->
	    D1 = D#uart_device { avail = true,
				 owner = undefined,
				 mon   = undefined },
	    ?debug("AUTO-RELEASED: ~p\n", [D1]),
	    Ds1 = [D1 | Ds],
	    {noreply,  State#state { devices = Ds1 }}
    end;

handle_info(_Info, State) ->
    ?debug("Info: ~p\n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    remove_all(State#state.devices, State),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

list_uart_devices() ->
    case os:type() of
	{unix,darwin} ->
	    Path = "/dev",
	    {ok,Ds} = file:list_dir(Path),
	    Ds1 = lists:filter(fun(D) -> is_uart_device(Path,D) end, Ds),
	    map(fun(Name) ->
			#uart_device { name=Name, 
				       path=Path, 
				       id=Name,
				       avail=true}
		end, Ds1);
	{unix,linux} ->
	    case filelib:is_dir("/dev/serial") of
		false -> 
		    [];
		true ->
		    ?debug("/dev/serial exist\n",[]),
		    IDs = list_serial_devices(),
		    map(fun({Name,ID}) ->
				#uart_device { name=Name, 
					       path="/dev",
					       id = ID,
					       avail=true}
			end, IDs)
	    end;
	_ ->
	    []
    end.

%% list serial devices on linux - return [{Name,ID}]
list_serial_devices() ->
    Path = "/dev/serial/by-id",
    case file:list_dir(Path) of 
	{ok,Fs} ->
	    ?debug("/dev/serial/by-ids = ~p\n",[Fs]),
	    lists:foldl(
	      fun(ID,Acc) ->
		      Link = filename:join(Path,ID),
		      case file:read_link(Link) of
			  {ok,Name} ->
			      Name1=filename:basename(Name),
			      ?debug("ID=~s, Name=~s\n", [ID,Name1]),
			      [{Name1,ID}|Acc];
			  _ ->
			      Acc
		      end
	      end, [], Fs);
	_E={error,enoent} ->
	    ?debug("/dev/serial/by-ids = ~p\n",[_E]),
	    []
    end.
	

is_uart_device(Path,Name) ->  %% fixme check fileinfo
    case os:type() of
	{unix,Type} ->
	    Prefix = if  Type =:= darwin -> "tty.";
			 true -> "tty"
		     end,
	    case lists:prefix(Prefix, Name) of  
		true ->
		    %% mac os x names are /dev/tty.*
		    %% linux /dev/tty*
		    case file:read_file_info(filename:join(Path,Name)) of
			{ok,I} when I#file_info.type =:= device ->
			    true;
			{ok,_} -> false;
			{error,_} -> false
		    end;
		false ->  %% reconsider this
		    false
	    end;
	{win32,_} ->
	    %% win32 \\.\COM* | COM[0-9]
	    case Name of
		"\\\\.\\COM"++_ -> true;
		"COM"++[_] -> true;
		_ -> false
	    end
    end.

watch_uart_devices() ->
    {ok,DRef} = fnotify:watch("/dev"),
    DRef.

unwatch(undefined) ->
    ok;
unwatch(Ref) ->
    fnotify:unwatch(Ref).

subscribe_all_([Pid|Ps], State) ->
    {_Ref,State1} = subscribe_(Pid, State),
    subscribe_all_(Ps, State1);
subscribe_all_([], State) ->
    State.

subscribe_(Pid, State) ->
    Ref = erlang:monitor(process,Pid),
    Sub = [{Pid,Ref} | State#state.sub],
    {Ref,State#state { sub = Sub }}.

unsubscribe_(Ref,State) ->
    case lists:keytake(Ref,2,State#state.sub) of
	{value,{_Pid,Ref},Subs} ->
	    erlang:demonitor(Ref, [flush]),
	    State#state { sub = Subs };
	_ ->
	    State
    end.

lookup_device_id(Name) ->
    case os:type() of 
	{unix,linux} ->
	    %% when a devices is created in /dev we "need"/want to 
	    %% find that device under /dev/serial/by-id to be sure
	    %% the device is a serial device.
	    cast_after(1000, {update_id,Name}),
	    Name;
	_ ->
	    Name
    end.

%%
%% add or update all devices from device list Ds
%%	    
update_all(Ds, State) ->
    lists:foldl(fun update/2, State, Ds).

update(D, State) ->
    Ds = State#state.devices,
    case lists:keytake(D#uart_device.name, #uart_device.name, Ds) of
	false ->
	    add(D,State);
	{value,D1,Ds1} ->
	    %% info may need update, more?
	    D2 = D1#uart_device { id=D#uart_device.id },
	    ?debug("UPDATE: ~p\n", [D]),
	    State#state { devices = [D2|Ds1] }
    end.


%% device was added (USB pugged in)
add(D,State) ->
    ?debug("ADD: ~p\n", [D]),
    lists:foreach(fun({Pid,Ref}) -> 
			  Pid ! {uart_device,Ref,[added],D#uart_device.name}
		  end, State#state.sub),
    Ds = State#state.devices,
    State#state { devices = [D|Ds]}.


%% device is removed (USB device unplugged)
removed_by_name(Name, State) ->
    case lists:keytake(Name, #uart_device.name, State#state.devices) of
	false ->
	    State;
	{value,D,Ds} ->
	    remove(D,Ds,State)
    end.

removed_by_id(ID, State) ->
    case lists:keytake(ID, #uart_device.id, State#state.devices) of
	false ->
	    State;
	{value,D,Ds} ->
	    remove(D,Ds,State)
    end.

remove_all([D|Ds],State) ->
    State1 = remove(D,Ds,State),
    remove_all(Ds, State1);
remove_all([],State) ->
    State.

remove(D,Ds,State) ->
    ?debug("REMOVED: ~p\n", [D]),
    if is_pid(D#uart_device.owner) ->
	    %% maybe flag this ?
	    D#uart_device.owner ! {uart_device,self(),[removed],
				   D#uart_device.name};
       true ->
	    ok
    end,
    lists:foreach(fun({Pid,Ref}) -> 
			  Pid ! {uart_device,Ref,[removed],D#uart_device.name}
		  end, State#state.sub),
    if is_reference(D#uart_device.mon) ->
	    erlang:demonitor(D#uart_device.mon,[flush]),
	    ok;
       true ->
	    ok
    end,
    State#state { devices = Ds }.

cast_after(Timeout, Msg) ->
    Server = self(),
    spawn(
      fun() ->
	      timer:sleep(Timeout),
	      gen_server:cast(Server, Msg)
      end).

cast_when_created(Path, Name, Msg, Timeout) ->
    Server = self(),
    spawn(
      fun() ->
	      {ok,FRef} = fnotify:watch(Path, [create]),
	      receive 
		  {fevent,FRef,[create],Path,Name} ->
		      ?debug("cast_when_created: cast(~p)\n", [Msg]),
		      fnotify:unwatch(FRef),
		      timer:sleep(1000), %% revert to sleep for a while ...
		      gen_server:cast(Server, Msg)
	      after Timeout ->
		      ?debug("cast_when_created: timeout\n", []),
		      fnotify:unwatch(FRef),
		      {error,timeout}
	      end
      end).
