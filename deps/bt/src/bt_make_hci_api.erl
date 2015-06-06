%%% @author Tony Rogvall <tony@up13>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Create header/erlang file from def file
%%% @end
%%% Created :  3 Apr 2015 by Tony Rogvall <tony@up13>

-module(bt_make_hci_api).

-compile(export_all).

%%
%% the def file content consist of:
%%
%% {define, Name::atom(), Value::integer() | string()}
%%
%% <NAME>_SIZE are really not needed since the size may be 
%% computed from the struct?
%% OGF_<Group?-Name>
%% OCF_<Command-Name>
%% 
%% A command OCG_<COMMAND-NAME> is also in company with
%% command structure: <command_name>_cp or <command_name>_rp
%%
%% {type, name(), type()}   type definition
%% 
%% {enum, [name()]} | {enum, [{name(),value()}]}
%%
%% {struct,name(),[{type(),name()}]}
%% {struct,name(),[{type(),size(),name()}]}
%%
%% <name>_cp  is command struct
%% <name>_rp  is reply struct
%%
%% 
%%
%% Generate include file hrl
%% Generate source file erl
%%
main() ->
    {ok,Defs0} = file:consult(filename:join(code:priv_dir(bt), "hci.def")),
    {ok,Erl} = file:open("hci_api.erl", [write]),
    {ok,Hrl} = file:open("hci_api.hrl", [write]),
    write_erl_header(Erl),
    write_hrl_header(Hrl),
    R = try write(Hrl, Erl, Defs0, Defs0, "", "", [], [], []) of
	    Result -> Result
	catch
	    error:Reason ->
		io:format("crash: ~p\n", [erlang:get_stacktrace()]),
		{error,Reason}
	end,
    file:close(Erl),
    write_hrl_footer(Hrl),
    file:close(Hrl),
    R.

write_erl_header(Erl) ->
    io:format(Erl, "%% -*- erlang -*-\n", []),
    io:format(Erl, "-module(hci_api).\n", []),
    io:format(Erl, "-compile(export_all).\n", []),
    io:format(Erl, "-include(\"hci_api.hrl\").\n", []),
    io:format(Erl, "\n", []),
    io:format(Erl, "~s\n\n", ["
cname(<<0,_/binary>>) -> [];
cname(<<C,Cs/binary>>) -> [C|cname(Cs)];
cname(<<>>) -> [].
"]),
    ok.

write_hrl_header(Hrl) ->
    io:format(Hrl, "%% -*- erlang -*-\n", []),
    io:format(Hrl, "-ifndef(__HCI_HRL__).\n", []),
    io:format(Hrl, "-define(__HCI_HRL__,true).\n", []),
    io:format(Hrl, "\n\n", []),
    ok.

write_hrl_footer(Erl) ->
    io:format(Erl, "-endif.\n", []),
    ok.

%%
%% Generate erlang files
%%
write(Hrl,Erl,[Def|Defs],Defs0,OGF0,OCF0,EVTs,Funcs,Ds) ->
    case write_def(Hrl,Erl,Def,Defs0) of
	{ogf,OGF} -> write(Hrl,Erl,Defs,Defs0,OGF,OCF0,EVTs,Funcs,Ds);
	{ocf,OCF} -> write(Hrl,Erl,Defs,Defs0,OGF0,OCF,EVTs,
			   [{OGF0,OCF}|Funcs],Ds);
	{evt,EVT} -> write(Hrl,Erl,Defs,Defs0,OGF0,OCF0,[EVT|EVTs],Funcs,Ds);
	{evt_decode,Name} ->
	    D = [{Evt,Name} || Evt <- EVTs],
	    Ds1 = lists:reverse(D, Ds),
	    write(Hrl,Erl,Defs,Defs0,OGF0,OCF0,[],Funcs,Ds1);
	_ -> write(Hrl,Erl,Defs,Defs0,OGF0,OCF0,EVTs,Funcs,Ds)
    end;
write(_Hrl,Erl,[],Defs0,_OGF0,_OCF0,_EVTs,Funcs,Ds) ->
    %% do "normal" events
    io:format(Erl,
	     "decode(Evt,_Data) ->\n"
	     "  case Evt of\n", []),
    lists:foreach(
      fun({"EVT_TESTING", _Name}) -> ok;
	 ({"EVT_VENDOR", _Name})  -> ok;
	 ({"EVT_SI_"++_, _Name})  -> ok;
	 ({Evt="EVT_INQUIRY_COMPLETE", _Name}) ->
	      write_clause(Erl, Evt, inquiry_info);
	 ({Evt="EVT_INQUIRY_RESULT", _Name}) ->
	      write_clause(Erl, Evt, inquiry_info);
	 ({Evt="EVT_LE_META_EVENT",Name}) ->
	      write_clause(Erl, Evt, Name);
	 ({"EVT_LE"++_,_Name}) -> ok;
	 ({Evt,Name}) ->
	      write_clause(Erl, Evt, Name)
      end, Ds),
    io:format(Erl,
	      "    _ -> erlang:error(bad_event)\n", []),
    io:format(Erl,
	      "  end.\n\n", []),
    %% do "LE" events
    io:format(Erl,
	      "decode_le(Evt,_Data) ->\n"
	      "  case Evt of\n", []),
    lists:foreach(
      fun({"EVT_LE_META_EVENT",_Name}) -> ok;
	 ({Evt="EVT_LE"++_,Name}) ->
	      write_clause(Erl, Evt, Name);
	 ({_, _}) -> ok
      end, Ds),
    io:format(Erl, 
	      "    _ -> erlang:error(bad_event)\n", []),
    io:format(Erl,
	      "  end.\n\n", []),
    %% Do hci calls
    write_calls(Erl, Funcs, Defs0),
    ok.

write_clause(Erl, Evt, Name) ->
    io:format(Erl, "    ?~s -> decode_~s(_Data);\n",
	      [Evt,Name]).

%% Generate send function
write_send(Erl,OGF,OCF,Defs) ->
    "ocf_"++ FuncName = string:to_lower(atom_to_list(OCF)),
    Cp = list_to_atom(FuncName++"_cp"),
    case find_struct(Cp, Defs) of
	false ->
	    io:format(Erl, "send_~s(Socket) ->\n",
		      [FuncName]),
	    io:format(Erl, "  hci_socket:send(Socket,?~s,?~s,<<>>).\n\n",
		      [OGF,OCF]);
	{struct,Cp,Fields} ->
	    FieldNames = field_names(Fields),
	    MacroArgs = [ var_name(F) || F <- FieldNames ],
	    %% generate the argument structure 
	    io:format(Erl,
		      "send_~s(Socket,~s) ->\n",
		      [FuncName, join(MacroArgs,",")]),
	    io:format(Erl,
		      "  hci_socket:send(Socket,?~s,?~s,<<?~s_bin(~s)>>).\n\n",
		      [OGF,OCF,Cp,join(MacroArgs,",")])
    end.

%% For each {OGF, OCF} generate a send_<ocf>
%% and also generate a call_<ocf>
write_calls(Erl,[{OGF,OCF}|Funcs],Defs) ->
    write_call(Erl, OGF, OCF, Defs),
    write_calls(Erl,Funcs,Defs);
write_calls(_Erl,[],_Defs) ->
    ok.

%% Emit call function
write_call(Erl,OGF,OCF,Defs) ->
    "ocf_"++ FuncName = string:to_lower(atom_to_list(OCF)),
    Cp = list_to_atom(FuncName++"_cp"),
    Rp = list_to_atom(FuncName++"_rp"),
    Decoder = case find_struct(Rp, Defs) of
		  false -> "undefined";
		  {struct,Rp,_} -> "fun decode_"++FuncName++"_rp/1"
	      end,
    case find_struct(Cp, Defs) of
	false ->
	    io:format(Erl, "~s(Socket) ->\n",
		      [FuncName]),
	    io:format(Erl, "  hci_socket:call(Socket,?~s,?~s,<<>>,~s).\n\n",
		      [OGF,OCF,Decoder]);
	{struct,Cp,Fields} ->
	    FieldNames = field_names(Fields),
	    MacroArgs = [ var_name(F) || F <- FieldNames ],
	    %% generate the argument structure 
	    io:format(Erl,
		      "~s(Socket,~s) ->\n",
		      [FuncName, join(MacroArgs,",")]),
	    io:format(Erl,
		      "  hci_socket:call(Socket,?~s,?~s,<<?~s_bin(~s)>>,~s).\n\n",
		      [OGF,OCF,Cp,join(MacroArgs,","),Decoder])
    end.

%%
%% Write definition to header file.
%% Write decode functions to erlang file.
%%    
write_def(Hrl, Erl, Def, Defs0) ->
    io:format("Def = ~p\n", [Def]),
    case Def of
	{define,Name,Value} when is_integer(Value) ->
	    NameString = atom_to_list(Name),
	    io:format(Hrl, "-define(~s, ~w).\n", [NameString, Value]),
	    case NameString of
		"OGF_"++_ -> {ogf,Name};
		"OCF_"++_ -> {ocf,Name};
		"EVT_"++_ ->
		    case lists:suffix("_SIZE", NameString) of
			true -> ok;
			false ->  {evt,NameString}
		    end;
		_ -> ok
	    end;
	{define,Name,Value} when is_list(Value) ->
	    NameString = atom_to_list(Name),
	    io:format(Hrl, "-define(~s, ~s).\n", [NameString, Value]),
	    case NameString of
		"OGF_"++_ -> {ogf,Name};
		"OCF_"++_ -> {ocf,Name};
		"EVT_"++_ ->
		    case lists:suffix("_SIZE", NameString) of
			true -> ok;
			false ->  {evt,NameString}
		    end;
		_ -> ok
	    end;

	{type, _Name, _Type} -> %% used internally
	    ok;

	{enum,Es} ->
	    lists:foreach(
	      fun({Name,Value}) when is_integer(Value) ->
		      io:format(Hrl, "-define(~s, ~w).\n", [Name, Value])
	      end, enum_es(Es));

	{struct,Name,Fields} ->
	    NameString = atom_to_list(Name),
	    %% generate a record
	    io:format(Hrl, "-record(~s, {\n  ", [Name]),
	    FieldNames = field_names(Fields),
	    StrFieldNames = [atom_to_list(Nm) || Nm <- FieldNames],
	    io:format(Hrl, "~s", [join(StrFieldNames, ",\n  ")]),
	    io:format(Hrl, "\n}).\n", []),
	    %% generate a binary match sequence
	    MacroArgs = [ var_name(F) || F <- FieldNames ],
	    BinBody = field_match_list(Fields, Defs0),
	    io:format(Hrl, "-define(~s_bin(~s),~s).\n",
		      [Name, join(MacroArgs,","), 
		       join(BinBody,",")]),
	    case lists:reverse(NameString) of
		"pc_"++_ -> %% <name>_cp async command
		    ok;
		"pr_" ++ _ -> %% reply data structure
		    decode_function(Erl, Name, Fields, MacroArgs);
		"ofni_yriuqni" ->
		    decode_function(Erl, Name, Fields, MacroArgs);
		_ ->
		    case lists:prefix("evt_", NameString) of
			true ->
			    decode_function(Erl, Name, Fields, MacroArgs),
			    {evt_decode, Name};
			false ->
			    ok
		    end
	    end
    end.

decode_function(Erl, Name,  Fields, MacroArgs) ->
    FsList = format_field_assign_list(Fields, MacroArgs),
    %% generate a decode function
    io:format(Erl, "decode_~s(_Data) ->\n", [Name]),
    io:format(Erl,
	      "  case _Data of\n"
	      "    <<?~s_bin(~s)>> ->\n"
	      "      #~s { ~s }\n"
	      "  end.\n\n",
	      [Name,
	       join(MacroArgs,","),
	       Name,
	       FsList]).


format_field_assign_list([F], [Arg]) ->
    format_field_assign(F, Arg);
format_field_assign_list([F|Fs], [Arg|As]) ->
    [format_field_assign(F, Arg), ",", 
     format_field_assign_list(Fs,As)];
format_field_assign_list([], []) ->
    [].

format_field_assign({char,_,Name}, Arg) ->
    [atom_to_list(Name)," = cname(", Arg, ")"];
format_field_assign({_Type,Name}, Arg) ->
    [atom_to_list(Name)," = ", Arg];
format_field_assign({_Type,_,Name}, Arg) ->
    [atom_to_list(Name)," = ", Arg].
    

var_name(Name) when is_atom(Name) ->
    var_name_(atom_to_list(Name));
var_name(Name) when is_list(Name) ->
    var_name_(Name).

var_name_([H|T]) ->
    [string:to_upper(H) | T].

remove_suffix(Suffix, List) ->
    RSuffix = lists:reverse(Suffix),
    RList   = lists:reverse(List),
    case lists:prefix(RSuffix, RList) of
	true ->
	    lists:reverse(lists:nthtail(length(RSuffix), RList));
	false ->
	    List
    end.

find_define(Name,[Def={define,Name,_}|_Defs]) ->
    Def;
find_define(Name,[_|Defs]) ->
    find_define(Name,Defs);
find_define(_, []) ->
    false.

find_struct(Name,[Def={struct,Name,_}|_Defs]) ->
    Def;
find_struct(Name,[_|Defs]) ->
    find_struct(Name,Defs);
find_struct(_, []) ->
    false.

find_type(char,_)     -> true;
find_type(uint8_t,_)  -> true;
find_type(int8_t,_)   -> true;
find_type(uint16_t,_) -> true;
find_type(uint32_t,_) -> true;
find_type(uint64_t,_) -> true;
find_type(Name,[TypeDef={type,Name,_}|_Defs]) ->
    TypeDef;
find_type(Name,[_|Defs]) ->
    find_type(Name,Defs);
find_type(_, []) ->
    false.

field_match_list([{Type,Name}|Fs], Defs) ->
    [ field_match(Type,Name,Defs) | field_match_list(Fs,Defs)];
field_match_list([], _Defs) ->
    [].

field_match(Type,Name,Defs) when is_atom(Type) ->
    case find_type(Type,Defs) of
	true -> %% basic type
	    var_name(Name)++type_unit(Type);
	{type,_,Type1} ->
	    field_match(Type1,Name,Defs)
    end;
field_match({Type,Size},Name,Defs) when is_atom(Type) ->
    case find_type(Type,Defs) of
	true ->
	    var_name(Name)++":"++
		if is_integer(Size) ->
			integer_to_list(Size);
		   is_list(Size) ->
			"("++Size++")"
		end ++ "/" ++ unit(Type)++"-binary";
	{type,_,Type1} ->
	    field_match({Type1,Size}, Name, Defs)
    end.
%% more cases are neede here at some point

type_unit(char)     -> ":1/unsigned-unit:8";
type_unit(uint8_t)  -> ":1/unsigned-unit:8";
type_unit(int8_t)   -> ":1/signed-unit:8";
type_unit(uint16_t) -> ":1/little-unsigned-unit:16";
type_unit(uint32_t) -> ":1/little-unsigned-unit:32";
type_unit(uint64_t) -> ":1/little-unsigned-unit:64".

%% element unit
unit(char)     -> "unit:8";
unit(uint8_t)  -> "unit:8";
unit(int8_t)   -> "unit:8";
unit(uint16_t) -> "unit:16";
unit(uint32_t) -> "unit:32";
unit(uint64_t) -> "unit:64".

    
field_names(Fs) ->
    [ Name || {_Type, Name} <- Fs].

join([], _Sep) -> [];
join([A], _Sep) -> [A];
join([A|As], Sep) -> [A,Sep|join(As,Sep)].


enum_es(Enums) ->
    enum_es(Enums, 0).

enum_es([Name|Enums], I) when is_atom(Name) ->
    [{Name,I} | enum_es(Enums, I+1)];
enum_es([{Name,Value}|Enums], I) ->
    [{Name,Value}|enum_es(Enums, erlang:max(Value,I)+1)];
enum_es([], _) ->
    [].
