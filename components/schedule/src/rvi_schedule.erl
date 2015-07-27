%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%

-module(rvi_schedule).

-include_lib("rvi_common/include/rvi_common.hrl").

-callback schedule_message(CompSpec :: #component_spec{}, 
			   SvcName :: string(), 
			   Timeout :: integer(),
			   Parameters :: any(), 
			   Signature :: binary()) -> Result::tuple().

-callback register_remote_services(CompSpec :: #component_spec{}, 
				   NetworkAddress :: string(), 
				   AvailableServices :: string()) -> Result::tuple().

-callback unregister_remote_services(CompSpec :: #component_spec{}, 
				     ServiceNames :: string()) -> Result::tuple().


