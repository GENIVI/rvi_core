%%
%% Copyright (C) 2014, Jaguar Land Rover
%%
%% This program is licensed under the terms and conditions of the
%% Mozilla Public License, version 2.0.  The full text of the 
%% Mozilla Public License is at https://www.mozilla.org/MPL/2.0/
%%

-module(rvi_schedule).

-callback schedule_message(SvcName :: string(), 
			   Timeout :: integer(),
			   Callback :: mfa(), 
			   Parameters :: any(), 
			   Signature :: string(),
			   Certificate :: string()) -> Result::tuple().


-callback register_remote_service(NetworkAddress :: string(), 
				  AvailableServices :: string()) -> Result::tuple().

-callback unregister_remote_service(ServiceNames :: string()) -> Result::tuple().


