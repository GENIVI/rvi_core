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
%%%    EXO http definition
%%% @end
%%% Created : 15 Dec 2011 by Tony Rogvall <tony@rogvall.se>

-ifndef(_EXO_HTTP_HRL_).
-define(_EXO_HTTP_HRL_, true).

-include("exo_url.hrl").

-ifndef(CRNL).
-define(CRNL, "\r\n").
-endif.

-ifndef(NL).
-define(NL,   "\n").
-endif.

-ifndef(CR).
-define(CR,   "\r").
-endif.

-record(http_request, 
	{
	  method,    %% 'GET' 'POST' ...
	  uri,       %% #url | string path
	  version,   %% version {Major,Minor}
	  headers    %% #http_chdr
	 }).

-record(http_response,
	{
	  version,   %% version {Major,Minor}
	  status,    %% status
	  phrase,    %% status text
	  headers    %% #http_shdr
	 }).

%% client headers
-record(http_chdr, 
	{
	  host,
	  connection,
	  transfer_encoding,
	  accept,
	  if_modified_since,
	  if_match,
	  if_none_match,
	  if_range,
	  if_unmodified_since,
	  range,
	  referer,
	  user_agent,
	  accept_ranges,
	  cookie = [],
	  keep_alive,
	  content_length,
	  content_type,
	  authorization,
	  other = []   %% misc other headers
	 }).

%% server headers
-record(http_shdr, 
	{
	  connection,
	  transfer_encoding,
	  location,
	  set_cookie,
	  content_length,
	  content_type,
	  other = []   %% misc other headers
	 }).


-endif.

