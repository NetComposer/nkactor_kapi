%% -------------------------------------------------------------------
%%
%% Copyright (c) 2019 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc Default callbacks for plugin definitions
-module(nkactor_kapi_plugin).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([plugin_deps/0, plugin_config/3, plugin_cache/3, plugin_start/3]).

-include("nkactor_kapi.hrl").
-include_lib("nkserver/include/nkserver.hrl").

%% ===================================================================
%% Plugin Callbacks
%% ===================================================================


%% @doc 
plugin_deps() ->
	[nkactor].


%% @doc
plugin_config(_SrvId, Config, #{class:=nkactor}) ->
    Syntax = #{
        debug => boolean
    },
    nkserver_util:parse_config(Config, Syntax).


%% @doc
plugin_cache(_SrvId, Config, _Service) ->
    Cache = #{
        debug => maps:get(debug, Config, false)
    },
    {ok, Cache}.


%% @doc
plugin_start(SrvId, _Config, _Service) ->
    FieldTrans1 = ?CALL_SRV(SrvId, actor_kapi_fields_trans, [#{}]),
    FieldTrans2 = [
        {nklib_util:to_binary(K), nklib_util:to_binary(V)}
        || {K, V} <- maps:to_list(FieldTrans1)
    ],
    FieldTrans3 = maps:from_list(FieldTrans2),
    nklib_util:do_config_put({nkactor_kapi_fields_trans, SrvId}, FieldTrans3),
    RevTrans1 = [{V, K} || {K, V} <- FieldTrans2],
    RevTrans2 = maps:from_list(RevTrans1),
    nklib_util:do_config_put({nkactor_kapi_fields_rev, SrvId}, RevTrans2),
    ok.


%% ===================================================================
%% Internal
%% ===================================================================

