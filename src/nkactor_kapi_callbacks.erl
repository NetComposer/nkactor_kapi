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

%% @doc Default plugin callbacks
-module(nkactor_kapi_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([status/1]).
-export([api_get_groups/2, api_get_paths/2]).
-export([actor_id/2, actor_config/1]).
-export([actor_from_external/5, actor_to_external/5]).

-include("nkactor_kapi.hrl").
-include_lib("nkactor/include/nkactor.hrl").

%% ===================================================================
%% Status & Msgs  Callbacks
%% ===================================================================

%% @private

status(_) -> continue.



%% ===================================================================
%% API Callbacks
%% ===================================================================


%% @doc Called when the list of base paths of the server is requested
-spec api_get_paths(nkservice:id(), [binary()]) ->
    [binary()].

api_get_paths(SrvId, Acc) ->
    [
        <<"/apis">>,
        <<"/apis-ws">>,
        <<"/openapi">>,
        <<"/graphql">>,
        get_actor_api_paths(SrvId)
        |
        Acc
    ].


%% @doc Called to add info about all supported APIs
%% Must be implemented by new actor APIs
-spec api_get_groups(nkserver:id(), #{nkactor_kapi:group() => [nkactor_kapi:vsn()]}) ->
    {continue, #{nkactor_kapi:group() => [nkactor_kapi:vsn()]}}.

api_get_groups(_SrvId, GroupsAcc) ->
    GroupsAcc.


%% @doc
actor_config(#{fields_trans:=FieldTrans}=Config) ->
    FieldTrans2 = FieldTrans#{<<"metadata.resourceVersion">> => <<"metadata.hash">>},
    {continue, [Config#{fields_trans:=FieldTrans2}]}.


%% @doc
actor_id(_SrvId, [<<"apis">>, Group, _Vsn, <<"namespaces">>, Namespace, Resource, Name]) ->
    #actor_id{
        group = Group,
        resource = Resource,
        namespace = Namespace,
        name = Name
    };

actor_id(SrvId, [<<"apis">>, Group, _Vsn, Resource, Name]) ->
    #actor_id{
        group = Group,
        resource = Resource,
        namespace = nkactor:base_namespace(SrvId),
        name = Name
    };

actor_id(_SrvId, _Parts) ->
    continue.


%% @doc API modules must implement this to update fields from external version
%% Core and metadata fields have already being updated from nkactor_kapi:request/1
-spec actor_from_external(nkactor_request:class(), nkactor:group(), nkactor:resource(),
                          nkactor:actor(), nkactor_request:request()) ->
    {ok, nkactor:actor()} | {error, term()}.

actor_from_external(nkactor_kapi, Group, Res, Actor, Req) ->
    Data1 = maps:get(data, Actor, #{}),
    Data2 = case maps:find(<<"spec">>, Actor) of
        {ok, DataSpec1} ->
            Data1#{<<"spec">> => DataSpec1};
        error ->
            Data1
    end,
    Data3 = case maps:find(spec, Actor) of
        {ok, DataSpec2} ->
            Data2#{<<"spec">> => DataSpec2};
        error ->
            Data2
    end,
    Data4 = case maps:find(<<"data">>, Actor) of
        {ok, DataData1} ->
            Data3#{<<"data">> => DataData1};
        error ->
            Data3
    end,
    Data5 = case maps:find(data, Actor) of
        {ok, DataData2} ->
            Data4#{<<"data">> => DataData2};
        error ->
            Data4
    end,
    Actor2 = maps:without([<<"spec">>, spec, <<"data">>, data], Actor),
    Actor3 = Actor2#{data => Data5},
    {continue, [nkactor_kapi, Group, Res, Actor3, Req]};

actor_from_external(_Class, _Group, _Res, _ApiActor, _Req) ->
    continue.



%% @doc API modules must implement this take the chance to modify the resulting actor
%% Called from nkactor_kapi:unparse/2
-spec actor_to_external(nkactor_request:class(), nkactor:group(), nkactor:resource(),
                        nkactor:actor(), nkactor_request:request()) ->
    map().

actor_to_external(nkactor_kapi, _Group, _Res, Actor, Req) ->
    nkactor_kapi_unparse:actor_to_api_actor(Actor, Req);

actor_to_external(nkactor_search, _Group, _Res, Actor, Req) ->
    nkactor_kapi_unparse:actor_to_api_actor(Actor, Req);

actor_to_external(_Class, _Group, _Res, _Actor, _Req) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
get_actor_api_paths(SrvId) ->
    lists:foldl(
        fun(Map, Acc) ->
            case Map of
                #{<<"versions">>:=Versions} ->
                    GV1 = [<<"/apis/", V/binary>> || #{<<"groupVersion">>:=V} <- Versions],
                    GV2 = [nkactor_kapi_lib:remove_vsn(G) || G <-GV1],
                    Acc++GV1++GV2;
                _ ->
                    Acc
            end
        end,
        [],
        nkactor_kapi_lib:get_groups(SrvId)).

