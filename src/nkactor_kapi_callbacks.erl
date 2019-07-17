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
-export([actor_id/2, actor_kapi_fields_trans/1]).
-export([actor_kapi_pre_request/5, actor_kapi_post_request/5]).
-export([actor_kapi_parse/2, actor_kapi_unparse/2]).

-include("nkactor_kapi.hrl").
-include_lib("nkactor/include/nkactor.hrl").
-include_lib("nkserver/include/nkserver.hrl").

%% ===================================================================
%% Status & Msgs  Callbacks
%% ===================================================================

%% @private

status(_) -> continue.



%% ===================================================================
%% Implemented API Callbacks
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


%% @doc Called by nkactor_kapi_plugin to generate trans for fields
-spec actor_kapi_fields_trans(#{atom() => atom()}) ->
    #{atom() => atom()}.

actor_kapi_fields_trans(Map) ->
    Map#{
        'apiVersion' => 'group',
        'kind' => 'metadata.kind',
        'metadata.creationTime' => 'metadata.creation_time',
        'metadata.expiresTime' => 'metadata.expires_time',
        'metadata.inAlarm' => 'metadata.in_alarm',
        'metadata.isActive' => 'metadata.is_active',
        'metadata.isEnabled' => 'metadata.is_enabled',
        'metadata.name' => 'name',
        'metadata.namespace' => 'namespace',
        'metadata.nextStatusTime' =>'metadata.next_status_time',
        'metadata.resourceVersion' => 'metadata.hash',
        'metadata.uid' => 'uid',
        'metadata.updateTime' => 'metadata.update_time'
    }.


%% @doc
actor_kapi_pre_request(list, _Group, _Res, <<>>, Req) ->
    {ok, nkactor_kapi_parse:search_params(Req)};

actor_kapi_pre_request(Verb, _Group, _Res, <<>>, #{srv:=SrvId, body:=Actor}=Req)
        when (Verb==create orelse Verb==update) andalso is_map(Actor) ->
    case nkactor_kapi_parse:from_external(SrvId, Actor) of
        {ok, Actor3} ->
            {ok, Req#{body:=Actor3}};
        {error, Error} ->
            {error, Error, Req}
    end;

actor_kapi_pre_request(_Verb, _Group, _Res, _SubRes, Req) ->
    {ok, Req}.


%% @doc
actor_kapi_post_request(list, _Group, _Res, <<>>, {ok, Data, #{srv:=SrvId}=Req}) ->
    #{items:=Items} = Data,
    Items2 = lists:map(
        fun(Actor) -> nkactor_kapi_unparse:to_external(SrvId, Actor) end,
        Items),
    Data2 = nkactor_kapi_unparse:list_to_api_list(Data#{items:=Items2}, Req),
    {ok, Data2, Req};

actor_kapi_post_request(_Verb, _Group, _Res, <<>>, {Status, Actor, #{srv:=SrvId}=Req})
        when Status==ok; Status==created ->
    Actor2 = nkactor_kapi_unparse:to_external(SrvId, Actor),
    {Status, Actor2, Req};

actor_kapi_post_request(_Verb, _Group, _Res, _SubRes, Reply) ->
    Reply.


%% @doc
actor_kapi_parse(_Group, _Resource) ->
    #{}.


%% @doc
actor_kapi_unparse(_Group, _Resource) ->
    #{}.






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

