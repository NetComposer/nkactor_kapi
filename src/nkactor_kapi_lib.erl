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

%% @doc
-module(nkactor_kapi_lib).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([get_group_list/1, get_groups/1, get_resources_list/2]).
-export([get_modules/3, get_resources/2]).
-export([get_group_vsn/1, api_path_to_actor_path/1]).
-export([remove_vsn/1, expand_api_links/1]).
-export([status/2, status/3, error/2, error/3]).

-include_lib("nkserver/include/nkserver.hrl").


%% ===================================================================
%% Utilities
%% ===================================================================


%% @doc
get_group_list(ActorSrvId) ->
    #{
        <<"kind">> => <<"APIGroupList">>,
        <<"apiVersion">> => <<"v1">>,
        <<"groups">> => get_groups(ActorSrvId)
    }.


%% @doc
get_groups(ActorSrvId) ->
    Groups = ?CALL_SRV(ActorSrvId, api_get_groups, [ActorSrvId, #{}]),
    lists:map(
        fun({Name, [Pref|_]=Versions}) ->
            #{
                <<"name">> => Name,
                <<"versions">> => lists:map(
                    fun(Vsn) ->
                        #{
                            <<"groupVersion">> => <<Name/binary, $/, Vsn/binary>>,
                            <<"version">> => Vsn
                        }
                    end,
                    Versions),
                <<"preferredVersion">> => #{
                    <<"groupVersion">> => <<Name/binary, $/, Pref/binary>>,
                    <<"version">> => Pref
                }
            }
        end,
        maps:to_list(Groups)).


%% @doc
-spec get_modules(nkserver:id(), nkactor_kapi:group(), nkactor_kapi:vsn()) ->
    {ok, [module()]} | {error, term()}.

get_modules(SrvId, Group, _Vsn) ->
    #{modules:=AllModules} = nkserver:get_config(SrvId),
    Modules = lists:filter(
        fun(Module) ->
            % TODO: Check if module is available in API "Vsn"
            _C = nkactor_actor:get_config(SrvId, Module),
            true
        end,
        maps:get(Group, AllModules, [])
    ),
    {ok, Modules}.


%% @private
get_resources(SrvId, Modules) ->
    lists:map(
        fun(Module) ->
            Config = nkactor_actor:get_config(SrvId, Module),
            #{
                resource := Resource,
                singular := Singular,
                camel := Kind,
                verbs := Verbs,
                short_names := ShortNames
            } = Config,
            #{
                <<"name">> => Resource,
                <<"singularName">> => Singular,
                <<"kind">> => Kind,
                <<"verbs">> => Verbs,
                <<"shortNames">> => ShortNames
            }
        end,
        Modules).

%% @doc
get_resources_list(API, Resources) ->
    #{
        <<"kind">> => <<"APIResourceList">>,
        <<"groupVersion">> => API,
        <<"resources">> => Resources
    }.


%% @doc Reports an standardized success
% https://github.com/kubernetes/community/blob/master/contributors/devel/api-conventions.md#response-status-kind
-spec status(nkserver:id(), nkserver:status()) ->
    map().

status(_, #{<<"kind">> := <<"Status">>, <<"code">> := _} = Status) ->
    Status;

status(SrvId, Msg) ->
    status(SrvId, Msg, #{}).


%% @doc
-spec status(nkserver:id(), nkserver:status(), map()) ->
    map().

status(SrvId, UserStatus, Meta) ->
    ExtStatus = nkserver_status:status(SrvId, UserStatus),
    #{
        <<"apiVersion">> => <<"v1">>,
        <<"kind">> => <<"Status">>,
        <<"metadata">> => Meta,
        <<"status">> => <<"Success">>,
        <<"message">> => maps:get(info, ExtStatus, <<>>),
        <<"reason">> => maps:get(status, ExtStatus),
        <<"details">> => maps:get(data, ExtStatus, #{}),
        <<"code">> => maps:get(code, ExtStatus, 500)
    }.


%% @doc Reports an standardized error
-spec error(nkserver:id(), nkserver:status()) ->
    map().

error(_, #{<<"kind">> := <<"Status">>, <<"code">> := _} = Status) ->
    Status;

error(SrvId, Msg) ->
    error(SrvId, Msg, #{}).


-spec error(nkserver:id(), nkserver:status(), map()) ->
    map().

error(SrvId, UserStatus, Meta) ->
    ExtStatus = nkserver_status:status(SrvId, UserStatus),
    #{
        <<"apiVersion">> => <<"v1">>,
        <<"kind">> => <<"Status">>,
        <<"metadata">> => Meta,
        <<"status">> => <<"Faillure">>,
        <<"message">> => maps:get(info, ExtStatus, <<>>),
        <<"reason">> => maps:get(status, ExtStatus),
        <<"details">> => maps:get(data, ExtStatus, #{}),
        <<"code">> => maps:get(code, ExtStatus, 500)
    }.


%%%% @doc
%%add_common_fields(SrvId, Actor) ->
%%    #actor{id=#actor_id{domain=Namespace, group=Group, resource=Res}} = Actor,
%%    {ok, #{camel:=Kind}} = nkdomain_actor_util:get_config(SrvId, Group, Res),
%%    add_common_fields(Actor, Namespace, Kind).
%%
%%
%%%% @doc
%%add_common_fields(Actor, Namespace, Kind) ->
%%    #actor{data=Data, metadata=Meta} = Actor,
%%    Data2 = Data#{<<"kind">> => Kind},
%%    Meta2 = Meta#{<<"domain">> => Namespace},
%%    Actor#actor{data=Data2, metadata=Meta2}.



%% @doc Generates an actor path (/domain/group/resource/name) from /apis...
-spec api_path_to_actor_path(term()) ->
    binary().

api_path_to_actor_path(Id) ->
    case binary:split(to_bin(Id), <<"/">>, [global]) of
        [<<>>, <<"apis">>, Group, _Vsn, <<"namespaces">>, Namespace, Resource, Name] ->
            api_to_actor_path(Namespace, Group, Resource, Name);
        [<<>>, <<"apis">>, Group, _Vsn, Resource, Name] ->
            api_to_actor_path(<<>>, Group, Resource, Name);
        _ ->
            % Must be a UID or actor path
            Id
    end.


%% @private Generates an actor path (/domain/group/resource/name) from api parts
api_to_actor_path(Namespace, Group, Resource, Name) ->
    list_to_binary([$/, Namespace, $/, Group, $/, Resource, $/, Name]).


%% @doc
get_group_vsn(ApiVsn) ->
    case binary:split(ApiVsn, <<"/">>) of
        [Group, Vsn] ->
            {Group, Vsn};
        [Group] ->
            {Group, <<>>};
        _ ->
            error
    end.


%%%% @doc Called to generate the API Group for an object
%%-spec get_api_group(nkservice:id(), nkdomain_api:group()|undefined,
%%    nkservice_actor:actor()) ->
%%    Group::binary().
%%
%%get_api_group(SrvId, Group, Actor) ->
%%    to_bin(?CALL_SRV(SrvId, nkdomain_actor_to_api_group, [SrvId, Group, Actor])).


%% @private
remove_vsn(Group) ->
    [_ | Rest] = lists:reverse(binary:split(Group, <<"/">>, [global])),
    nklib_util:bjoin(lists:reverse(Rest), $/).


%% @private Change links format from /api/... to /domain/...
expand_api_links(#{metadata:=Meta}=Actor) ->
    Links1 = maps:get(links, Meta, #{}),
    Links2 = expand_api_links(maps:to_list(Links1), #{}),
    Meta2 = Meta#{links => Links2},
    Actor#{metadata := Meta2}.


%% @private
expand_api_links([], Acc) ->
    Acc;

expand_api_links([{Id, Type}|Rest], Acc) ->
    Path = nkactor_kapi_lib:api_path_to_actor_path(Id),
    expand_api_links(Rest, Acc#{Path => Type}).


%%%% @doc Parses an API and makes sure group and vsn are available,
%%%% from api itself or body's apiVersion
%%-spec parse_api_request(map()) ->
%%    {ok, nkdomain_api:request()} | {error, nkservice:msg()}.
%%
%%parse_api_request(Api) ->
%%    case nklib_syntax:parse(Api, api_syntax()) of
%%        {ok, #{group:=_, vsn:=_}=Api2, _} ->
%%            {ok, Api2};
%%        {ok, #{group:=_}, _} ->
%%            {error, {field_missing, <<"vsn">>}};
%%        {ok, #{vsn:=_}, _} ->
%%            {error, {field_missing, <<"group">>}};
%%        {ok, Api2, _} ->
%%            Body = maps:get(body, Api2, #{}),
%%            case
%%                is_map(Body) andalso
%%                nklib_syntax:parse(Body, #{apiVersion=>binary})
%%            of
%%                {ok, #{apiVersion:=ApiVsn}, _} ->
%%                    case get_group_vsn(ApiVsn) of
%%                        {Group, Vsn} ->
%%                            {ok, Api2#{group=>Group, vsn=>Vsn}};
%%                        error ->
%%                            {error, {field_missing, <<"group">>}}
%%                    end;
%%                _ ->
%%                    {error, {field_missing, <<"group">>}}
%%            end;
%%        {error, Error} ->
%%            {error, Error}
%%    end.


%% @private
to_bin(Term) when is_binary(Term) -> Term;
to_bin(Term) -> nklib_util:to_binary(Term).