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
-module(nkactor_kapi_unparse).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([to_external/2, list_to_api_list/2]).

-include_lib("nkserver/include/nkserver.hrl").


%% ===================================================================
%% Utilities
%% ===================================================================




%% @doc
to_external(SrvId, #{group:=Group, resource:=Res}=Actor) ->
    Syntax = ?CALL_SRV(SrvId, actor_kapi_unparse, [Group, Res]),
    {ok, Actor2} = nklib_syntax:parse_all(Actor, Syntax),
    actor_to_api_actor(Actor2);

to_external(_SrvId, Other) ->
    Other.


%% @doc
actor_to_api_actor(Actor) ->
    case Actor of
        #{
            uid := UID,
            group := Group,
            resource := _,
            name := Name,
            namespace := Namespace,
            metadata := Metadata
        } ->
            {ok, ApiActor, _} = nklib_syntax:parse(Actor, actor_to_api_actor_syntax()),
            Data1 = maps:get(data, Actor, #{}),
            Data2 = [{to_bin(K), V} || {K, V} <- maps:to_list(Data1)],
            Data3 = maps:from_list(Data2),
            ApiActor2 = maps:merge(ApiActor, Data3),
            Kind = case Actor of
                #{metadata:=#{kind:=MetaKind}} ->
                    MetaKind;
                _ ->
                    ActorId = nkactor_lib:actor_to_actor_id(Actor),
                    {ok, _, #{camel:=Camel}} = nkactor_actor:get_config(ActorId),
                    Camel
            end,
            Meta1 = maps:get(<<"metadata">>, ApiActor, #{}),
            Meta2 = Meta1#{
                <<"uid">> => UID,
                <<"name">> => Name,
                <<"namespace">> => Namespace
            },
            Vsn = maps:get(vsn, Metadata, <<>>),
            ApiActor3 = ApiActor2#{
                <<"apiVersion">> => <<Group/binary, $/, Vsn/binary>>,
                <<"kind">> => Kind,
                <<"metadata">> => Meta2
            },
            ApiActor3;
        _ ->
            Actor
    end.


%% @private
actor_to_api_actor_syntax() ->
    #{
        metadata => {'__key', <<"metadata">>, #{
            vsn => ignore,
            kind => ignore,
            subtype => {'__key', <<"subtype">>},
            hash => {'__key', <<"resourceVersion">>},
            generation => {'__key', <<"generation">>},
            creation_time => {'__key', <<"creationTime">>},
            update_time => {'__key', <<"updateTime">>},
            expire_time => {'__key', <<"expireTime">>},
            labels => {'__key', <<"labels">>},
            fts => {'__key', <<"fts">>},
            links => {'__key', <<"links">>},
            annotations => {'__key', <<"annotations">>},
            is_enabled => {'__key', <<"isEnabled">>},
            in_alarm => {'__key', <<"inAlarm">>},
            alarms => {'__key', <<"alarms">>, {list, #{
                class => {'__key', <<"class">>},
                code => {'__key', <<"code">>},
                last_time => {'__key', <<"lastTime">>},
                message => {'__key', <<"message">>},
                meta => {'__key', <<"meta">>}
            }}},
            auto_activate => {'__key', <<"autoActivate">>},
            activate_time => {'__key', <<"activateTime">>},
            description => {'__key', <<"description">>},
            trace_id => {'__key', <<"trace_id">>}
        }}
    }.



%% @doc Generates a API actors list
list_to_api_list(List, Req) ->
    #{items:=Items, size:=Size} = List,
    #{group:=Group, resource:=Res, srv:=SrvId} = Req,
    Total = maps:get(total, List, undefined),
    Meta1 = #{<<"size">> => Size},
    Meta2 =  case Total of
        undefined ->
            Meta1;
        _ ->
            Meta1#{<<"total">> => Total}
    end,
    Module = nkactor_actor:get_module(SrvId, Group, Res),
    #{camel:=Camel} = nkactor_actor:get_config(SrvId, Module),
    % Standard request processing already calls to_external
    #{
        <<"apiVersion">> => <<"v1">>,
        <<"items">> => Items,
        <<"kind">> => <<Camel/binary, "List">>,
        <<"metadata">> => Meta2
        %% See https://kubernetes.io/docs/reference/api-concepts/
        %% <<"continue">>
        %% <<"resourceVersion">,
        %% <<"selfLink">>
    }.



%% @private
to_bin(Term) when is_binary(Term) -> Term;
to_bin(Term) -> nklib_util:to_binary(Term).