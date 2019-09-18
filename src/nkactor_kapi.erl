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

%% This plugins adds an external API to nkactor
%%
%% - When a request comes from nkrest, you call http_request/4, that calls request/1
%% - You can call request/1 directly
%%
%% Important callbacks
%% - actor_kapi_parse: return syntax to adapt data from kapi to actor
%% - actor_kapi_unparse: return syntax to adapt actor to kapi
%% - actor_kapi_pre_request: can preprocess and incoming petition
%% - actor_kapi_post_request: can modify a request result
%% - actor_kapi_fields_trans: use it to convert kapi fields to actor fields, and
%%   the opposite to report errors on fields


%% @doc
-module(nkactor_kapi).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([http_request/4, request/1, search/1]).
-export([get_fields_trans/1, get_fields_rev/1, rev_field/2]).
-export_type([api_vsn/0, kind/0]).

-include_lib("nkserver/include/nkserver.hrl").

%% ===================================================================
%% Types
%% ===================================================================


-type api_vsn() :: binary().
-type kind() :: binary().


-type http_method() :: nkrest_http:method().
-type http_path() :: nkrest_http:path().
-type http_req() :: nkrest_http:req().
-type http_reply() :: nkrest_http:http_reply().



%% ===================================================================
%% Public
%% ===================================================================


%% @doc Call this function (from nkrest) to process a new http request
%% Use ActorSrvId as framework for debug, API calls etc.
%% It will process the request and call request/1
-spec http_request(nkserver:id(), http_method(), http_path(), http_req()) ->
    http_reply() |
    {redirect, Path::binary(), http_req()} |
    {cowboy_static, cowboy_static:opts()} |
    {cowboy_rest, Callback::module(), State::term()}.

http_request(ActorSrvId, Method, Path, Req) ->
    nkactor_kapi_http:request(ActorSrvId, Method, Path, Req).


%% @doc Process an actor API request
%% - First it adapts body for create/update operations
%% - Calls standard nkactor_request:pre_request/1 and then actor_kapi_pre_request
%% - Calls standard nkactor_request:do_request/1
%% - Calls nkactor_request:post_request/1 and then actor_kapi_post_request
-spec request(nkactor:request()) ->
    {ok|created, map(), nkactor:request()} |
    {status, map(), nkactor:request()} |
    {error, map(), nkactor:request()} |
    {raw, {CT::binary(), Body::binary()}, nkactor:request()}.

request(Req) ->
    % We need to update body, in case it provides group, resource, etc.
    Reply = case nkactor_kapi_parse:req_actor(Req) of
        {ok, Req2} ->
            % Body is updated to standard format, but no data or metadata processing
            case nkactor_request:pre_request(Req2) of
                {ok, Req3} ->
                    #{
                        verb := Verb,
                        srv := SrvId,           % Associated to namespace
                        group := Group,
                        resource := Res,
                        subresource := SubRes
                    } = Req3,
                    PreArgs = [Verb, Group, Res, SubRes, Req3],
                    % For verb 'list', adapts params
                    % For standard requests, calls nkactor_kapi_parse:from_external/4
                    case ?CALL_SRV(SrvId, actor_kapi_pre_request, PreArgs) of
                        {ok, Req4} ->
                            Reply1 = nkactor_request:do_request(Req4),
                            Reply2 = nkactor_request:post_request(Reply1, Req4),
                            PostArgs = [Verb, Group, Res, SubRes, Reply2],
                            % Convert back data and metadata
                            ?CALL_SRV(SrvId, actor_kapi_post_request, PostArgs);
                        {error, Error, Req4} ->
                            {error, Error, Req4}
                    end;
                {error, Error, Req3} ->
                    {error, Error, Req3}
            end;
        {error, Error, Req2} ->
            {error, Error, Req2}
    end,
    reply(Reply).



%% @doc Process an actor API request, but after adapting the actor
%% calling callback actor_api_pre_search/2, and then
%% actor_api_post_request/2 to adapt the response to API's format
-spec search(nkactor:request()) ->
    {ok, map(), nkactor:request()} |
    {error, map(), nkactor:request()}.

search(#{srv:=SrvId, verb:=Verb}=Req) when Verb==list; Verb==deletecollection ->
    Reply = case nkactor_kapi_parse:search_opts(Req) of
        {ok, Opts} ->
            case nkactor_request:pre_request(Req) of
                {ok, Req2} ->
                    Spec1 = maps:get(body, Req2, #{}),
                    Spec2 = maps:without([apiVersion, <<"apiVersion">>, kind, <<"kind">>], Spec1),
                    ReqReply = case Verb of
                        list ->
                            case nkactor:search_actors(SrvId, Spec2, Opts) of
                                {ok, ActorList, Meta} ->
                                    ActorList2 = lists:map(
                                        fun(Actor) ->
                                            nkactor_kapi_unparse:to_external(SrvId, Actor)
                                        end,
                                        ActorList),
                                    reply({ok, Meta#{items=>ActorList2}, Req2});
                                {error, Error} ->
                                    reply({error, Error, Req2})
                            end;
                        deletecollection ->
                            case nkactor:delete_multi(SrvId, Spec2, Opts) of
                                {ok, Meta} ->
                                    {ok, Meta, Req2};
                                {error, Error} ->
                                    {error, Error, Req2}
                            end
                    end,
                    nkactor_request:post_request(ReqReply, Req2);
                {error, Error, Req2} ->
                    {error, Error, Req2}
            end;
        {error, Error} ->
            {error, Error, Req}
    end,
    reply(Reply);

search(Req) ->
    reply({error, verb_not_allowed, Req}).



%% ===================================================================
%% Internal
%% ===================================================================


%% @doc
get_fields_trans(SrvId) ->
    nklib_util:do_config_get({nkactor_kapi_fields_trans, SrvId}).

%% @doc
get_fields_rev(SrvId) ->
    nklib_util:do_config_get({nkactor_kapi_fields_rev, SrvId}).

%% @private
rev_field(SrvId, Field) ->
    case maps:get(Field, get_fields_rev(SrvId), Field) of
        <<"data.", Field2/binary>> ->
            lager:error("NKLOG F1 ~s -> ~s", [Field, Field2]),
            Field2;
        Field2 ->
            lager:error("NKLOG F2 ~s -> ~s", [Field, Field2]),
            Field2
    end.


%% @private
reply({ok, Data, Req}) ->
    {ok, Data, Req};

reply({created, Data, Req}) ->
    {created, Data, Req};

reply({status, Status, #{srv:=SrvId}=Req}) ->
    Status2 = nkactor_kapi_lib:status(SrvId, Status),
    {status, Status2, Req};

reply({error, {field_invalid, Field}, #{srv:=SrvId}=Req}) ->
    Status2 = nkactor_kapi_lib:error(SrvId, {field_invalid, rev_field(SrvId, Field)}),
    {error, Status2, Req};

reply({error, {field_missing, Field}, #{srv:=SrvId}=Req}) ->
    Status2 = nkactor_kapi_lib:error(SrvId, {field_missing, rev_field(SrvId, Field)}),
    {error, Status2, Req};

reply({error, {field_unknown, Field}, #{srv:=SrvId}=Req}) ->
    Status2 = nkactor_kapi_lib:error(SrvId, {field_unknown, rev_field(SrvId, Field)}),
    {error, Status2, Req};

reply({error, {updated_invalid_field, Field}, #{srv:=SrvId}=Req}) ->
    Status2 = nkactor_kapi_lib:error(SrvId, {updated_invalid_field, rev_field(SrvId, Field)}),
    {error, Status2, Req};

reply({error, Error, #{srv:=SrvId}=Req}) ->
    Status2 = nkactor_kapi_lib:error(SrvId, Error),
    {error, Status2, Req};

reply({raw, {CT, Bin}, Req}) ->
    {raw, {CT, Bin}, Req}.




