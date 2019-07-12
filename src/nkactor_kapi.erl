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
%% When a request comes from nkrest, you call http_request/4
%% - The url will be parsed and a nkactor_request:request() will be generated
%% - nkactor_request:request/1 will be called
%% - actor_pre_request/3 will be called,



%% @doc
-module(nkactor_kapi).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([http_request/4, request/1, search/1]).
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
%% It will process the request and call request/1

-spec http_request(nkserver:id(), http_method(), http_path(), http_req()) ->
    http_reply() |
    {redirect, Path::binary(), http_req()} |
    {cowboy_static, cowboy_static:opts()} |
    {cowboy_rest, Callback::module(), State::term()}.

http_request(SrvId, Method, Path, Req) ->
    nkactor_kapi_http:request(SrvId, Method, Path, Req).



%% @doc Process an actor API request, but after adapting the actor
%% calling callback actor_api_pre_search/2, and then
%% actor_api_post_request/2 to adapt the response to API's format
-spec request(nkactor:request()) ->
    nkactor_request:reply().

request(Req) ->
    Req2 = Req#{class=>nkactor_kapi},
    case maps:get(verb, Req2, get) of
        list ->
            Req3 = nkactor_kapi_parse:search_params(Req2),
            case nkactor_request:request(Req3) of
                {ok, Data, Req4} ->
                    Data2 = nkactor_kapi_unparse:list_to_api_list(Data, Req4),
                    {ok, Data2, Req4};
                Other ->
                    Other
            end;
        Verb when Verb==create; Verb==update ->
            % Extract request fields from body, checks for inconsistency
            % and changes body's format
            case nkactor_kapi_parse:pre_request(Req2) of
                {ok, Req3} ->
                    % actor_to_external will be called if necessary
                    nkactor_request:request(Req3);
                {error, Error, Req3} ->
                    {error, Error, Req3}
            end;
        _ ->
            nkactor_request:request(Req2)
    end.


%% @doc Process an actor API request, but after adapting the actor
%% calling callback actor_api_pre_search/2, and then
%% actor_api_post_request/2 to adapt the response to API's format
-spec search(nkactor:request()) ->
    nkactor_request:reply().

search(#{srv:=SrvId, verb:=Verb}=Req) when Verb==list; Verb==deletecollection ->
    Req2 = Req#{class => nkactor_search},
    case nkactor_kapi_parse:search_opts(Req2) of
        {ok, Opts} ->
            case nkactor_request:pre_request(Req2) of
                {ok, Req3} ->
                    Spec1 = maps:get(body, Req2, #{}),
                    Spec2 = maps:without([apiVersion, <<"apiVersion">>, kind, <<"kind">>], Spec1),
                    Reply = case Verb of
                        list ->
                            case nkactor:search_actors(SrvId, Spec2, Opts) of
                                {ok, ActorList, Meta} ->
                                    ActorList2 = [nkactor_actor:to_external(Actor, Req) || Actor <- ActorList],
                                    {ok, Meta#{items=>ActorList2}, Req3};
                                {error, Error} ->
                                    {error, Error, Req3}
                            end;
                        deletecollection ->
                            case nkactor:delete_multi(SrvId, Spec2, Opts) of
                                {ok, Meta} ->
                                    {ok, Meta, Req3};
                                {error, Error} ->
                                    {error, Error, Req3}
                            end
                    end,
                    nkactor_request:post_request(Reply, Req2);
                {error, Error, Req3} ->
                    {error, Error, Req3}
            end;
        {error, Error} ->
            {error, Error, Req2}
    end;

search(Req) ->
    {error, verb_not_allowed, Req}.



