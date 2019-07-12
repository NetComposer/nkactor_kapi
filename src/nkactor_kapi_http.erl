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

%% @doc NkACTOR HTTP API processing
-module(nkactor_kapi_http).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([request/4]).
%-export([event_stream_start/1, event_stream_heartbeat/1, event_stream_stop/2, new_event/2]).
% -behavior(nkactor_kapi_watch).


-define(MAX_BODY_SIZE, 1000000).
-define(MAX_UPLOAD_SIZE, 100000000).

-include("nkactor_kapi.hrl").
-include_lib("nkserver/include/nkserver.hrl").

%% ===================================================================
%% REST API Invocation
%% ===================================================================



%%request(SrvId, Vsn, #{verb:=Verb}=ApiReq) ->
%%    lager:error("NKLOG CALL REQUEST"),
%%    case nkactor_request:request(ApiReq) of
%%        {ok, Data, ApiReq2} when Verb==get ->
%%            ApiActor = nkactor_core_kapi_request:actor_to_api_actor(SrvId, Vsn, Data),
%%            {ok, ApiActor, ApiReq2};
%%        {created, Data, ApiReq2} ->
%%            ApiActor = nkactor_core_kapi_request:actor_to_api_actor(SrvId, Vsn, Data),
%%            {ok, ApiActor, ApiReq2};
%%        {raw, {CT, Body}, ApiReq2} ->
%%            {raw, {CT, Body}, ApiReq2};
%%        {status, Status, ApiReq2} ->
%%            {status, Status, ApiReq2};
%%        {error, Error, ApiReq2} ->
%%            {error, Error, ApiReq2}
%%    end.


%% @private
request(ActorSrvId, Verb, Path, Req) ->
    set_debug(ActorSrvId),
    try do_rest_api(ActorSrvId, Verb, Path, Req) of
        %{ok, _, #{meta:=#{nkdomain_http_stream:=true, nkrest_req:=HttpReq}}} ->
        %    {stop, HttpReq};
        {ok, Data, Req2} ->
            rest_api_reply(200, Data, Req2);
        {created, Data, Req2} ->
            rest_api_reply(201, Data, Req2);
        {status, Status, Req2} ->
            #{<<"code">>:=Code} = Status = nkactor_kapi_lib:status(ActorSrvId, Status),
            rest_api_reply(Code, Status, Req2);
        {error, Error, Req2} ->
            #{<<"code">>:=Code} = Status = nkactor_kapi_lib:error(ActorSrvId, Error),
            rest_api_reply(Code, Status, Req2);
        {raw, {CT, Bin}, Req2} ->
            Hds = #{
                <<"Content-Type">> => CT,
                <<"Server">> => <<"NetComposer">>
            },
            {http, 200, Hds, Bin, Req2};
        {http, Code, Hds, Body, HttpReq} ->
            {http, Code, Hds, Body, HttpReq}
    catch
        throw:{error, Error, Req2} ->
            #{<<"code">>:=Code} = Status = nkactor_kapi_lib:error(ActorSrvId, Error),
            rest_api_reply(Code, Status, Req2)
    end.


%% ===================================================================
%% Callbacks
%% ===================================================================

%%
%%%% @doc
%%event_stream_start(#{meta:=#{nkrest_req:=Req}=Meta}=ApiReq) ->
%%    Hds = #{
%%        <<"Content-Type">> => <<"application/json">>,
%%        <<"Server">> => <<"NetComposer">>
%%    },
%%    Req2 = nkrest_http:stream_start(200, Hds, Req),
%%    Meta2 = Meta#{
%%        nkdomain_http_stream => true,
%%        nkrest_req := Req2
%%    },
%%    {ok, ApiReq#{meta:=Meta2}}.
%%
%%
%%%% @doc
%%event_stream_stop(_Reason, #{meta:=#{nkrest_req:=Req}}=ApiReq) ->
%%    lager:error("NKLOG EVENT STREAM STOP"),
%%    nkrest_http:stream_stop(Req),
%%    {ok, ApiReq}.
%%
%%
%%%% @doc
%%event_stream_heartbeat(#{meta:=#{nkrest_req:=Req}}=ApiReq) ->
%%    lager:error("NKLOG EVENT STREAM HEARTBEAT"),
%%    ok = nkrest_http:stream_body(<<"\r\n">>, Req),
%%    {ok, ApiReq}.
%%
%%
%%%% @doc
%%new_event(Event, #{meta:=#{nkrest_req:=Req}}=ApiReq) ->
%%    Body = nklib_json:encode(Event),
%%    nkrest_http:stream_body(Body, Req),
%%    {ok, ApiReq}.


%% ===================================================================
%% Internal
%% ===================================================================


%% @doc
set_debug(ActorSrvId) ->
    Debug = nkserver:get_cached_config(ActorSrvId, nkactor_kapi, debug)==true,
    put(nkactor_kapi_debug, Debug),
    ?API_DEBUG("HTTP debug started", []).


%% @private
rest_api_reply(Code, Body, RestReq) ->
    Hds = #{
        <<"Content-Type">> => <<"application/json">>,
        <<"Server">> => <<"NetComposer">>
    },
    Body2 = nklib_json:encode_sorted(Body),
    {http, Code, Hds, Body2, RestReq}.


%% @private
% /
do_rest_api(ActorSrvId, <<"GET">>, [], RestReq) ->
    Paths1 = ?CALL_SRV(ActorSrvId, api_get_paths, [ActorSrvId, []]),
    Paths2 = lists:usort(lists:flatten(Paths1)),
    {ok, #{<<"paths">>=>Paths2}, RestReq};

do_rest_api(ActorSrvId, <<"GET">>, [<<>>], _RestReq) ->
    do_rest_api(ActorSrvId, <<"GET">>, [], _RestReq);

do_rest_api(_ActorSrvId, <<"GET">>, [<<"favicon.ico">>], RestReq) ->
    {error, {resource_invalid, <<"favicon.ico">>}, RestReq};


% /apis
do_rest_api(ActorSrvId, <<"GET">>, [<<"apis">>], RestReq) ->
    {ok, nkactor_kapi_lib:get_group_list(ActorSrvId), RestReq};

do_rest_api(ActorSrvId, <<"GET">>, [<<"apis">>, <<>>], RestReq) ->
    do_rest_api(ActorSrvId, <<"GET">>, [<<"apis">>], RestReq);


% /apis/Api
do_rest_api(ActorSrvId, <<"GET">>, [<<"apis">>, Group], RestReq) ->
    Groups = nkactor_kapi_lib:get_groups(ActorSrvId),
    case [Info || #{<<"name">>:=N}=Info <-Groups, N==Group] of
        [Info] ->
            {ok, Info, RestReq};
        _ ->
            {error, {api_group_unknown, Group}, RestReq}
    end;


% /apis/Api/Vsn
do_rest_api(ActorSrvId, <<"GET">>, [<<"apis">>, Group, Vsn], RestReq) ->
    case nkactor_kapi_lib:get_modules(ActorSrvId, Group, Vsn) of
        {ok, Modules} ->
            Resources = nkactor_kapi_lib:get_resources(ActorSrvId, Modules),
            ApiVsn = <<Group/binary, $/, Vsn/binary>>,
            {ok, nkactor_kapi_lib:get_resources_list(ApiVsn, Resources), RestReq};
        {error, Error} ->
            {error, Error, RestReq}
    end;

do_rest_api(ActorSrvId, <<"GET">>, [<<"apis">>, Group, Vsn, <<>>], RestReq) ->
    do_rest_api(ActorSrvId, <<"GET">>, [<<"apis">>, Group, Vsn], RestReq);


% /apis/core/v1/namespaces
do_rest_api(ActorSrvId, Verb, [<<"apis">>, Group, Vsn, <<"namespaces">>], RestReq) ->
    Namespace = nkactor:base_namespace(ActorSrvId),
    do_rest_api(ActorSrvId, Verb, [<<"apis">>, Group, Vsn, <<"namespaces">>, Namespace, <<"namespaces">>], RestReq);

% /apis/core/v1/namespaces/Namespace
do_rest_api(ActorSrvId, Verb, [<<"apis">>, Group, Vsn, <<"namespaces">>, Name], RestReq) ->
    Namespace = nkactor:base_namespace(ActorSrvId),
    do_rest_api(ActorSrvId, Verb, [<<"apis">>, Group, Vsn, <<"namespaces">>, Namespace, <<"namespaces">>, Name], RestReq);

% /apis/core/v1/namespaces/Namespace/ResType
do_rest_api(ActorSrvId, Verb, [<<"apis">>, Group, Vsn, <<"namespaces">>, Namespace, ResType], RestReq) ->
    ApiReq = #{
        srv => ActorSrvId,
        verb => Verb,
        group => Group,
        vsn => Vsn,
        namespace => Namespace,
        resource => ResType
    },
    launch_rest_api(ApiReq, RestReq);

% /apis/core/v1/namespaces/Namespace/ResType/_upload
do_rest_api(ActorSrvId, Verb, [<<"apis">>, Group, Vsn, <<"namespaces">>, Namespace, ResType, <<"_upload">>], RestReq) ->
    ApiReq = #{
        srv => ActorSrvId,
        verb => Verb,
        group => Group,
        vsn => Vsn,
        namespace => Namespace,
        resource => ResType
    },
    launch_rest_upload(ApiReq, RestReq);

% /apis/core/v1/namespaces/Namespace/ResType/Name/SubRes/_upload
do_rest_api(ActorSrvId, Verb, [<<"apis">>, Group, Vsn, <<"namespaces">>, Namespace, ResType, Name, RestType2, <<"_upload">>], RestReq) ->
    ApiReq = #{
        srv => ActorSrvId,
        verb => Verb,
        group => Group,
        vsn => Vsn,
        namespace => Namespace,
        resource => ResType,
        name => Name,
        subresource => RestType2
    },
    launch_rest_upload(ApiReq, RestReq);

% /apis/core/v1/namespaces/Namespace/ResType/Name...
do_rest_api(ActorSrvId, Verb, [<<"apis">>, Group, Vsn, <<"namespaces">>, Namespace, ResType, Name|SubRes], RestReq) ->
    ApiReq1 = #{
        srv => ActorSrvId,
        verb => Verb,
        group => Group,
        vsn => Vsn,
        namespace => Namespace,
        resource => ResType,
        name => Name
    },
    case lists:reverse(SubRes) of
        [<<"_upload">>|SubRes2] ->
            ApiReq2 = ApiReq1#{subresource => nklib_util:bjoin(lists:reverse(SubRes2), $/)},
            launch_rest_upload(ApiReq2, RestReq);
        _ ->
            ApiReq2 = ApiReq1#{subresource => nklib_util:bjoin(SubRes, $/)},
            launch_rest_api(ApiReq2, RestReq)
    end;

% /apis/core/v1/ResType (implicit namespace)
do_rest_api(ActorSrvId, Verb, [<<"apis">>, Group, Vsn, ResType], RestReq) ->
    Namespace = nkactor:base_namespace(ActorSrvId),
    do_rest_api(ActorSrvId, Verb, [<<"apis">>, Group, Vsn, <<"namespaces">>, Namespace, ResType], RestReq);

% /apis/core/v1/ResType/Name (implicit namespace)
do_rest_api(ActorSrvId, Verb, [<<"apis">>, Group, Vsn, ResType, Name|SubRes], RestReq) ->
    Namespace = nkactor:base_namespace(ActorSrvId),
    do_rest_api(ActorSrvId, Verb, [<<"apis">>, Group, Vsn, <<"namespaces">>, Namespace, ResType, Name|SubRes], RestReq);


% /search/v1
do_rest_api(ActorSrvId, Verb, [?GROUP_SEARCH, Vsn], RestReq) ->
    Namespace = nkactor:base_namespace(ActorSrvId),
    do_rest_api(ActorSrvId, Verb, [?GROUP_SEARCH, Vsn, <<"namespaces">>, Namespace], RestReq);

% /search/v1/namespaces/Namespace
do_rest_api(ActorSrvId, Verb, [?GROUP_SEARCH, Vsn, <<"namespaces">>, Namespace], RestReq) ->
    ApiReq = #{
        srv => ActorSrvId,
        verb => Verb,
        group => ?GROUP_SEARCH,
        vsn => Vsn,
        resource => <<"actors">>,
        namespace => Namespace
    },
    launch_rest_search(ApiReq, RestReq);


% /graphql
do_rest_api(_ActorSrvId, <<"POST">>, [<<"graphql">>], RestReq) ->
    {error, {resource_invalid, <<>>}, RestReq};

do_rest_api(_ActorSrvId, Verb, [<<"_test">>, <<"faxin">>|Rest], RestReq) ->
    BodyOpts = #{max_size=>?MAX_BODY_SIZE},
    {Body, _Req2} = case nkrest_http:get_body(RestReq, BodyOpts) of
        {ok, B0, R0} ->
            {B0, R0};
        {error, Error} ->
            ?API_LOG(warning, "error reading body: ~p" , [Error]),
            throw({error, request_body_invalid, RestReq})
    end,
    Qs = nkrest_http:get_qs(RestReq),
    Hds = nkrest_http:get_headers(RestReq),
    lager:error("NKLOG HTTP FAX IN (~s)\nPath: ~p\nQs: ~p\nHeaders: ~p\nBody: ~p\n",
        [Verb, Rest, Qs, Hds, Body]),
    Rep = <<"<Response><Receive action=\"/fax/received\"/></Response>">>,
    {binary, <<"application/xml">>, Rep};


% /bulk
do_rest_api(ActorSrvId, Verb, [?GROUP_BULK], RestReq) ->
    launch_rest_bulk(ActorSrvId, Verb, RestReq);


% /_test
do_rest_api(_ActorSrvId, Verb, [<<"_test">>|Rest], RestReq) ->
    BodyOpts = #{max_size=>?MAX_BODY_SIZE},
    {Body, _RestReq2} = case nkrest_http:get_body(RestReq, BodyOpts) of
        {ok, B0, R0} ->
            {B0, R0};
        {error, Error} ->
            ?API_LOG(warning, "error reading body: ~p" , [Error]),
            throw({error, request_body_invalid, RestReq})
    end,
    Qs = nkrest_http:get_qs(RestReq),
    Hds = nkrest_http:get_headers(RestReq),
    lager:error("NKLOG HTTP _TEST (~s)\nPath: ~p\nQs: ~p\nHeaders: ~p\nBody: ~p\n",
                [Verb, Rest, Qs, Hds, Body]),
    {ok, #{}, RestReq};

do_rest_api(_ActorSrvId, _Verb, Path, RestReq) ->
    {error, {resource_invalid, nklib_util:bjoin(Path, $/)}, RestReq}.


%% @doc
launch_rest_api(ApiReq, RestReq) ->
    #{verb:=Verb} = ApiReq,
    ?API_DEBUG("HTTP incoming: ~s ~p", [Verb, ApiReq]),
    Qs = maps:from_list(nkrest_http:get_qs(RestReq)),
    Hds = nkrest_http:get_headers(RestReq),
    Token = case maps:get(<<"x-nkdomain-token">>, Hds, <<>>) of
        <<>> ->
            maps:get(<<"adminToken">>, Qs, <<>>);
        HdToken ->
            HdToken
    end,
    BodyOpts = #{max_size=>?MAX_BODY_SIZE, parse=>true},
    {Body, Req2} = case nkrest_http:get_body(RestReq, BodyOpts) of
        {ok, B0, R0} ->
            {B0, R0};
        {error, Error} ->
            ?API_LOG(warning, "error reading body: ~p" , [Error]),
            throw({error, request_body_invalid, RestReq})
    end,
    ToWatch = maps:get(<<"watch">>, Qs, undefined) == <<"true">>,
    Name = maps:get(name, ApiReq, <<>>),
    Verb2 = case Verb of
        <<"GET">> when ToWatch ->
            watch;
        <<"GET">> when Name == <<>> ->
            list;
        <<"POST">> when Name == <<>> ->
            list;
        <<"GET">> ->
            get;
        <<"HEAD">> when ToWatch ->
            watch;
        <<"HEAD">> when Name == <<>> ->
            list;
        <<"HEAD">> ->
            get;
        <<"POST">> ->
            create;
        <<"PUT">> ->
            update;
        <<"PATCH">> when Name == <<>> ->
            throw({error, method_not_allowed, RestReq});
        <<"PATCH">> ->
            patch;
        <<"DELETE">> when Name == <<>> ->
            deletecollection;
        <<"DELETE">> ->
            delete;
        _ ->
            throw({error, method_not_allowed, RestReq})
    end,
ApiReq2 = ApiReq#{
        verb => Verb2,
        params => Qs,
        body => Body,
        auth => #{token => Token},
        callback => ?MODULE,
        external_url => nkrest_http:get_external_url(RestReq),
        meta => #{
            nkrest_req => Req2
        },
        ot_span_id => maps:get(span, RestReq, undefined)
    },
    {Status, Result, #{meta:=#{nkrest_req:=Req3}}} = nkactor_kapi:request(ApiReq2),
    {Status, Result, Req3}.


%% @doc
launch_rest_upload(ApiReq, RestReq) ->
    #{verb:=Verb} = ApiReq,
    ?API_DEBUG("HTTP incoming upload: ~s ~p", [Verb, ApiReq]),
    Qs = maps:from_list(nkrest_http:get_qs(RestReq)),
    Hds = nkrest_http:get_headers(RestReq),
    Token = case maps:get(<<"x-nkdomain-token">>, Hds, <<>>) of
        <<>> ->
            maps:get(<<"adminToken">>, Qs, <<>>);
        HdToken ->
            HdToken
    end,
    BodyOpts = #{max_size=>?MAX_UPLOAD_SIZE, parse=>false},
    {Body, Req2} = case nkrest_http:get_body(RestReq, BodyOpts) of
        {ok, B0, R0} ->
            {B0, R0};
        {error, Error} ->
            ?API_LOG(warning, "error reading body: ~p" , [Error]),
            throw({error, request_body_invalid, RestReq})
    end,
    Verb2 = case Verb of
        <<"POST">> ->
            upload;
        _ ->
            throw({error, method_not_allowed, RestReq})
    end,
    ApiReq2 = ApiReq#{
        %class => upload,
        verb => Verb2,
        params => Qs,
        body => Body,
        auth => #{token => Token},
        callback => ?MODULE,
        external_url => nkrest_http:get_external_url(RestReq),
        meta => #{
            nkrest_req => Req2
        }
    },
    {Status, Result, #{meta:=#{nkrest_req:=Req3}}} = nkactor_kapi:request(ApiReq2),
    {Status, Result, Req3}.


%% @private
launch_rest_search(ApiReq, RestReq) ->
    Qs = maps:from_list(nkrest_http:get_qs(RestReq)),
    Hds = nkrest_http:get_headers(RestReq),
    Token = case maps:get(<<"x-nkdomain-token">>, Hds, <<>>) of
        <<>> ->
            maps:get(<<"adminToken">>, Qs, <<>>);
        HdToken ->
            HdToken
    end,
    BodyOpts = #{max_size=>?MAX_BODY_SIZE, parse=>true},
    {Body, Req2} = case nkrest_http:get_body(RestReq, BodyOpts) of
        {ok, B0, R0} ->
            {B0, R0};
        {error, Error} ->
            ?API_LOG(warning, "error reading body: ~p" , [Error]),
            throw({error, request_body_invalid, RestReq})
    end,
    Delete = maps:get(<<"delete">>, Qs, undefined) == <<"true">>,
    #{verb:=Verb} = ApiReq,
    Verb2 = case Verb of
        <<"POST">> when Delete ->
            deletecollection;
        <<"GET">> ->
            list;
        <<"POST">> ->
            list;
        <<"DELETE">> ->
            deletecollection;
        _ ->
            throw({error, {method_not_allowed, Verb}, RestReq})
    end,
    ApiReq2 = ApiReq#{
        verb => Verb2,
        group => ?GROUP_SEARCH,
        body => Body,
        auth => #{token => Token},
        external_url => nkrest_http:get_external_url(RestReq),
        meta => #{
            nkrest_req => Req2
        }
    },
    {Status, Result, #{meta:=#{nkrest_req:=Req3}}} = nkactor_kapi:search(ApiReq2),
    {Status, Result, Req3}.


%% @private
launch_rest_bulk(ActorSrvId, <<"PUT">>, RestReq) ->
    Qs = maps:from_list(nkrest_http:get_qs(RestReq)),
    Hds = nkrest_http:get_headers(RestReq),
    Token = case maps:get(<<"x-nkdomain-token">>, Hds, <<>>) of
        <<>> ->
            maps:get(<<"adminToken">>, Qs, <<>>);
        HdToken ->
            HdToken
    end,
    BodyOpts = #{max_size=>?MAX_BODY_SIZE, parse=>true},
    {Body, RestReq2} = case nkrest_http:get_body(RestReq, BodyOpts) of
        {ok, B0, R0} ->
            {B0, R0};
        {error, Error} ->
            ?API_LOG(warning, "error reading body: ~p" , [Error]),
            throw({error, request_body_invalid, RestReq})
    end,
    Status = case nkdomain:load_actor_data(Body, Token) of
        {ok, Res} ->
            lists:map(
                fun
                    ({Name, created}) -> #{name=>Name, result=>created};
                    ({Name, updated}) -> #{name=>Name, result=>updated};
                    ({Name, {error, Error}}) -> #{name=>Name, result=>error, error=>nklib_util:to_binary(Error)}
                end,
                Res);
        {error, LoadError} ->
            nkactor_kapi_lib:error(ActorSrvId, LoadError)
    end,
    rest_api_reply(200, Status, RestReq2);

launch_rest_bulk(_ActorSrvId, _Verb, RestReq) ->
    throw({error, method_not_allowed, RestReq}).





%%%% @private
%%to_bin(Term) when is_binary(Term) -> Term;
%%to_bin(Term) -> nklib_util:to_binary(Term).