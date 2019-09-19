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
-module(nkactor_kapi_parse).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([req_actor/1, from_external/4, search_params/1, search_opts/1]).

-include_lib("nkserver/include/nkserver.hrl").


%% ===================================================================
%% Utilities
%% ===================================================================

%% @doc
%% - Adapts standard parameters
%% In case request has a body, it is verb or create and empty subres
%% - adapts the body from KAPI format to request format
%% - it does not adapts data or most of metadata fields
req_actor(Req) ->
    Verb = maps:get(verb, Req, get),
    SubRes = maps:get(subresource, Req, <<>>),
    case params(Verb, SubRes, Req) of
        {ok, Req2} ->
            do_req_actor(Verb, SubRes, Req2);
        {error, Error} ->
            {error, Error, Req}
    end.


%% @private
do_req_actor(Verb, <<>>, #{body:=Actor}=Req) when
        (Verb==create orelse Verb==update) andalso is_map(Actor) ->
    try
        Syntax = #{
            apiVersion => binary,
            kind => binary,
            metadata => #{
                uid => binary,
                name => binary,
                namespace => binary
                %resourceVersion => {'__key', hash}
            }
        },
        Parsed = case nklib_syntax:parse_all(Actor, Syntax) of
            {ok, ParsedActor} ->
                ParsedActor;
            {error, ParseError} ->
                throw(ParseError)
        end,
        Meta = maps:get(metadata, Parsed, #{}),
        BaseFields = maps:with([uid, name, namespace], Meta),
        Data = maps:without([apiVersion, kind, metadata], Parsed),
        MetaFields = maps:without([uid, name, namespace], Meta),
        Actor1 = BaseFields#{
            data => Data,
            metadata => MetaFields
        },
        Actor2 = case Parsed of
            #{apiVersion:=ApiVersion} ->
                case nkactor_kapi_lib:get_group_vsn(ApiVersion) of
                    {ApiGroup, Vsn} ->
                        Actor1#{
                            group => ApiGroup,
                            metadata := MetaFields#{vsn => Vsn}
                        };
                    error ->
                        throw({resource_invalid, apiVersion})
                end;
            _ ->
                Actor1
        end,
        Actor3 = case Parsed of
            #{kind:=Kind} ->
                Group = case Actor2 of
                    #{group:=ActorGroup} ->
                        ActorGroup;
                    _ ->
                        case nklib_syntax:parse(Req, #{group=>binary}) of
                            {ok, #{group:=ReqGroup}, _} ->
                                ReqGroup;
                            _ ->
                                throw({field_missing, <<"apiVersion">>})
                        end
                end,
                Namespace = case maps:find(namespace, Actor2) of
                    {ok, ActorNamespace} ->
                        ActorNamespace;
                    error ->
                        case nklib_syntax:parse(Req, #{namespace=>binary}) of
                            {ok, #{namespace:=ReqNamespace}, _} ->
                                ReqNamespace;
                            _ ->
                                <<>>
                        end
                end,
                SrvId = case nkactor_namespace:find_service(Namespace) of
                    {ok, SrvId0} ->
                        SrvId0;
                    {error, SrvError} ->
                        throw(SrvError)
                end,
                Res = case nkactor_actor:get_module(SrvId, Group, {camel, Kind}) of
                    undefined ->
                        throw({resource_invalid, kind});
                    Module ->
                        #{resource:=Res0} = nkactor_actor:get_config(SrvId, Module),
                        Res0
                end,
                Actor2#{resource => Res};
            _ ->
                Actor2
        end,
        {ok, Req#{body:=Actor3}}
    catch
        throw:Throw ->
            {error, Throw, Req}
    end;

do_req_actor(_Verb, _SubRes, Req) ->
    {ok, Req}.


%% @doc
from_external(SrvId, Group, Res, Actor) ->
    Actor2 = actor_metadata(Actor),
    Syntax = ?CALL_SRV(SrvId, actor_kapi_parse, [Group, Res]),
    nklib_syntax:parse_all(Actor2, Syntax).


%% @doc
actor_metadata(Actor) when is_map(Actor) ->
    Syntax = #{
        metadata => #{
            resourceVersion => {'__key', hash},
            creationTime => {'__key', creation_time},
            updateTime => {'__key', update_time},
            isEnabled => {'__key', is_enabled},
            isActive => {'__key', is_active},
            expiresTime => {'__key', expires_time},
            inAlarm => {'__key', in_alarm},
            alarms => {list, #{
                lastTime => {'__key', last_time}
            }},
            updatedBy => {'__key', updated_by},
            createdBy => {'__key', created_by},
            nextStatusTime => {'__key', next_status_time}
        }
    },
    {ok, Parsed} = nklib_syntax:parse_all(Actor, Syntax),
    Parsed;

actor_metadata(Actor) ->
    Actor.

%% @doc
params(list, <<>>, Req) ->
    case search_params(Req) of
        {ok, Params2} ->
            {ok, Req#{params:=Params2}};
        {error, Error} ->
            {error, Error}
    end;

params(Verb, <<>>, #{params:=Params}=Req) ->
    Syntax = params_syntax(Verb),
    case nklib_syntax:parse(Params, Syntax) of
        {ok, Params2, []} ->
            {ok, Req#{params:=Params2}};
        {ok, _, [Field|_]} ->
            {error, {parameter_invalid, Field}};
        {error, Error} ->
            {error, Error}
    end;

params(_Verb, _SubRes, Req) ->
    {ok, Req}.


%% @private
params_syntax(get) ->
    #{
        activate => boolean,
        consume => boolean,
        ttl => pos_integer
    };

params_syntax(create) ->
    #{
        activate => boolean,
        ttl => pos_integer
    };

params_syntax(update) ->
    #{
        activate => boolean,
        ttl => pos_integer,
        mergeData => {'__key', merge_data, boolean},
        allowNameChange => {'__key', allow_name_change, boolean}
    };

params_syntax(delete) ->
    #{
        activate => boolean
    };

params_syntax(_) ->
    #{
    }.


%% @doc
search_opts(Req) ->
    #{namespace:=Namespace, srv:=SrvId} = Req,
    Body = maps:get(body, Req, #{}),
    Syntax = #{
        apiVersion => binary,
        kind => binary
    },
    case nklib_syntax:parse(Body, Syntax) of
        {ok, #{apiVersion:=ApiVersion, kind:=Kind}, _} ->
            case nkactor_kapi_lib:get_group_vsn(ApiVersion) of
                {Group, _Vsn} ->
                    case nkactor_namespace:find_service(Namespace) of
                        {ok, SrvId} ->
                            case nkactor_actor:get_module(SrvId, Group, {camel, Kind}) of
                                undefined ->
                                    {error, {resource_invalid, ApiVersion}};
                                Module ->
                                    #{resource:=Res} = nkactor_actor:get_config(SrvId, Module),
                                    Config = nkactor_actor:get_config(SrvId, Module),
                                    Base = maps:with([fields_filter, fields_sort, fields_type], Config),
                                    case search_params(Req) of
                                        {ok, Params2} ->
                                            Opts = Base#{
                                                params => Params2,
                                                fields_trans => nkactor_kapi:get_fields_trans(SrvId),
                                                forced_spec => #{
                                                    namespace => Namespace,
                                                    filter => #{
                                                        'and' => [
                                                            #{field=>group, value=>Group},
                                                            #{field=>resource, value=>Res}
                                                        ]
                                                    }
                                                }
                                            },
                                            {ok, Opts};
                                        {error, Error} ->
                                            {error, Error}
                                    end
                            end;
                        {error, Error} ->
                            {error, Error}
                    end;
                error ->
                    {error, {resource_invalid, ApiVersion}}
            end;
        {ok, #{apiVersion:=ApiVersion}, _} ->
            case nkactor_kapi_lib:get_group_vsn(ApiVersion) of
                {Group, _Vsn} ->
                    Config = nkactor_actor:get_common_config(SrvId),
                    Base = maps:with([fields_filter, fields_sort, fields_type], Config),
                    case search_params(Req) of
                        {ok, Params2} ->
                            Opts = Base#{
                                params => Params2,
                                fields_trans => nkactor_kapi:get_fields_trans(SrvId),
                                forced_spec => #{
                                    namespace => Namespace,
                                    filter => #{
                                        'and' => [
                                            #{field=>group, value=>Group}
                                        ]
                                    }
                                }
                            },
                            {ok, Opts};
                        {error, Error} ->
                            {error, Error}
                    end;
                error ->
                    {error, {resource_invalid, ApiVersion}}
            end;
        {ok, #{kind:=_}, _} ->
            {error, {field_missing, <<"apiVersion">>}};
        {ok, _, _} ->
            Config = nkactor_actor:get_common_config(SrvId),
            Base = maps:with([fields_filter, fields_sort, fields_type], Config),
            case search_params(Req) of
                {ok, Params2} ->
                    Opts = Base#{
                        params => Params2,
                        fields_trans => nkactor_kapi:get_fields_trans(SrvId),
                        forced_spec => #{
                            namespace => Namespace
                        }
                    },
                    {ok, Opts};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Adapts API request to a valid request
search_params(#{srv:=SrvId}=Req) ->
    Params = maps:get(params, Req, #{}),
    Syntax = #{
        namespace => binary,
        deep => boolean,
        from => pos_integer,
        size => pos_integer,
        fieldSelector => {'__key', fields, binary},
        labelSelector => {'__key', labels, binary},
        linkedTo => {'__key', links, binary},
        getTotal => {'__key', get_total, boolean},
        fts => binary,
        sort => binary
    },
    case nklib_syntax:parse(Params, Syntax) of
        {ok, Params2, []} ->
            Params3 = parse_params_fields(Params2),
            Params4 = Params3#{fields_trans => nkactor_kapi:get_fields_trans(SrvId)},
            {ok, Params4};
        {ok, _, [Field|_]} ->
            {error, {parameter_invalid, Field}};
        {error, Error} ->
            {error, Error}
    end.


%% @private
parse_params_fields(#{fields:=Fields}=Params) ->
    Values = binary:split(Fields, <<$,>>, [global]),
    Fields2 = parse_params_fields(Values, #{}),
    parse_params_labels(Params#{fields:=Fields2});

parse_params_fields(Params) ->
    parse_params_labels(Params).


%% @private
parse_params_fields([], Map) ->
    Map;

parse_params_fields([Field|Rest], Map) ->
    Map2 = case binary:split(Field, <<":">>) of
        [Field2] ->
            Map#{to_field(Field2) => <<>>};
        [Field2, Value] ->
            Map#{to_field(Field2) => Value}
    end,
    parse_params_fields(Rest, Map2).


%% @private
parse_params_labels(#{labels:=Labels}=Params) ->
    Values = binary:split(Labels, <<$,>>, [global]),
    Labels2 = parse_params_labels(Values, #{}),
    parse_params_links(Params#{labels:=Labels2});

parse_params_labels(Params) ->
    parse_params_links(Params).


%% @private
parse_params_labels([], Map) ->
    Map;

parse_params_labels([Label|Rest], Map) ->
    Map2 = case binary:split(Label, <<":">>) of
        [Label2] ->
            Map#{Label2 => <<>>};
        [Label2, Value] ->
            Map#{Label2 => Value}
    end,
    parse_params_labels(Rest, Map2).


%% @private
parse_params_links(#{links:=Links}=Params) ->
    Values = binary:split(Links, <<$,>>, [global]),
    Links2 = parse_params_links(Values, #{}),
    parse_params_fts(Params#{links:=Links2});

parse_params_links(Params) ->
    parse_params_fts(Params).


%% @private
parse_params_links([], Map) ->
    Map;

parse_params_links([Link|Rest], Map) ->
    Map2 = case binary:split(Link, <<":">>) of
        [Link2] ->
            Map#{Link2 => <<>>};
        [Link2, Type] ->
            Map#{Link2 => Type}
    end,
    parse_params_links(Rest, Map2).


%% @private
parse_params_fts(#{fts:=Fts}=Params) ->
    Values = binary:split(Fts, <<$,>>, [global]),
    Links2 = parse_params_fts(Values, #{}),
    parse_params_sort(Params#{fts:=Links2});

parse_params_fts(Params) ->
    parse_params_sort(Params).


%% @private
parse_params_fts([], Map) ->
    Map;

parse_params_fts([Link|Rest], Map) ->
    Map2 = case binary:split(Link, <<":">>) of
        [Word] ->
            Map#{<<"*">> => Word};
        [Field, Word] ->
            Map#{Field => Word}
    end,
    parse_params_fts(Rest, Map2).


%% @private
parse_params_sort(#{sort:=Sort}=Params) ->
    Fields = binary:split(Sort, <<$,>>, [global]),
    Sort2 = parse_params_sort(Fields, #{}),
    Params#{sort:=Sort2};

parse_params_sort(Params) ->
    Params.


%% @private
parse_params_sort([], Map) ->
    Map;

parse_params_sort([Field|Rest], Map) ->
    Map2 = case binary:split(Field, <<":">>) of
        [Field2] ->
            Map#{to_field(Field2) => <<"asc">>};
        [Prefix, Field2] ->
            Map#{to_field(Field2) => Prefix}
    end,
    parse_params_sort(Rest, Map2).


%% @private Adds "data." at head it not there
to_field(<<"data.", _/binary>>=Field) -> Field;
to_field(<<"metadata.", _/binary>>=Field) -> Field;
to_field(Field) -> <<"data.", Field/binary>>.


%%%% @private
%%to_bin(Term) when is_binary(Term) -> Term;
%%to_bin(Term) -> nklib_util:to_binary(Term).

