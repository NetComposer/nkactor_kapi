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

-export([pre_request/1, search_params/1, search_opts/1]).

-include_lib("nkserver/include/nkserver.hrl").


%% ===================================================================
%% Utilities
%% ===================================================================

%% @doc Updates request with group, vsn, resource, name, namespace, uid from body,
%% and checks that nothing is changed if present.
pre_request(#{body:=Body}=Req) ->
    try
        Syntax = #{
            apiVersion => binary,
            kind => binary,
            metadata => #{
                uid => binary,
                name => binary,
                namespace => binary,
                resourceVersion => {'__key', hash}
            }
        },
        Body2 = case nklib_syntax:parse_all(Body, Syntax) of
            {ok, ParsedBody} ->
                ParsedBody;
            {error, ParseError} ->
                throw(ParseError)
        end,
        Meta2 = maps:get(metadata, Body2, #{}),
        Req2 = case Meta2 of
            #{namespace:=BodyNamespace} ->
                case maps:find(namespace, Req) of
                    {ok, BodyNamespace} ->
                        Req;
                    error ->
                        Req#{namespace => BodyNamespace};
                    _ ->
                        throw({resource_invalid, namespace})
                end;
            _ ->
                Req
        end,
        Namespace = case Req2 of
            #{namespace:=ReqNamespace} ->
                ReqNamespace;
            _ ->
                throw(namespace_missing)
        end,
        Req3 = case Body2 of
            #{apiVersion:=ApiVersion, kind:=Kind} ->
                case nkactor_kapi_lib:get_group_vsn(ApiVersion) of
                    {Group, Vsn} ->
                        case maps:get(group, Req, Group) of
                            Group ->
                                ok;
                            _ ->
                                throw({resource_invalid, apiVersion})
                        end,
                        case maps:get(vsn, Req, Vsn) of
                            Vsn ->
                                ok;
                            _ ->
                                throw({resource_invalid, apiVersion})
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
                        Req2#{
                            group => Group,
                            vsn => Vsn,
                            resource => Res
                        };
                    error ->
                        throw({resource_invalid, apiVersion})
                end;
            _ ->
                Req2
        end,
        Req4 = case maps:find(name, Meta2) of
            {ok, Name} ->
                Req3#{name => Name};
            _ ->
                Req3
        end,
        Req5 = case maps:find(uid, Meta2) of
            {ok, UID} ->
                Req4#{uid => UID};
            _ ->
                Req4
        end,
        Meta3 = maps:without([uid, name, namespace], Meta2),
        Body3 = maps:without([apiVersion, kind], Body2),
        {ok, Req5#{body:=Body3#{metadata=>Meta3}}}
    catch
        throw:Throw ->
            {error, Throw, Req}
    end;

pre_request(Req) ->
    {ok, Req}.




%% @doc
search_opts(Req) ->
    #{namespace:=Namespace, srv:=SrvId} = Req,
    Body = maps:get(body, Req, #{}),
    Syntax = #{
        apiVersion => binary,
        kind => binary
    },
    Params1 = maps:get(params, Req, #{}),
    Params2 = search_params(Params1),
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
                                    Base = maps:with([fields_filter, fields_sort, fields_trans, fields_type], Config),
                                    Opts = Base#{
                                        params => Params2,
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
                                    {ok, Opts}
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
                    Base = maps:with([fields_filter, fields_sort, fields_trans, fields_type], Config),
                    Opts = Base#{
                        params => Params2,
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
                error ->
                    {error, {resource_invalid, ApiVersion}}
            end;
        {ok, #{kind:=_}, _} ->
            {error, {field_missing, <<"apiVersion">>}};
        {ok, _, _} ->
            Config = nkactor_actor:get_common_config(SrvId),
            Base = maps:with([fields_filter, fields_sort, fields_trans, fields_type], Config),
            Opts = Base#{
                params => Params2,
                forced_spec => #{
                    namespace => Namespace
                }
            },
            {ok, Opts};
        {error, Error} ->
            {error, Error}
    end.



%% @doc Adapts API request to a valid request
%% - Adds fields_trans (nkactor_kapi_search:field_trans())
%% - Converts parameters fieldSelector, labelSelector, linkedTo, fts, sort to maps
search_params(Req) ->
    Params = maps:get(params, Req, #{}),
    Syntax = #{
        fieldSelector => {'__key', fields, binary},
        labelSelector => {'__key', labels, binary},
        linkedTo => {'__key', links, binary},
        getTotals => {'__key', get_totals, boolean},
        fts => binary,
        sort => binary
    },
    {ok, Params2} = nklib_syntax:parse_all(Params, Syntax),
    Params3 = parse_params_fields(Params2),
    Req#{params => Params3}.


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
            Map#{Field2 => <<>>};
        [Field2, Value] ->
            Map#{Field2 => Value}
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
            Map#{Field2 => <<>>};
        [Prefix, Field2] ->
            Map#{Field2 => Prefix}
    end,
    parse_params_sort(Rest, Map2).



%%%% @private
%%to_bin(Term) when is_binary(Term) -> Term;
%%to_bin(Term) -> nklib_util:to_binary(Term).

