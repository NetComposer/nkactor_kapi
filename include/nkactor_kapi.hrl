-ifndef(NKACTOR_KAPI_HRL_).
-define(NKACTOR_KAPI_HRL_, 1).

%% ===================================================================
%% Defines
%% ===================================================================

-define(GROUP_SEARCH, <<"search">>).
-define(GROUP_BULK, <<"bulk">>).


-define(API_DEBUG(Txt, Args),
    case erlang:get(nkactor_kapi_debug) of
        true -> ?API_LOG(debug, Txt, Args);
        _ -> ok
    end).

-define(API_DEBUG(Txt, Args, Req),
    case erlang:get(nkactor_kapi_debug) of
        true -> ?API_LOG(debug, Txt, Args, Req);
        _ -> ok
    end).


-define(API_LOG(Type, Txt, Args, Req),
    lager:Type(
        [
            {verb, maps:get(verb, Req, get)},
            {group, maps:get(group, Req, <<>>)},
            {resource, maps:get(resource, Req, <<>>)},
            {name, maps:get(name, Req, <<>>)}
        ],
        "NkACTOR KAPI (~s ~s/~s/~s) " ++ Txt,
        [
            maps:get(verb, Req, get),
            maps:get(group, Req, <<>>),
            maps:get(resource, Req, <<>>),
            maps:get(name, Req, <<>>) |
            Args
        ]
    )).

-define(API_LOG(Type, Txt, Args),
    lager:Type( "NkACTOR KAPI " ++ Txt, Args)).


-endif.