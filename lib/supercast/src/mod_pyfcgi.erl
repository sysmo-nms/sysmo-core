-module(mod_pyfcgi).

-compile(export_all).

-include_lib("../yaws/include/yaws_api.hrl").

box(Str) ->
    {'div',[{class,"box"}],
     {pre,[],Str}}.

% out(A) ->
%         {ehtml,
%      [{p,[],
%        box(io_lib:format("A#arg.appmoddata = ~p~n"
%                          "A#arg.appmod_prepath = ~p~n"
%                          "A#arg.querydata = ~p~n",
%                          [A#arg.appmoddata,
%                           A#arg.appmod_prepath,
%                           A#arg.querydata]))}]}.
out(Arg) ->
    case yaws_cgi:call_fcgi_responder(Arg) of
        R when is_list(R) ->
            case proplists:get_value(status, R) of
                404 -> {page, Arg#arg.server_path};  % Let Yaws try to serve static files
                _   -> R
            end;

        X -> X
    end.
