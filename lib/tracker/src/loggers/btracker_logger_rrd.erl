% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
% Copyright (C) 2012 <Sébastien Serre sserre.bx@gmail.com>
% 
% Enms is a Network Management System aimed to manage and monitor SNMP
% targets, monitor network hosts and services, provide a consistent
% documentation system and tools to help network professionals
% to have a wide perspective of the networks they manage.
% 
% Enms is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
% 
% Enms is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with Enms.  If not, see <http://www.gnu.org/licenses/>.
% @doc
% This module log data to an rrd database. Data must exist in the
% #probe_return.key_val record.
% To work a valid #rrd_def record must be givent at init conf input.
% @end
-module(btracker_logger_rrd).
-behaviour(beha_tracker_logger).
-include("../../include/tracker.hrl").

-export([
    init/2,
    log/2,
    dump/2
]).

-record(rrd_logger_state, {
    file,
    binds
}).

init(Conf, #probe_server_state{
            target          = #target{directory = TargetDir},
            loggers_state   = LoggersState
        } = ProbeServerState) -> 

    #rrd_def{
        create          = #rrd_create{file = FileName} = RrdCreate,
        update_binds    = Binds
    } = Conf,

    File = filename:absname_join(TargetDir, FileName),

    case errd_server:info(rrd_tracker, File) of
        {error, _}  ->
            {ok, _} = errd_server:command(rrd_tracker, 
                    RrdCreate#rrd_create{file = File});
        {rrd, _, _, _, _, _, _}    ->
            ok;
        A ->
            io:format("rrd:info is ~p~n", [A])
    end,

    
    
    RrdLoggerState = #rrd_logger_state{
        file            = File,
        binds           = Binds
    },
    {ok, ProbeServerState#probe_server_state{
            loggers_state = [RrdLoggerState | LoggersState]
        }
    }.

log(_, #probe_return{key_val = []}) ->
    io:format("no data do nothing~n"),
    ok;

log(    #probe_server_state{
            loggers_state = LoggersState
        }, 
        #probe_return{
            timestamp   = Timestamp,
            key_val     = Datas
        }) ->

    #rrd_logger_state{
        file            = File,
        binds           = Binds
    } = lists:keyfind(rrd_logger_state, 1, LoggersState),

    RrdUpdateList = lists:foldl(fun({rrd_ds_bind, Term, DsName}, Acc) ->
        case get_ds_data_for(Term, Datas) of
            false ->
                Acc;
            {ok, Value} ->
                [
                    #rrd_ds_update{
                        name    =   DsName,
                        value   =   Value
                    }
                | Acc]
        end
    end, [], Binds),

    RrdUpdate = #rrd_update {
        file    =   File,
        time    =   integer_to_list(Timestamp),
        updates =   RrdUpdateList
    },

    {ok, _} = errd_server:command(rrd_tracker, RrdUpdate),

    ok;

log(_,_) ->
    io:format("."),
    ok.

dump(_PState, _Timeout) -> 
    ignore.

% PRIVATE
% @private
get_ds_data_for(Term, DataList) ->
    case lists:keyfind(Term, 1, DataList) of
        false ->
            false;
        {Term, Value}       ->
            {ok, Value}
    end.
