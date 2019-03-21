%%==============================================================================
%% Sysmo NMS Network Management and Monitoring solution (https://sysmo-nms.github.io)
%%
%% Copyright (c) 2012-2017 Sebastien Serre <ssbx@sysmo.io>
%%
%% Sysmo NMS is free software: you can redistribute it and/or modify it under
%% the terms of the GNU General Public License as published by the Free Software
%% Foundation, either version 3 of the License, or (at your option) any later
%% version.
%%
%% Sysmo NMS is distributed in the hope that it will be useful, but WITHOUT ANY
%% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
%% A PARTICULAR PURPOSE. See the GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License along with
%% Sysmo.  If not, see <http://www.gnu.org/licenses/>.
%%==============================================================================
% @private
-module(monitor_jobs).
-include("monitor.hrl").
-include("monitor_snmp.hrl").
-include_lib("common_hrl/include/logs.hrl").

-export([update_snmp_system_info/1, update_snmp_if_aliases/1]).

update_snmp_system_info(TargetKey) ->
    case j_server_snmpman:get(TargetKey, [
        ?SYS_DESCR,
        ?SYS_OBJECTID,
        ?SYS_CONTACT,
        ?SYS_NAME,
        ?SYS_LOCATION,
        ?SYS_SERVICES]) of
        {ok, {varbinds, Ret}} ->
            case lists:keyfind(?SYS_DESCR, 2, Ret) of
                {_,_,_,SDescr} ->
                    P = [{"sysDescr", SDescr}];
                _ ->
                    P = []
            end,
            case lists:keyfind(?SYS_OBJECTID, 2, Ret) of
                {_,_,_,SOId} ->
                    P1 = [{"sysObjectId", SOId} | P];
                _ ->
                    P1 = P
            end,
            case lists:keyfind(?SYS_CONTACT, 2, Ret) of
                {_,_,_,SCont} ->
                    P2 = [{"sysContact", SCont} | P1];
                _ ->
                    P2 = P1
            end,
            case lists:keyfind(?SYS_NAME, 2, Ret) of
                {_,_,_,SName} ->
                    P3 = [{"sysName", SName} | P2];
                _ ->
                    P3 = P2
            end,
            case lists:keyfind(?SYS_LOCATION, 2, Ret) of
                {_,_,_,SLoc} ->
                    P4 = [{"sysLocation", SLoc} | P3];
                _ ->
                    P4 = P3
            end,
            case lists:keyfind(?SYS_SERVICES, 2, Ret) of
                {_,_,_,SServ} ->
                    PF = [{"sysServices", SServ} | P4];
                _ ->
                    PF = P4
            end,

            _ = PF,

            case monitor_data_master:get(target, TargetKey) of
                [] ->
                    ?LOG_INFO("get fail", TargetKey);
                [Target] ->
                    #target{properties=OrigP} = Target, 
                    ModifP = lists:foldl(fun({K,V},Acc) ->
                        lists:keystore(K,1,Acc,{K,V})
                    end, OrigP, PF),
                    maybe_update_target(Target, Target#target{properties=ModifP})
            end;
        {error, _Reason} ->
            ?LOG_INFO("snmp fail", {_Reason, TargetKey})
    end.


update_snmp_if_aliases(TargetKey) ->
    IfNames = j_server_snmpman:walk_table(TargetKey, [
        "1.3.6.1.2.1.2.2.1.1",
        "1.3.6.1.2.1.2.2.1.2"
    ]),
    IfAliases = j_server_snmpman:walk_table(TargetKey, [
        "1.3.6.1.2.1.31.1.1.1.1",
        "1.3.6.1.2.1.31.1.1.1.18"
    ]),

    case IfNames of
        {ok, {table, Names}} ->
            P = build_if_names(Names);
        _ ->
            P = []
    end,

    case IfAliases of
        {ok, {table, Aliases}} ->
            P1 = build_if_aliases(Aliases, IfNames),
            PF = lists:append([P,P1]);
        _ ->
            PF = P
    end,

    _ = PF,

    [Target] = monitor_data_master:get(target, TargetKey),
    #target{properties=OrigP} = Target,
    ModifP = lists:foldl(fun({K,V},Acc) ->
       lists:keystore(K,1,Acc,{K,V})
    end, OrigP, PF),
    maybe_update_target(Target, Target#target{properties=ModifP}).


build_if_names(Rows) ->
    build_if_names(Rows, []).
build_if_names([], Acc) -> Acc;
build_if_names([{table_row, _, []}|Rows], Acc) ->
    build_if_names(Rows, Acc);
build_if_names([{table_row, Index, Name}|Rows], Acc) ->
    Key = lists:concat(["ifIndex",Index, "-ifName"]),
    build_if_names(Rows, [{Key, Name}| Acc]).

build_if_aliases(Rows, IfNames) ->
    case IfNames of
        {ok, {table, Names}} ->
            build_if_aliases(Rows, Names, []);
        _ ->
            []
    end.
build_if_aliases([],_,Acc) -> Acc;
build_if_aliases([{table_row, _, []}|Rows], Names, Acc) ->
    build_if_aliases(Rows, Names, Acc);
build_if_aliases([{table_row, Name, Alias}|Rows], Names, Acc) ->
    case lists:keyfind(Name, 3, Names) of
        false ->
            build_if_aliases(Rows, Names, Acc);
        {table_row, Index, _} ->
            Key = lists:concat(["ifIndex", Index, "-ifAlias"]),
            build_if_aliases(Rows, Names, [{Key,Alias}|Acc])
    end.

maybe_update_target(#target{name=_}=Target, #target{name=_}=Target) -> ok;
maybe_update_target(_, Target) ->
    monitor_data_master:update(target, Target).
