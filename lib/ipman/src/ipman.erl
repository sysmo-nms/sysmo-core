% This file is part of "Enms" (http://sourceforge.net/projects/enms/)
% Copyright (C) 2012 <Sébastien Serre sserre.bx@gmail.com>
% 
% Enms is a Network Management System aimed to manage and monitor SNMP
% target, monitor network hosts and services, provide a consistent
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
-module(ipman).
-behaviour(gen_server).
-include("include/ipman.hrl").
-include("../snmp_manager/include/snmp_manager.hrl").

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    start_link/0
]).

start_link() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

% GEN_SERVER
init([]) ->
    Agents = snmp_manager:which_agents(),
    % XXX should call:
    % btracker_probe_standard_snmp:sys_infos/1 and
    AgentsRecords = [
        #ipman_agent{
            agent_name  = Agent,
            sys_infos   = snmp_manager:get_mib2_system(Agent),
            net_infos   = format_ipAddrTable(
                snmp_manager:get_ipAddrTable(Agent)
            )
        } || Agent <- Agents],
    % XXX END

    % filter non router agents:
%     RoutersRecords = lists:filter(
%     fun(#ipman_agent{sys_infos = #mib2_system{sys_services = Services}}) ->
%         case Services of
%             #services{internet = true}  -> true;
%             #services{internet = false} -> false
%     end, AgentsRecords),

    % TODO subscribe to tracker events triggered by 
    % btracker_probe_standard_snmp module
    {ok, AgentsRecords}.

% CALL 
handle_call(_R, _F, S) ->
    {noreply, S}.


% CAST
handle_cast(_,S) ->
    {noreply, S}.


% INFO
handle_info(_, S) ->
    {noreply, S}.



% TERMINATE
terminate(_,_) ->
    ok.


% CHANGE
code_change(_,S,_) ->
    {ok, S}.

% PRIVATE
format_ipAddrTable(Table) ->
    Process = filter_ipAddrTable(Table),
    Result = format_ipAddrTable([], Process),
    ?LOG(Result).
format_ipAddrTable(Records, {[],_,_}) ->
    Records;
format_ipAddrTable(Records, {[Add|Adds],Masks,IfIndexes}) ->
    {_, [1,3,6,1,2,1,4,20,1,1|Rest],_, Address,_} = Add,

    IfOid   = [1,3,6,1,2,1,4,20,1,2 | Rest],
    MaskOid = [1,3,6,1,2,1,4,20,1,3 | Rest],

    {value, MaskFound, Mask2}       = lists:keytake(MaskOid, 2, Masks),
    {value, IfFound, IfIndexes2}    = lists:keytake(IfOid, 2, IfIndexes),

    {_,_,_, Mask,_} = MaskFound,
    {_,_,_, If,  _} = IfFound,

    Record = #ip_addr_entry{
        ip          = Address,
        mask        = Mask,
        if_index    = If
    },

    format_ipAddrTable([Record|Records], {Adds, Mask2, IfIndexes2}).


filter_ipAddrTable(Table) ->
    filter_ipAddrTable({[],[],[]}, Table).
filter_ipAddrTable(Process, []) ->
    Process;
filter_ipAddrTable({Addr, Mask, IfIndex}, [H|Tail]) ->
    {_,Oid,_,_,_} = H,
    case Oid of
        [1,3,6,1,2,1,4,20,1,1|_] ->
            filter_ipAddrTable({[H|Addr],Mask,IfIndex}, Tail);
        [1,3,6,1,2,1,4,20,1,2|_] ->
            filter_ipAddrTable({Addr,Mask,[H|IfIndex]}, Tail);
        [1,3,6,1,2,1,4,20,1,3|_] ->
            filter_ipAddrTable({Addr,[H|Mask],IfIndex}, Tail);
        _ ->
            filter_ipAddrTable({Addr,Mask,IfIndex}, Tail)
    end.
