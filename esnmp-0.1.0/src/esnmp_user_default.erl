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
-module(esnmp_user_default).

-behaviour(snmpm_user).
-include_lib("../include/esnmp.hrl").

-export([handle_error/3,
	 handle_agent/5,
	 handle_pdu/4,
	 handle_trap/3,
	 handle_inform/3,
	 handle_report/3]).

-define(DEFAULT_COMMUNITY, "unknown_agent").

handle_error(_ReqId,  {failed_processing_message, 
    {{securityError,usmStatsUnknownEngineIDs}, _Addr, _Port}}, _UserData) ->
	%io:format("received usmStatsUnknownEngineIDs from ~p port ~p~n", 
    %    [_Addr, _Port]),
    ok;

handle_error(ReqId, Reason, UserData) ->
	info("received handle_error:"
	 "~n   ReqId:    ~p"
	 "~n   Reason:   ~p"
	 "~n   UserData: ~p", [ReqId, Reason, UserData]),
	ignore.


handle_agent(Addr, Port, Type, SnmpInfo, _UserData) ->
    gen_event:notify(esnmp_events, {Type, 
        {v2_community,"unknown_community"}, {Addr, Port}, SnmpInfo}),
	ignore.

handle_pdu(TargetName, ReqId, SnmpResponse, UserData) ->
	info("received handle_pdu:"
	 "~n   TargetName:   ~p"
	 "~n   ReqId:        ~p"
	 "~n   SnmpResponse: ~p"
	 "~n   UserData:     ~p", 
	 [TargetName, ReqId, SnmpResponse, UserData]),
	ignore.


handle_trap(TargetName, SnmpTrap, _UserData) ->
    io:format("ici ~p ~p ~ptrap: ~p~n",[?MODULE,?LINE,TargetName,SnmpTrap]),
	ok.


handle_inform(TargetName, SnmpInform, UserData) ->
	info("received handle_inform:"
	 "~n   TargetName:   ~p"
	 "~n   SnmpInform: ~p"
	 "~n   UserData:   ~p", 
	 [TargetName, SnmpInform, UserData]),
	no_reply.


handle_report(TargetName, SnmpReport, UserData) ->
	info("received handle_inform:"
	 "~n   TargetName:   ~p"
	 "~n   SnmpReport: ~p"
	 "~n   UserData:   ~p", 
	 [TargetName, SnmpReport, UserData]),
	ok.

%info(A) ->
%	error_logger:info_msg(A).

info(F, A) ->
	error_logger:info_msg("SNMPM default user callback " ++ F ++ "~n", A).
