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

% PDUS
-module(monitor_pdu).
-include("include/monitor.hrl").
-export([
    'PDU-MonitorPDU-fromServer-infoTarget-create'/1
]).

'PDU-MonitorPDU-fromServer-infoTarget-create'(
        #target{name=Name, properties=Prop}
    ) ->
    AsnProps = lists:foldl(fun({K,V}, Acc) -> 
        [{'Property', K, V} | Acc]
    end, [], Prop),
    {modMonitorPDU,
        {fromServer,
            {infoTarget,
                {'InfoTarget',
                    Name,
                    AsnProps,
                    [],
                    create}}}}.

%'PDU-MonitorPDU-fromServer-infoProbe-create'(
%    #probe{
%        permissions         = #perm_conf{read = R, write = W},
%        monitor_probe_conf  = ProbeConf,
%        description         = Descr,
%        info                = Info
%    } = Probe) ->
%    {modMonitorPDU,
%        {fromServer,
%            {infoProbe,
%                {'InfoProbe',
%                    atom_to_list(TargetId),
%                    atom_to_list(Probe#probe.name),
%                    Descr,
%                    Info,
%                    {'PermConf', R, W},
%                    atom_to_list(Probe#probe.monitor_probe_mod),
%                    gen_asn_probe_conf(ProbeConf),
%                    atom_to_list(Probe#probe.status),
%                    Probe#probe.timeout,
%                    Probe#probe.step,
%                    gen_asn_probe_inspectors(Probe#probe.inspectors),
%                    gen_asn_probe_loggers(Probe#probe.loggers),
%                    gen_asn_probe_properties(Probe#probe.properties),
%                    gen_asn_probe_active(Probe#probe.active),
%                    InfoType
%    }   }   }   },
