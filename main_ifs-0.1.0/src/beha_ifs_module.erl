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
% @doc ifs_module behaviour
% Pour s'integrer dans le systeme ifs, un module doit exporter un certaines
% fonctions mais aussi se faire connaitre du systeme.
% <h1>Fonctionnement</h1>
% <p>Dans l'ordre voici ce qui ce passe pour qu'un module soit bien enregistré:
% <ul>
% <li>ifs_serveur demare</li>
% <li>le module (modx) implementant beha_ifs_module demare</li>
% <li>modx fait un <em>ifs_server:register_mod({ModuleName, AsnKey})</em>. 
% <b>ModuleName</b> est le nom du module
% sur lequel if_server va effectuer les callbacks, <b>AsnKey</b> est la clé qui permet
% a ifs de correctement router les message provenant des clients (definit dans le fichier asn.1).</li>
% <li>chaque message venant du client est routé selon <b>AsnKey</b> et delivré au module <b>ModuleName</b> via la
% fonction exportée <b>handle_msg/2</b></li>.
% <li><em>modx</em> envoie chaqun de ses evenements au module ifs_rbac 
% via la fonction <b>handle_event/3</b>
% <p><em><b>ifs_rbac:handle_event(ModuleName, Msg, ReadRoles)</b></em> ou
% <ul>
% <li><b>ModuleName</b> est le nom du module enregistre</li>
% <li><b>Msg</b> est un message compris par ifs_encoder</li>
% <li><b>ReadRoles</b> est une liste de roles qui recevront le message</li>
% </ul></p></li>
% </ul></p>
%
% <h1>Export</h1>
% <p><em>handle_msg(<b>Msg, SocketState</b>)</em>.
% <ul>
% <li><b>Msg</b> est le message envoyé par le client definit dans le fichier asn.1</li>
% <li><b>SocketState</b> est un record <em>#client_state</em> definit dans <em>include/client_state.hrl</em></li>
% </ul></p>
% <p><em>initial_conn(<b>SocketState</b>)</em>. Est executé par ifs_server lors de la connexion d'un client.
% Le module peut ainsi initialiser les données client</p>
% <p><em>present(<b>Term</b>)</em> return a valide term() for the asnencoder and a list of allowed read roles.</p>
-module(beha_ifs_module).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{handle_msg, 2}, {initial_conn, 1}, {pre_process, 1}];

behaviour_info(_) ->
    undefined.
