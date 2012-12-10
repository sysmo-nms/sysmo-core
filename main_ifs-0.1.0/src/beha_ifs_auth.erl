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
%% @doc
%% Un module qui implemente ce behaviour doit exporter une fonction authenticate/2
%% et retourner la liste des roles de l'utilisateur. Un example simple est {@link bifs_auth_ldap. bifs_auth_ldap}.
%% <p> authenticate/2 -> <em>authenticate(Uname, UPass) -> Roles</em></p><br></br>
%% Roles = list of Role,<br></br>
%% Uname = string,<br></br>
%% UPass = string,<br></br>
%% @end
-module(beha_ifs_auth).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{authenticate, 2}];

behaviour_info(_) ->
    undefined.
