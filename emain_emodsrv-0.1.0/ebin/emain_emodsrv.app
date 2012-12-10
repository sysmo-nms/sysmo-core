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
{application, emain_emodsrv,
	[
		{description, "Module registration and information"},
		{vsn, "1.0.0"},
		{modules, [
            emodsrv_app,
            emodsrv_sup,
            emodsrv
            ]},
		{registered, [
            emodsrv_sup,
            emodsrv
            ]},
		{applications, [kernel, stdlib]},
		% mandatory
		{mod, {emodsrv_app, []}}
	]
}.
