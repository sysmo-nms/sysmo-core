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
-ifndef( _ELDAP_HRL ).
-define( _ELDAP_HRL , 1 ).

%%%
%%% Search input parameters
%%%
-record(eldap_search, {
	  base = [],             % Baseobject
	  filter = [],           % Search conditions
	  scope=wholeSubtree,    % Search scope
	  deref=derefAlways,     % Dereference
	  attributes = [],       % Attributes to be returned
	  types_only = false,    % Return types+values or types
	  timeout = 0            % Timelimit for search
	 }).

%%%
%%% Returned search result
%%%
-record(eldap_search_result, {
	  entries = [],          % List of #eldap_entry{} records
	  referrals = []         % List of referrals
	  }).

%%%
%%% LDAP entry
%%%
-record(eldap_entry, {
	  object_name = "",      % The DN for the entry
	  attributes = []        % List of {Attribute, Value} pairs
	 }).

-endif.
