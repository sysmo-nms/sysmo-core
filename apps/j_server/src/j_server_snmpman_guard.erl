%%=
% Copyright (C) 2014, Sebastien Serre <sserre.bx@gmail.com>
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in all
% copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
% THE SOFTWARE.
%%=
% @private
-module(j_server_snmpman_guard).
-include("snmpman.hrl").
-export([validate_discovery_conf/4, validate_register_conf/1, validate_oids/1]).

validate_oids(_) -> true.

validate_register_conf(ElementConf) ->
    vrc0(ElementConf).
vrc0(#element_def{name = Name} = EC) ->
    case is_string(Name) of
        true -> vrc1(EC);
        false -> {error, "Bad value for element name"}
    end.
vrc1(#element_def{host = ""}) -> {error, "Host undefined"};
vrc1(#element_def{host = Host} = EC) when is_list(Host) ->
        vrc2(EC).
vrc2(#element_def{port = Port} = EC) ->
    case is_network_port(Port) of
        true -> vrc3(EC);
        false -> {error, "Wrong network port"}
    end.
vrc3(#element_def{snmp_version = SnmpV} = EC) ->
    case is_snmpversion(SnmpV) of
        true -> vrc4(EC);
        false -> {error, "Bad snmp version"}
    end.
vrc4(#element_def{sec_level = SecL} = EC) ->
    case is_seclevel(SecL) of
        true -> vrc5(EC);
        false -> {error, "Bad security level"}
    end.
vrc5(#element_def{retries = Retries} = EC) ->
    case is_integer(Retries) of
        true -> vrc6(EC);
        false -> {error, "Bad retries value"}
    end.
vrc6(#element_def{timeout = Timeout} = EC) ->
    case is_integer(Timeout) of
        true -> vrc7(EC);
        false -> {error, "Bad timeout value"}
    end.
vrc7(#element_def{sec_name = SecName} = EC) ->
    % TODO verify type accepted
    case is_string(SecName) of
        true -> vrc8(EC);
        false -> {error, "Bad security name value"}
    end.
vrc8(#element_def{community = Community} = EC) ->
    % TODO verify type accepted
    case is_string(Community) of
        true -> vrc9(EC);
        false -> {error, "Bad community value"}
    end.
vrc9(#element_def{auth_proto = AuthProto} = EC) ->
    case is_authproto(AuthProto) of
        true -> vrc10(EC);
        false -> {error, "Bad auth protocol value"}
    end.
vrc10(#element_def{auth_key = AuthKey} = EC) ->
    % TODO verify type accepted
    case is_valid_key(AuthKey) of
        true -> vrc11(EC);
        false -> {error, "Bad auth key value"}
    end.
vrc11(#element_def{priv_proto = PrivProto} = EC) ->
    case is_privproto(PrivProto) of
        true -> vrc12(EC);
        false -> {error, "Bad priv protocol"}
    end.
vrc12(#element_def{priv_key = PrivKey}) ->
    case is_valid_key(PrivKey) of
        true -> true;
        false -> {error, "Bad priv key value"}
    end.

validate_discovery_conf(Ip, IpVer, Port, Timeout) ->
    case is_network_port(Port) of
        true ->
            case is_ipdef(IpVer, Ip) of
                true ->
                    case is_integer(Timeout) of
                        true -> true;
                        false ->
                            {error, "Bad timeout value"}
                    end;
                false -> {error, "Bad ip, ip version value"}
            end;
        false -> {error, "Bad network port value"}
    end.

is_privproto("AES")         -> true;
is_privproto("AES192")      -> true;
is_privproto("AES256")      -> true;
is_privproto("DES")         -> true;
is_privproto("3DES")        -> true;
is_privproto("AES192_3DES") -> true;
is_privproto("AES256_3DES") -> true;
is_privproto(_)             -> false.

is_authproto("MD5") -> true;
is_authproto("SHA") -> true;
is_authproto(_)     -> false.

is_seclevel("authPriv")     -> true;
is_seclevel("authNoPriv")   -> true;
is_seclevel("noAuthNoPriv") -> true;
is_seclevel(_)              -> false.

is_snmpversion("1")     -> true;
is_snmpversion("2c")    -> true;
is_snmpversion("3")     -> true;
is_snmpversion(_)       -> false.

is_valid_key(Key) ->
    case is_string(Key) of
        true ->
            case length(Key) of
                N when N >= 8 -> true;
                _ -> false
            end;
        false -> false
    end.

is_network_port(Port) ->
    case is_integer(Port) of
        true ->
            case (Port > 0) and (Port =< 65536) of
                true  -> true;
                false -> false
            end;
        false -> false
    end.

is_string([]) -> false;
is_string(V) when is_list(V) ->
    case io_lib:printable_unicode_list(V) of
        true -> true;
        false -> false
    end;
is_string(_) -> false.

is_ipdef("v4", Ip) ->
    case is_string(Ip) of
        true  ->
            case inet:parse_ipv4_address(Ip) of
                {ok, _} -> true;
                _ -> false
            end;
        false -> false
    end;
is_ipdef("v6", Ip) ->
    case is_string(Ip) of
        true  ->
            case inet:parse_ipv6_address(Ip) of
                {ok, _} -> true;
                _ -> false
            end;
        false -> false
    end;
is_ipdef(_,_) -> false.

% is_printable_string([]) -> true;
% is_printable_string([H|T]) when (H > 95) and (H < 123) -> % a-z
%     is_printable_string(T);
% is_printable_string([H|T]) when (H > 64) and (H < 91) -> % A-Z
%     is_printable_string(T);
% is_printable_string([H|T]) when (H > 42) and (H < 58) -> % +,,,-,.,/,0-9
%     is_printable_string(T);
% is_printable_string([H|T]) when (H == 32) or (H == 39) -> % SPACE,'
%     is_printable_string(T);
% is_printable_string([H|T]) when (H == 28) or (H == 29) -> % (,)
%     is_printable_string(T);
% is_printable_string([H|T]) when (H == 58) or (H == 61) -> % :,=
%     is_printable_string(T);
% is_printable_string([H|T]) when (H == 63) -> % ?
%     is_printable_string(T);
% is_printable_string(_) -> false.
