/* Copyright (C) 2014, Sebastien Serre <sserre.bx@gmail.com>
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package io.sysmo.nchecks.modules;

import io.sysmo.nchecks.NChecksInterface;
import io.sysmo.nchecks.Argument;
import io.sysmo.nchecks.Reply;
import io.sysmo.nchecks.Const;

import java.util.Map;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.net.Socket;
import java.net.InetSocketAddress;
import java.net.InetAddress;


public class CheckSNMPIfPerformances implements NChecksInterface
{
    private String  ifSelection;

    private Integer snmpPort;
    private String  snmpVersion;
    private String  snmpSeclevel;
    private String  snmpCommunity;
    private String  snmpUsmUser;
    private String  snmpAuthKey;
    private String  snmpAuthProto;
    private String  snmpPrivKey;
    private String  snmpPrivProto;
    private Integer snmpTimeout;
    private Integer snmpRetries;

    public CheckSNMPIfPerformances()
    {
        System.out.println("init CheckIfPerformances");
    }

    public void setOpaqueData(byte[] opaqueData)
    {
        /* DESERIALIZATION EXAMPLE
        ByteArrayInputStream b = new ByteArrayInputStream(opaqueData);
        ObjectInputStream o = new ObjectInputStream(b);

        Object myObject = o.readObject();
            or beter
        MyObjectClass = (MyObjectClass) o.readObject();
        */

        /* SERICALIZATION EXAMPLE
        ByteArrayOutputStream b = new ByteArrayOutputStream();
        ObjectOutputStream o = new ObjectOutputStream(b);
        o.writeObject(myObject);
        opaqueData = b.toByteArray();
        */
    }

    public void setConfig(Map<String,Argument> config)
    {
        this.ifSelection    = config.get("if_selection").getStr();
        this.snmpPort       = config.get("snmp_port").getInt();
        this.snmpVersion    = config.get("snmp_version").getStr();
        this.snmpSeclevel   = config.get("snmp_seclevel").getStr();
        this.snmpCommunity  = config.get("snmp_community").getStr();
        this.snmpUsmUser    = config.get("snmp_usm_user").getStr();
        this.snmpAuthKey    = config.get("snmp_authkey").getStr();
        this.snmpAuthProto  = config.get("snmp_authproto").getStr();
        this.snmpPrivKey    = config.get("snmp_privkey").getStr();
        this.snmpPrivProto  = config.get("snmp_privproto").getStr();
        this.snmpTimeout    = config.get("snmp_timeout").getInt();
        this.snmpRetries    = config.get("snmp_retries").getInt();
    }

    public Reply execute()
    {
        Reply reply = new Reply();
        String[] indexes = ifSelection.split(",");
        for (String i: indexes)
        {
            reply.putPerformance(i, "OctetsIn",             500);
            reply.putPerformance(i, "UnicastPacketsIn",     500);
            reply.putPerformance(i, "NonUnicastPacketsIn",  500);
            reply.putPerformance(i, "ErrorsIn",             500);

            reply.putPerformance(i, "OctetsOut",            500);
            reply.putPerformance(i, "UnicastPacketsOut",    500);
            reply.putPerformance(i, "NonUnicastPacketsOut", 500);
            reply.putPerformance(i, "ErrorsOut",            500);
        }
        reply.setStatus(Const.STATUS_OK);
        reply.setReply("IfPerTableTest success fetch for: " + ifSelection);
        return reply;
    }
}
