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
import io.sysmo.nchecks.NChecksSNMP;

import org.snmp4j.Snmp;
import org.snmp4j.AbstractTarget;
import org.snmp4j.util.TableUtils;
import org.snmp4j.util.TableEvent;
import org.snmp4j.util.DefaultPDUFactory;
import org.snmp4j.smi.OID;
import org.snmp4j.smi.VariableBinding;
import org.snmp4j.PDU;

import java.util.Map;
import java.util.List;
import java.util.Iterator;


public class CheckNetworkInterfaces implements NChecksInterface
{
    private static OID[] oids = new OID[]{
        new OID("1.3.6.1.2.1.2.2.1.1"),
        new OID("1.3.6.1.2.1.2.2.1.10"),
        new OID("1.3.6.1.2.1.2.2.1.11"),
        new OID("1.3.6.1.2.1.2.2.1.12"),
        new OID("1.3.6.1.2.1.2.2.1.14"),
        new OID("1.3.6.1.2.1.2.2.1.16"),
        new OID("1.3.6.1.2.1.2.2.1.17"),
        new OID("1.3.6.1.2.1.2.2.1.18"),
        new OID("1.3.6.1.2.1.2.2.1.20")
    };

    private String  ifSelection;

    private Map<String,Argument> conf;

    public CheckNetworkInterfaces()
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
        ifSelection = config.get("if_selection").getStr();
        /*
        because it is defined as a "snmp" type Check (xml), config contain
        all elements needed to register the agent to query.
        */
        conf = config;
        System.out.println("setconfig??");
    }

    public Reply execute()
    {
        System.out.println("execute??");

        AbstractTarget target;
        try {
            target = NChecksSNMP.getTarget(conf);
            System.out.println("snmptarget? " + target);
        } catch (Exception e) {
            e.printStackTrace();
            Reply rep = new Reply();
            rep.setStatus(Const.STATUS_ERROR);
            rep.setReply("Error: " + e);
            return rep;
        }

        Snmp session = NChecksSNMP.getSnmpSession();
        System.out.println("snmpsession? " + session);


        // !!! PDU.GETNEXT to degrade....
        TableUtils tablewalker = new TableUtils(session,
                                                //new DefaultPDUFactory(PDU.GETBULK));
                                                new DefaultPDUFactory(PDU.GETNEXT));

        System.out.println("tableutils?" + tablewalker);

        OID ifIndex = new OID("1.3.6.1.2.1.2.2.1.1");
        OID ifDescr = new OID("1.3.6.1.2.1.2.2.1.2");
        OID[] columns = new OID[]{ifIndex, ifDescr};
        System.out.println("columns?" + columns);

        List<TableEvent> snmpReply =
                   tablewalker.getTable(target, oids, null, null);
        System.out.println("snmpReply?" + snmpReply);

        Iterator<TableEvent> it = snmpReply.iterator();

        // degrade to PDU.GETNEXT if some vb(s) == null
        // TODO whereis is the error status?
        while (it.hasNext()) {
            TableEvent evt = it.next();
            System.out.println("event: is error?" + evt.isError());
            System.out.println("event: get error" + evt.getErrorMessage());
            System.out.println("event: get except" + evt.getException());
            System.out.println("event: get status" + evt.getStatus());
            VariableBinding[] vbs = evt.getColumns();
            for(VariableBinding vb: vbs) {
                System.out.println("vb: " + vb + " ");
            }
        }

        Reply reply = new Reply();
        String[] indexes = ifSelection.split(",");
        for (String i: indexes)
        {
            reply.putPerformance(i, "IfInOctets",       500);
            reply.putPerformance(i, "IfInUcastPkts",    500);
            reply.putPerformance(i, "IfInNucastPkts",   500);
            reply.putPerformance(i, "IfInErrors",       500);

            reply.putPerformance(i, "IfOutOctets",      500);
            reply.putPerformance(i, "IfOutUcastPkts",   500);
            reply.putPerformance(i, "IfOutNucastPkts",  500);
            reply.putPerformance(i, "IfOutErrors",      500);
        }
        reply.setStatus(Const.STATUS_OK);
        reply.setReply("IfPerTableTest success fetch for: " + ifSelection);
        return reply;
    }
}
