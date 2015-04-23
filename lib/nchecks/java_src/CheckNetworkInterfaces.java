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

package io.sysmo.nchecks.checks;

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
import java.util.Arrays;
import java.util.List;
import java.util.Iterator;


public class CheckNetworkInterfaces implements NChecksInterface
{
    private static String IF_INDEX = "1.3.6.1.2.1.2.2.1.1";
    private static String IF_IN_OCTETS = "1.3.6.1.2.1.2.2.1.10";
    private static String IF_IN_UCASTPKTS = "1.3.6.1.2.1.2.2.1.11";
    private static String IF_IN_NUCASTPKTS = "1.3.6.1.2.1.2.2.1.12";
    private static String IF_IN_ERRORS = "1.3.6.1.2.1.2.2.1.14";
    private static String IF_OUT_OCTETS = "1.3.6.1.2.1.2.2.1.16";
    private static String IF_OUT_UCASTPKTS = "1.3.6.1.2.1.2.2.1.17";
    private static String IF_OUT_NUCASTPKTS = "1.3.6.1.2.1.2.2.1.18";
    private static String IF_OUT_ERRORS = "1.3.6.1.2.1.2.2.1.20";

    private static OID[] columns = new OID[]{
        new OID(IF_INDEX),
            new OID(IF_IN_OCTETS),
            new OID(IF_IN_UCASTPKTS),
            new OID(IF_IN_NUCASTPKTS),
            new OID(IF_IN_ERRORS),
            new OID(IF_OUT_OCTETS),
            new OID(IF_OUT_UCASTPKTS),
            new OID(IF_OUT_NUCASTPKTS),
            new OID(IF_OUT_ERRORS)
    };

    private String  ifSelection;

    private Map<String,Argument> conf;

    public CheckNetworkInterfaces()
    {
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
    }

    public Reply execute()
    {
        Reply  reply = new Reply();
        String error = "undefined";

        try {
            AbstractTarget target = NChecksSNMP.getTarget(conf);
            System.out.println("snmptarget? " + target);

            Snmp session = NChecksSNMP.getSnmpSession();
            System.out.println("snmpsession? " + session);


            // TODO try PDU.GETBULK then PDU.GETNEXT to degrade....
            // TODO keep degrade state in reply.setOpaqueData(v)
            TableUtils tablewalker = 
                new TableUtils(
                        session,
                        new DefaultPDUFactory(PDU.GETNEXT));

            System.out.println("tableutils?" + tablewalker);

            // TODO set lower and upper bound indexes
            List<TableEvent> snmpReply = tablewalker.getTable(
                    target,
                    columns,
                    null,
                    null);

            // TODO degrade to PDU.GETNEXT if some vb(s) == null
            // TODO check if reply is valid. Whereis is the error status?

            String[]     indexesArray = ifSelection.split(",");
            List<String> indexesList  = Arrays.asList(indexesArray);

            Iterator<TableEvent> it = snmpReply.iterator();
            TableEvent evt;
            while (it.hasNext()) {
                evt = it.next();
                error = evt.getErrorMessage();
                VariableBinding[]   vbs = evt.getColumns();
                String ifIndex = vbs[0].getVariable().toString();
                if (indexesList.contains(ifIndex) == false) continue;
                reply.putPerformance(ifIndex,"IfInOctets",
                        vbs[1].getVariable().toLong());
                reply.putPerformance(ifIndex,"IfInUcastPkts",
                        vbs[2].getVariable().toLong());
                reply.putPerformance(ifIndex,"IfInNucastPkts",
                        vbs[3].getVariable().toLong());
                reply.putPerformance(ifIndex,"IfInErrors",
                        vbs[4].getVariable().toLong());

                reply.putPerformance(ifIndex,"IfOutOctets",
                        vbs[5].getVariable().toLong());
                reply.putPerformance(ifIndex,"IfOutUcastPkts",
                        vbs[6].getVariable().toLong());
                reply.putPerformance(ifIndex,"IfOutNucastPkts",
                        vbs[7].getVariable().toLong());
                reply.putPerformance(ifIndex,"IfOutErrors",
                        vbs[8].getVariable().toLong());
            }


            reply.setStatus(Const.STATUS_OK);
            reply.setReply("IfPerTableTest success fetch for: " + ifSelection);
            return reply;
        } catch (Exception|Error e) {
            e.printStackTrace();
            reply.setStatus(Const.STATUS_ERROR);
            reply.setReply("Error: " + error);
            return reply;
        }
    }
}
