/*
 * Sysmo NMS Network Management and Monitoring solution (http://www.sysmo.io)
 *
 * Copyright (c) 2012-2015 Sebastien Serre <ssbx@sysmo.io>
 *
 * This file is part of Sysmo NMS.
 *
 * Sysmo NMS is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Sysmo NMS is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Sysmo.  If not, see <http://www.gnu.org/licenses/>.
 */

package io.sysmo.nchecks.modules;

import io.sysmo.nchecks.NChecksInterface;
import io.sysmo.nchecks.Reply;
import io.sysmo.nchecks.Query;
import io.sysmo.nchecks.NChecksSNMP;

import io.sysmo.nchecks.Status;
import org.snmp4j.AbstractTarget;
import org.snmp4j.util.TableUtils;
import org.snmp4j.util.TableEvent;
import org.snmp4j.smi.OID;
import org.snmp4j.smi.VariableBinding;
import org.snmp4j.PDU;

import java.util.Arrays;
import java.util.List;
import java.util.Iterator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CheckNetworkInterfaces implements NChecksInterface
{
    static Logger logger =
            LoggerFactory.getLogger(CheckNetworkInterfaces.class);
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

    public CheckNetworkInterfaces() {}

    public Reply execute(Query query)
    {
        Reply  reply = new Reply();
        String error = "undefined";
        String ifSelection;

        try {
            ifSelection = query.get("if_selection").asString();
        } catch (Exception|Error e) {
            CheckNetworkInterfaces.logger.error(e.getMessage(), e);
            reply.setStatus(Status.ERROR);
            reply.setReply("Missing or wrong argument: " + e);
            return reply;
        }

        try {

            // get indexes string list
            String[] indexesArrayString = ifSelection.split(",");

            // transform to index array int
            Integer[] indexesArrayInt = new Integer[indexesArrayString.length];
            for (int i = 0; i < indexesArrayString.length; i++) {
                indexesArrayInt[i] = Integer.parseInt(indexesArrayString[i]);
            }

            // sort it
            Arrays.sort(indexesArrayInt);

            // build upper and lower bound indexes
            Integer lower = indexesArrayInt[0] - 1;
            Integer upper = indexesArrayInt[indexesArrayInt.length - 1];
            OID lowerBoundIndex = new OID(lower.toString());
            OID upperBoundIndex = new OID(upper.toString());

            // TODO try PDU.GETBULK then PDU.GETNEXT to degrade....
            // TODO keep degrade state in reply.setOpaqueData(v)
            AbstractTarget target = NChecksSNMP.getTarget(query);
            TableUtils tableWalker = NChecksSNMP.getTableUtils(PDU.GETNEXT);
            List<TableEvent> snmpReply = tableWalker.getTable(
                    target, CheckNetworkInterfaces.columns,
                    lowerBoundIndex, upperBoundIndex);

            // TODO degrade to PDU.GETNEXT if some vb(s) == null
            // TODO check if reply is valid. Where is is the error status?

            List<Integer> intList = Arrays.asList(indexesArrayInt);
            Iterator<TableEvent> it = snmpReply.iterator();
            TableEvent evt;
            while (it.hasNext()) {
                evt = it.next();
                error = evt.getErrorMessage();
                VariableBinding[]   vbs = evt.getColumns();
                Integer ifIndex = vbs[0].getVariable().toInt();

                if (intList.contains(ifIndex)) {
                    reply.putPerformance(ifIndex, "IfInOctets",
                            vbs[1].getVariable().toLong());
                    reply.putPerformance(ifIndex, "IfInUcastPkts",
                            vbs[2].getVariable().toLong());
                    reply.putPerformance(ifIndex, "IfInNucastPkts",
                            vbs[3].getVariable().toLong());
                    reply.putPerformance(ifIndex, "IfInErrors",
                            vbs[4].getVariable().toLong());

                    reply.putPerformance(ifIndex, "IfOutOctets",
                            vbs[5].getVariable().toLong());
                    reply.putPerformance(ifIndex, "IfOutUcastPkts",
                            vbs[6].getVariable().toLong());
                    reply.putPerformance(ifIndex, "IfOutNucastPkts",
                            vbs[7].getVariable().toLong());
                    reply.putPerformance(ifIndex, "IfOutErrors",
                            vbs[8].getVariable().toLong());
                }
            }


            reply.setStatus(Status.OK);
            reply.setReply("IfPerTableTest success fetch for: " + ifSelection);
            return reply;
        } catch (Exception|Error e) {
            CheckNetworkInterfaces.logger.error(e.getMessage(), e);
            reply.setStatus(Status.ERROR);
            reply.setReply("Error: " + error);
            return reply;
        }
    }
}
