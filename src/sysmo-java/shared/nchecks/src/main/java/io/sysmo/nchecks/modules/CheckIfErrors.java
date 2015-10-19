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

import io.sysmo.nchecks.HelperInterface;
import io.sysmo.nchecks.HelperReply;
import io.sysmo.nchecks.NChecksInterface;
import io.sysmo.nchecks.NChecksSNMP;
import io.sysmo.nchecks.Query;
import io.sysmo.nchecks.Reply;
import io.sysmo.nchecks.helpers.GetIfTableHelper;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snmp4j.AbstractTarget;
import org.snmp4j.PDU;
import org.snmp4j.Snmp;
import org.snmp4j.smi.OID;
import org.snmp4j.smi.VariableBinding;
import org.snmp4j.util.DefaultPDUFactory;
import org.snmp4j.util.TableEvent;
import org.snmp4j.util.TableUtils;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

/**
 * Definition of the check is in the file CheckIfErrors.xml
 */
public class CheckIfErrors implements NChecksInterface, HelperInterface
{
    static Logger logger = LoggerFactory.getLogger(CheckIfErrors.class);
    private static String IF_INDEX      = "1.3.6.1.2.1.2.2.1.1";
    private static String IF_IN_ERRORS  = "1.3.6.1.2.1.2.2.1.14";
    private static String IF_OUT_ERRORS = "1.3.6.1.2.1.2.2.1.20";

    private static OID[] columns = new OID[]{
            new OID(IF_INDEX),
            new OID(IF_IN_ERRORS),
            new OID(IF_OUT_ERRORS)
    };

    public CheckIfErrors() {}

    public Reply execute(Query query)
    {
        Reply  reply = new Reply();
        String error = "undefined";
        String ifSelection;

        try {
            ifSelection = query.get("if_selection").asString();
        } catch (Exception|Error e) {
            CheckIfErrors.logger.error(e.getMessage(), e);
            reply.setStatus(Reply.STATUS_ERROR);
            reply.setReply("Missing or wrong argument: " + e);
            return reply;
        }

        try {
            AbstractTarget target = NChecksSNMP.getInstance().getTarget(query);

            Snmp session = NChecksSNMP.getInstance().getSnmpSession();

            // TODO try PDU.GETBULK then PDU.GETNEXT to degrade....
            // TODO keep degrade state in reply.setOpaqueData(v)
            TableUtils tableWalker =
                    new TableUtils(
                            session,
                            new DefaultPDUFactory(PDU.GETNEXT));


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

            List<TableEvent> snmpReply = tableWalker.getTable(
                    target, CheckIfErrors.columns,
                    lowerBoundIndex, upperBoundIndex);

            // TODO degrade to PDU.GETNEXT if some vb(s) == null
            // TODO check if reply is valid. Where is is the error status?

            // asList for List.contains
            List<Integer> intList = Arrays.asList(indexesArrayInt);
            Iterator<TableEvent> it = snmpReply.iterator();
            TableEvent evt;
            while (it.hasNext()) {
                evt = it.next();
                error = evt.getErrorMessage();
                VariableBinding[]   vbs = evt.getColumns();
                Integer ifIndex = vbs[0].getVariable().toInt();

                if (intList.contains(ifIndex)) {
                    reply.putPerformance(ifIndex, "IfInErrors",
                            vbs[1].getVariable().toLong());
                    reply.putPerformance(ifIndex, "IfOutErrors",
                            vbs[2].getVariable().toLong());
                }
            }


            reply.setStatus(Reply.STATUS_OK);
            reply.setReply("CheckIfErrors successful");
            return reply;
        } catch (Exception|Error e) {
            CheckIfErrors.logger.error(e.getMessage(), e);
            reply.setStatus(Reply.STATUS_ERROR);
            reply.setReply("Error: " + error);
            return reply;
        }
    }

    /*
     * Helper interface
     */
    public HelperReply callHelper(Query query, String id)
    {
        GetIfTableHelper helper = new GetIfTableHelper();
        return helper.call(query);
    }

    static class CheckIfErrorsState implements Serializable {

    }
}
