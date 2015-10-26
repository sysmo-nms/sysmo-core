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

package io.sysmo.nchecks.checks;

import io.sysmo.nchecks.CheckInterface;
import io.sysmo.nchecks.NChecksSNMP;
import io.sysmo.nchecks.Query;
import io.sysmo.nchecks.Reply;

import io.sysmo.nchecks.Status;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.snmp4j.AbstractTarget;
import org.snmp4j.PDU;
import org.snmp4j.smi.OID;
import org.snmp4j.smi.VariableBinding;
import org.snmp4j.util.TableEvent;
import org.snmp4j.util.TableUtils;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Definition of the check is in the file CheckIfTraffic.xml
 */
public class CheckIfTraffic implements CheckInterface
{
    static Logger logger = LoggerFactory.getLogger(CheckIfTraffic.class);
    private static String IF_INDEX      = "1.3.6.1.2.1.2.2.1.1";
    private static String IF_IN_OCTETS  = "1.3.6.1.2.1.2.2.1.10";
    private static String IF_OUT_OCTETS = "1.3.6.1.2.1.2.2.1.16";

    private static OID[] columns = new OID[]{
            new OID(IF_INDEX),
            new OID(IF_IN_OCTETS),
            new OID(IF_OUT_OCTETS)
    };

    public CheckIfTraffic() {}

    public Reply execute(Query query)
    {
        Reply  reply = new Reply();
        String error = "undefined";
        String ifSelection;
        int warningThreshold;
        int criticalThreshold;

        try {
            ifSelection = query.get("if_selection").asString();
            warningThreshold = query.get("warning_threshold").asInteger();
            criticalThreshold = query.get("critical_threshold").asInteger();
        } catch (Exception|Error e) {
            CheckIfTraffic.logger.error(e.getMessage(), e);
            reply.setStatus(Status.ERROR);
            reply.setReply("Missing or wrong argument: " + e);
            return reply;
        }

        IfTrafficState state = null;
        // Avoid calling query.getState() if not required.
        if (warningThreshold != 0 || criticalThreshold != 0) {
           state = (IfTrafficState) query.getState();
            if (state == null) {
                state = new IfTrafficState();
            }
        }

        HashMap<Integer, Long> newStatusMap = new HashMap<>();
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
            // TODO keep degrade state in reply.setState(v)
            AbstractTarget target = NChecksSNMP.getTarget(query);
            TableUtils tableWalker = NChecksSNMP.getTableUtils(PDU.GETNEXT);
            List<TableEvent> snmpReply = tableWalker.getTable(
                    target, CheckIfTraffic.columns,
                    lowerBoundIndex, upperBoundIndex);

            // TODO check the last element of the list see TableUtils.getTable
            // and TableEvent.getStatus()

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
                    Long octetsIn = vbs[1].getVariable().toLong();
                    Long octetsOut = vbs[2].getVariable().toLong();
                    reply.putPerformance(ifIndex, "IfInOctets", octetsIn);
                    reply.putPerformance(ifIndex, "IfOutOctets", octetsOut);

                    newStatusMap.put(ifIndex, octetsIn);
                }
            }

            String replyMsg;
            Status newStatus;
            // Avoid calling reply.setState() if not required.
            if (state != null) {
                newStatus = state.computeStatusMaps(
                        newStatusMap, warningThreshold, criticalThreshold);

                if (newStatus.equals(Status.OK)) {
                    replyMsg = "CheckIfTraffic OK";
                } else if (newStatus.equals(Status.UNKNOWN)) {
                    replyMsg = "CheckIfTraffic UNKNOWN. No enough data to set sensible status.";
                } else if (newStatus.equals(Status.WARNING)) {
                    replyMsg = "CheckIfTraffic have exceeded WARNING threshold!";
                } else if (newStatus.equals(Status.CRITICAL)) {
                    replyMsg = "CheckIfTraffic have exceeded CRITICAL threshold!";
                } else {
                    replyMsg = "";
                }
                reply.setState(state);
            } else {
                newStatus = Status.OK;
                replyMsg = "CheckIfTraffic OK";
            }

            reply.setStatus(newStatus);
            reply.setReply(replyMsg);
            return reply;
        } catch (Exception|Error e) {
            CheckIfTraffic.logger.error(e.getMessage(), e);
            reply.setStatus(Status.ERROR);
            reply.setReply("Error: " + error);
            return reply;
        }
    }

    static class IfTrafficState implements Serializable {
        private Status status;
        private Date time;
        private HashMap<Integer, Long> data;

        IfTrafficState() {
            this.time = null;
            this.status  = Status.UNKNOWN;
        }

        /**
         * Take two counter entries from different dates, compare to critical
         * and warning values and return the appropriate status.
         * @param update the new value from snmp walk
         * @param warning the warning threshold
         * @param critical the critical threshold
         * @return the new state
         */
        public Status computeStatusMaps(HashMap<Integer,Long> update,
                int warning, int critical)
        {
            if (this.time == null) {
                // this is the first compute statusMap, nothing to compare
                this.time = new Date();
                this.data = update;
                return this.status;
            }

            // get the minutes diff from last walk
            Date newDate = new Date();
            Date oldDate = this.time;
            long seconds = (newDate.getTime() - oldDate.getTime()) / 1000;
            long minutes;

            boolean keepWorstState;
            if (seconds < 60) {
                keepWorstState = true;
                // no enough time elapsed return old status
                //keepWorstState = true;
                minutes = 1;
            } else {
                keepWorstState = false;
                minutes = seconds / 60;
            }


            Status newStatus = Status.OK;
            // if one of the key reach threshold value set the new status.
            for (Map.Entry<Integer, Long> entry: update.entrySet())
            {
                Integer key = entry.getKey();
                Long upd = entry.getValue();
                Long old = this.data.get(key);
                if (old != null) {
                    long diff = (upd - old) / minutes;
                    if (warning != 0) {
                        if (diff > warning) {
                            newStatus = Status.WARNING;
                        }
                    }
                    if (critical != 0) {
                        if (diff > critical) {
                            newStatus = Status.CRITICAL;
                        }
                    }
                }
            }

            if (keepWorstState) {
                switch (this.status.compareTo(newStatus)) {
                    case 1:
                        // this.status is superior keep it and forget update
                        return this.status;
                    default:
                        // this.status is equal or inferior change it but stay
                        // with the old update time and update
                        this.status = newStatus;
                        return this.status;
                }
            } else {
                this.time = newDate;
                this.data = update;
                this.status = newStatus;
                return this.status;
            }
        }
    }
}
