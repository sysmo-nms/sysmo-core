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
 * Definition of the check is in the file CheckIfErrors.xml
 */
public class CheckIfErrors implements NChecksInterface
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
        int warningThreshold;
        int criticalThreshold;

        try {
            ifSelection = query.get("if_selection").asString();
            warningThreshold = query.get("warning_threshold").asInteger();
            criticalThreshold = query.get("critical_threshold").asInteger();
        } catch (Exception|Error e) {
            CheckIfErrors.logger.error(e.getMessage(), e);
            reply.setStatus(Status.ERROR);
            reply.setReply("Missing or wrong argument: " + e);
            return reply;
        }

        IfErrorsState state = (IfErrorsState) query.getState();
        if (state == null) {
            state = new IfErrorsState();
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
            TableUtils tableWalker = NChecksSNMP.getTableUtils(state.getPduType());
            List<TableEvent> snmpReply = tableWalker.getTable(
                    target, CheckIfErrors.columns,
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
                    Long errsIn = vbs[1].getVariable().toLong();
                    Long errsOut = vbs[2].getVariable().toLong();
                    reply.putPerformance(ifIndex, "IfInErrors", errsIn);
                    reply.putPerformance(ifIndex, "IfOutErrors", errsOut);

                    newStatusMap.put(ifIndex, errsIn + errsOut);
                }
            }

            Status newStatus = state.computeStatusMaps(
                    newStatusMap, warningThreshold, criticalThreshold);

            String replyMsg;
            if (newStatus.equals(Status.OK)) {
                replyMsg = "CheckIfErrors OK";
            } else if (newStatus.equals(Status.UNKNOWN)) {
                replyMsg = "CheckIfErrors UNKNOWN. No enough data to compute thresholds";
            } else if (newStatus.equals(Status.WARNING)) {
                replyMsg = "CheckIfErrors WARNING have found errors!";
            } else if (newStatus.equals(Status.CRITICAL)) {
                replyMsg = "CheckIfErrors CRITICAL have found errors!";
            } else {
                replyMsg = "";
            }

            reply.setState(state);
            reply.setStatus(newStatus);
            reply.setReply(replyMsg);
            return reply;
        } catch (Exception|Error e) {
            CheckIfErrors.logger.error(e.getMessage(), e);
            reply.setStatus(Status.ERROR);
            reply.setReply("Error: " + error);
            return reply;
        }
    }

    static class IfErrorsState implements Serializable {
        private int pduType;
        private Status status;
        private Date time;
        private HashMap<Integer, Long> data;

        IfErrorsState() {
            this.time = null;
            this.pduType = PDU.GETNEXT;
            this.status  = Status.UNKNOWN;
        }

        public int getPduType() {
            return this.pduType;
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
            //boolean keepWorstState = false;
            if (seconds < 60) {
                // no enough time elapsed return old status
                // TODO check and return worst status
                //keepWorstState = true;
                //minutes = 1;
                return this.status;
            } else {
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
                    if (diff > warning) {
                        newStatus = Status.WARNING;
                    }
                    if (diff > critical) {
                        newStatus = Status.CRITICAL;
                    }
                }
            }

            this.time = newDate;
            this.data = update;
            this.status = newStatus;
            return this.status;

            /* TODO test worst state if no enough data
            // keep old worst state if the date diff is under 60 seconds, but
            // change the state to a new worst status if thresholds are reached.
            if (keepWorstState) {
                switch (status) {
                    case Reply.CRITICAL:
                        // it is the worst
                        this.status = status;
                        break;
                    case Reply.WARNING:
                        // is it the worst?
                        if (!this.status.equals(Reply.CRITICAL)) {
                            // yes go warning
                            this.status = status;
                        }
                        break;
                    case Reply.OK:
                        // is it the worst?
                        switch(this.status) {
                            case Reply.CRITICAL:
                                // no stay with our critical
                                break;
                            case Reply.WARNING:
                                // no stay with our warning
                                break;
                            case Reply.UNKNOWN:
                                // no stay with our unknown
                                break;
                            default:
                                // then go ok
                                this.status = status;
                                break;
                        }
                        break;
                }
            } else {
                this.time = newDate;
                this.data = update;
                this.status = status;
            }
        return this.status;
            */
        }
    }
}
