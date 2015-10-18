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

package io.sysmo.nchecks.helpers;

import io.sysmo.nchecks.HelperReply;
import io.sysmo.nchecks.HelperSimpleReply;
import io.sysmo.nchecks.NChecksSNMP;
import io.sysmo.nchecks.HelperTableReply;
import io.sysmo.nchecks.HelperTableRow;
import io.sysmo.nchecks.Query;

import org.snmp4j.AbstractTarget;
import org.snmp4j.PDU;
import org.snmp4j.Snmp;
import org.snmp4j.smi.OID;
import org.snmp4j.smi.VariableBinding;
import org.snmp4j.util.DefaultPDUFactory;
import org.snmp4j.util.TableEvent;
import org.snmp4j.util.TableUtils;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Created by seb on 18/10/15.
 */
public class GetIfTableHelper {

    private static final String IF_INDEX       = "1.3.6.1.2.1.2.2.1.1";
    private static final String IF_DESCR       = "1.3.6.1.2.1.2.2.1.2";
    private static final String IF_TYPE        = "1.3.6.1.2.1.2.2.1.3";
    private static final String IF_PHYSADDRESS = "1.3.6.1.2.1.2.2.1.6";

    private static final OID[] columns = new OID[]{
            new OID(IF_INDEX),
            new OID(IF_DESCR),
            new OID(IF_TYPE),
            new OID(IF_PHYSADDRESS)
    };

    private static final Map<String, String> iftype;
    static
    {
        iftype = new HashMap<>();
        iftype.put("1", "other");
        iftype.put("2", "regular1822");
        iftype.put("3", "hdh1822");
        iftype.put("4", "ddn-x25");
        iftype.put("5", "rfc877-x25");
        iftype.put("6", "ethernet-csmacd");
        iftype.put("7", "iso88023-csmacd");
        iftype.put("8", "iso88024-tokenBus");
        iftype.put("9", "iso88025-tokenRing");
        iftype.put("10", "iso88026-man");
        iftype.put("11", "starLan");
        iftype.put("12", "proteon-10Mbit");
        iftype.put("13", "proteon-80Mbit");
        iftype.put("14", "hyperchannel");
        iftype.put("15", "fddi");
        iftype.put("16", "lapb");
        iftype.put("17", "sdlc");
        iftype.put("18", "ds1");
        iftype.put("19", "e1");
        iftype.put("20", "basicISDN");
        iftype.put("21", "primaryISDN");
        iftype.put("22", "propPointToPointSerial");
        iftype.put("23", "ppp");
        iftype.put("24", "softwareLoopback");
        iftype.put("25", "eon");
        iftype.put("26", "ethernet-3Mbit");
        iftype.put("27", "nsip");
        iftype.put("28", "slip");
        iftype.put("29", "ultra");
        iftype.put("30", "ds3");
        iftype.put("31", "sip");
        iftype.put("32", "frame-relay");
    }

    public GetIfTableHelper() {}

    public HelperReply call(Query query)
    {
        try {
            AbstractTarget target = NChecksSNMP.getInstance().getTarget(query);

            Snmp session = NChecksSNMP.getInstance().getSnmpSession();
            TableUtils tableWalker =
                new TableUtils(
                        session,
                        new DefaultPDUFactory(PDU.GETNEXT));

            List<TableEvent> snmpReply = tableWalker.getTable(
                    target, columns, null, null);

            Iterator<TableEvent> it = snmpReply.iterator();
            TableEvent evt;

            HelperTableReply table = new HelperTableReply();
            while (it.hasNext()) {
                evt = it.next();
                VariableBinding[]   vbs = evt.getColumns();
                HelperTableRow row = new HelperTableRow();
                row.addItem("ifIndex", vbs[0].getVariable().toString());
                row.addItem("ifDescr", vbs[1].getVariable().toString());
                row.addItem("ifType", getType(vbs[2].getVariable().toString()));
                row.addItem("ifPhysAddress", vbs[3].getVariable().toString());
                table.addRow(row);
            }
            table.setId("SelectNetworkInterfaces");
            table.setStatus(HelperReply.SUCCESS);
            table.setMessage("Please select interfaces you want to monitor:");
            table.setTreeRoot("ifType");
            table.setSelectColumn("ifIndex");
            table.setSelectType(HelperTableReply.SELECT_MULTIPLE);
            table.setListSeparator(",");
            return table;

        } catch (Exception|Error e) {
            HelperSimpleReply simple = new HelperSimpleReply();
            simple.setId("SelectNetworkInterfaces");
            simple.setStatus(HelperReply.FAILURE);
            simple.setMessage("Error, the SNMP query has failed.");
            return simple;
        }
    }

    private static String getType(String type)
    {
        String val = iftype.get(type);
        if (val == null) return "unknown(" + type + ")";
        return val;
    }
}