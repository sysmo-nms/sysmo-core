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

package io.sysmo.nchecks.helpers;

import io.sysmo.nchecks.NHelperInterface;
import io.sysmo.nchecks.NHelperReply;
import io.sysmo.nchecks.Argument;
import io.sysmo.nchecks.Query;
import io.sysmo.nchecks.NHelperReply;
import io.sysmo.nchecks.NHelperSimpleReply;
import io.sysmo.nchecks.NHelperTableReply;
import io.sysmo.nchecks.NHelperTableRow;

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
import java.util.HashMap;
import java.util.Arrays;
import java.util.List;
import java.util.Iterator;



import java.io.CharArrayWriter;
import javax.json.Json;
import javax.json.JsonWriter;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonBuilderFactory;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;


public class HelperNetworkInterfaces implements NHelperInterface
{

    private static final String IF_INDEX = "1.3.6.1.2.1.2.2.1.1";
    private static final String IF_DESCR = "1.3.6.1.2.1.2.2.1.2";
    private static final String IF_TYPE  = "1.3.6.1.2.1.2.2.1.3";
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
        iftype = new HashMap<String,String>();
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

    public HelperNetworkInterfaces() {}

    public NHelperReply execute(Query query)
    {
        try {
            AbstractTarget target = NChecksSNMP.getInstance().getTarget(query);

            Snmp session = NChecksSNMP.getInstance().getSnmpSession();
            TableUtils tablewalker =
                new TableUtils(
                        session,
                        new DefaultPDUFactory(PDU.GETNEXT));

            List<TableEvent> snmpReply = tablewalker.getTable(
                    target,
                    columns,
                    null,
                    null);

            Iterator<TableEvent> it = snmpReply.iterator();
            TableEvent evt;

            NHelperTableReply table = new NHelperTableReply();
            while (it.hasNext()) {
                evt = it.next();
                VariableBinding[]   vbs = evt.getColumns();
                NHelperTableRow     row = new NHelperTableRow();
                row.addItem("ifIndex", vbs[0].getVariable().toString());
                row.addItem("ifDescr", vbs[1].getVariable().toString());
                row.addItem("ifType", getType(vbs[2].getVariable().toString()));
                row.addItem("ifPhysAddress", vbs[3].getVariable().toString());
                table.addRow(row);
            }
            table.setId("SelectNetworkInterfaces");
            table.setStatus(NHelperReply.SUCCESS);
            return table;

        } catch (Exception|Error e) {
            NHelperSimpleReply simple = new NHelperSimpleReply();
            simple.setId("SelectNetworkInterfaces");
            simple.setStatus(NHelperReply.FAILURE);
            simple.setMessage(e.toString());
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
