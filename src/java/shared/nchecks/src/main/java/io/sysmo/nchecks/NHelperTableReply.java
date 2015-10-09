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

package io.sysmo.nchecks;

import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;

import java.io.CharArrayWriter;
import javax.json.Json;
import javax.json.JsonWriter;
import javax.json.JsonArrayBuilder;
import javax.json.JsonBuilderFactory;
import javax.json.JsonObjectBuilder;

/**
 * This class build a simple message for client view. It will pop up
 * with an icon corresponding to the status, and the message defined.
 * It will fill the flag as defined in the xml check definition file
 * to "value". See tutorials for more informations
 */

public class NHelperTableReply implements NHelperReply
{

    public static final String SELECT_SINGLE   = "single";
    public static final String SELECT_MULTIPLE = "multiple";
    private String messageId = "";
    private String message   = "";
    private String status    = NHelperReply.SUCCESS;
    private String treeRoot  = "";
    private String select    = "";
    private String selectionType = "";
    private String listSeparator = "";

    private ArrayList<NHelperTableRow> rows;
    public NHelperTableReply() {
        rows = new ArrayList<>();
    }


    /*
    *  Add a HelperTableRow to the table.
    */
    public void addRow(NHelperTableRow row) {
        rows.add(row);
    }



    /*
    *  Set the reply id.
    */
    public void setId(String val)  { messageId = val; }

    /*
    *  Set the status
    * (NHelperReply.SUCCESS | NHelperReply.FAILURE)
    */
    public void setStatus(String val) { status = val; }

    /*
    *  Set the message string shown on top of the table.
    */
    public void setMessage(String val) { message = val; }

    /*
     * Set the treeroot
     */
    public void setTreeRoot(String val) {treeRoot = val; }

    /*
     * Set the selectCol
     */
    public void setSelectColumn(String val) {select = val; }

    /*
     * Set the selectType
    * (NHelperReply.SINGLE | NHelperReply.MULTIPLE)
     */
    public void setSelectType(String val) {selectionType = val; }


    /*
     * Set the list separator
     */
    public void setListSeparator(String val) {listSeparator = val; }


    public char[] toCharArray()
    {
        CharArrayWriter     buffer          = new CharArrayWriter();
        JsonBuilderFactory  factory         = Json.createBuilderFactory(null);
        JsonWriter          jsonWriter      = Json.createWriter(buffer);
        JsonObjectBuilder   objectBuilder   = factory.createObjectBuilder();
        JsonArrayBuilder    arrayBuilder    = factory.createArrayBuilder();

        Iterator<NHelperTableRow> it = rows.iterator();

        while (it.hasNext()) {
            NHelperTableRow              row     = it.next();
            JsonObjectBuilder            rowObj  = factory.createObjectBuilder();
            List<NHelperTableItem>       items   = row.getItems();
            Iterator<NHelperTableItem>   tit     = items.iterator();
            while(tit.hasNext()) {
                NHelperTableItem item  = tit.next();
                String           key   = item.getColumn();
                String           value = item.getValue();
                rowObj.add(item.getColumn(), item.getValue());
            }
            arrayBuilder.add(rowObj);
        }

        objectBuilder.add("type",          "table");
        objectBuilder.add("treeRoot",      treeRoot);
        objectBuilder.add("select",        select);
        objectBuilder.add("selectionType", selectionType);
        objectBuilder.add("listSeparator", listSeparator);

        objectBuilder.add("status",  status);
        objectBuilder.add("message", message);
        objectBuilder.add("id",      messageId);
        objectBuilder.add("rows",    arrayBuilder);
        jsonWriter.writeObject(objectBuilder.build());
        return buffer.toCharArray();
    }
}
