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

import java.io.CharArrayWriter;
import javax.json.Json;
import javax.json.JsonWriter;
import javax.json.JsonObjectBuilder;

/*
* This class build a simple message for client view. It will pop up
* with an icon corresponding to the status, and the message defined.
* It will fill the flag as defined in the xml check definition file
* to "value". See tutorials on Nchecks Helpers xml for more informations
*/

public class HelperSimpleReply implements HelperReply
{

    private String  messageId   = "";
    private String  message     = "";
    private String  value       = "";
    private String  status      = HelperReply.SUCCESS;

    public HelperSimpleReply() {}

    /*
    * Set the identifier of the reply
    */
    public void setId(String val)  { this.messageId = val; }

    /* set the status of the reply. Must be HelperReply.SUCCESS or
    * HelperReply.FAILURE.
    */
    public void setStatus(String val) { this.status = val; }

    /*
    * Set the message string show to the user.
    */
    public void setMessage(String val) { this.message = val; }

    /*
    * Set the value for the target flag.
    */
    public void setValue(String val) { this.value = val; }

    /*
     * Build a json representation of the message.
     */
    public char[] toCharArray()
    {
        CharArrayWriter     buffer          = new CharArrayWriter();
        JsonWriter          jsonWriter      = Json.createWriter(buffer);
        JsonObjectBuilder   objectbuilder   = Json.createObjectBuilder();

        objectbuilder.add("status",     this.status);
        objectbuilder.add("id",         this.messageId);
        objectbuilder.add("message",    this.message);
        objectbuilder.add("value",      this.value);
        
        jsonWriter.writeObject(objectbuilder.build());
        return buffer.toCharArray();
    }
}
