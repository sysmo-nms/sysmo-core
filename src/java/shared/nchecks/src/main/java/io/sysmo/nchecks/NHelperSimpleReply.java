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

package io.sysmo.nchecks;

import io.sysmo.nchecks.NHelperReply;

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

/*
* This class build a simple message for client view. It will pop up
* with an icon corresponding to the status, and the message defined.
* It will fill the flag as defined in the xml check definition file
* to "value". See tutorials on Nchecks Helpers xml for more informations
*/

public class NHelperSimpleReply implements NHelperReply
{

    private String  messageId   = "";
    private String  message     = "";
    private String  value       = "";
    private String  status      = NHelperReply.SUCCESS;

    public NHelperSimpleReply() {}

    /*
    * Set the identifier of the reply
    */
    public void setId(String val)  { this.messageId = val; }

    /* set the status of the reply. Must be NHelperReply.SUCCESS or
    * NHelperReply.FAILURE.
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
