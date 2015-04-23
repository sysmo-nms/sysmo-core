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
import io.sysmo.nchecks.Argument;

import java.util.Map;
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

    Map<String,Argument> conf;

    public HelperNetworkInterfaces()
    {
        System.out.println("init helper");
    }

    public void setConfig(Map<String,Argument> config)
    {
        conf = config;
    }

    public char[] execute()
    {
        CharArrayWriter     buff            = new CharArrayWriter();
        JsonWriter          jsonWriter      = Json.createWriter(buff);

        JsonBuilderFactory  factory         = Json.createBuilderFactory(null);
        JsonObjectBuilder   objectbuilder   = factory.createObjectBuilder();
        JsonArrayBuilder    arraybuilder    = factory.createArrayBuilder();
        
        arraybuilder
                .add(factory.createObjectBuilder()
                    .add("iftype", "k")
                    .add("jojo", "lolo"));
        arraybuilder
                .add(factory.createObjectBuilder()
                    .add("iftype", "l")
                    .add("jojo", "lojo"));

        objectbuilder.add("id", "SelectNetworkInterfaces");
        objectbuilder.add("rows", arraybuilder);
        
        jsonWriter.writeObject(objectbuilder.build());

        return buff.toCharArray();
    }
}
