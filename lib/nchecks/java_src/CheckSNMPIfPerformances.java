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

package io.sysmo.nchecks.modules;

import io.sysmo.nchecks.NChecksInterface;
import io.sysmo.nchecks.Argument;
import io.sysmo.nchecks.Reply;
import io.sysmo.nchecks.Const;

import java.util.Map;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.net.Socket;
import java.net.InetSocketAddress;
import java.net.InetAddress;


public class CheckSNMPIfPerformances implements NChecksInterface
{
    private String  ifSelection;

    public CheckSNMPIfPerformances()
    {
        System.out.println("init CheckIfPerformances");
    }

    public void setOpaqueData(byte[] opaqueData)
    {
        /* DESERIALIZATION EXAMPLE
        ByteArrayInputStream b = new ByteArrayInputStream(opaqueData);
        ObjectInputStream o = new ObjectInputStream(b);

        Object myObject = o.readObject();
            or beter
        MyObjectClass = (MyObjectClass) o.readObject();
        */

        /* SERICALIZATION EXAMPLE
        ByteArrayOutputStream b = new ByteArrayOutputStream();
        ObjectOutputStream o = new ObjectOutputStream(b);
        o.writeObject(myObject);
        opaqueData = b.toByteArray();
        */
    }

    public void setConfig(Map<String,Argument> config)
    {
        Argument ifPerfArg = config.get("if_selection");

        ifSelection = ifPerfArg.getStr();
    }

    public Reply execute()
    {
        Reply reply = new Reply();
        reply.setStatus(Const.STATUS_ERROR);
        reply.setReply("IfPerTableTest...");
        return reply;
    }
}
