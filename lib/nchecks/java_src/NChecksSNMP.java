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
import io.sysmo.nchecks.Argument;

import java.io.*;
import java.util.*;
import java.nio.file.*;

import org.snmp4j.*;
import org.snmp4j.asn1.*;
import org.snmp4j.event.*;
import org.snmp4j.log.*;
import org.snmp4j.mp.*;
import org.snmp4j.security.*;
import org.snmp4j.smi.*;
import org.snmp4j.transport.*;
import org.snmp4j.util.*;

// AES192_3DES AES256_3DES
import org.snmp4j.security.nonstandard.*;

public class NChecksSNMP
{
    private static NChecksSNMP singleton;

    private Snmp                     snmp4jSession;
    private Map<String, SNMPElement> snmpElements; 

    public static synchronized void initialize() {
        if (singleton == null) new NChecksSNMP();
    }

    public NChecksSNMP()
    {
        try
        {
            byte[] engineId = SNMPUtils.getEngineId("cfg/worker_engine.id");
            UdpTransportMapping transport = new DefaultUdpTransportMapping();
            snmp4jSession   = new Snmp(transport);
            USM usm         = new USM(SecurityProtocols.getInstance(),
                                                new OctetString(engineId), 0);
            SecurityModels.getInstance().addSecurityModel(usm);
            transport.listen();
            singleton = this;
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }

    public static Snmp getSnmpSession() {return singleton.snmp4jSession;}
}


class SNMPElement
{
    private AbstractTarget   target         = null;
    private OctetString      securityName   = null;
    private String           targetName     = null;

    // target get and set
    public void setTarget(AbstractTarget aTarget)
    {
        this.target = aTarget;
        this.securityName = this.target.getSecurityName();
    }

    public AbstractTarget getTarget()
    {
        return this.target;
    }

    public OctetString getSecurityName()
    {
        return this.securityName;
    }

    // targetName get and set
    public void setName(String name)
    {
        this.targetName = name;
    }

    public String getName()
    {
        return this.targetName;
    }
}




class SNMPUtils
{
    private static char[] hexArray = "0123456789ABCDEF".toCharArray();

    public static byte[] getEngineId(String stringPath)
                                                    throws Exception, Error
    {
        Path path = Paths.get(stringPath);
        if (Files.isRegularFile(path) == true)
        {
            byte[] engineIdDump = Files.readAllBytes(path);
            String engineIdHex = new String(engineIdDump);
            byte[] engineId = SNMPUtils.hexStringToBytes(engineIdHex);
            return engineId;
        }
        else
        {
            byte[] engineId     = MPv3.createLocalEngineID();
            String engineIdHex  = SNMPUtils.bytesToHexString(engineId);
            byte[] engineIdDump = engineIdHex.getBytes();
            Files.write(path, engineIdDump);
            return engineId;
        }
    }

    private static byte[] hexStringToBytes(String s)
    {
        int len = s.length();
        byte[] data = new byte[len / 2];
        for (int i = 0; i < len; i += 2) {
            data[i / 2] = (byte) ((Character.digit(s.charAt(i), 16) << 4)
                    + Character.digit(s.charAt(i+1), 16));
        }
        return data;
    }

    private static String bytesToHexString(byte[] bytes)
    {
        char[] hexChars = new char[bytes.length * 2];
        for (int j=0; j<bytes.length; j++)
        {
            int v = bytes[j] & 0xFF;
            hexChars[j * 2] = hexArray[v >>> 4];
            hexChars[j * 2 + 1] = hexArray[v & 0x0F];
        }
        return new String(hexChars);
    }
}
