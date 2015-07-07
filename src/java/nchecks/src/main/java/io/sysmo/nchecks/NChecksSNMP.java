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
import io.sysmo.nchecks.Query;
import io.sysmo.nchecks.NChecksLogger;

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
    public  Snmp snmp4jSession;
    private Map<String, AbstractTarget> agents;
    private static NChecksSNMP INSTANCE;

    public static void startSnmp() {new NChecksSNMP();}
    private NChecksSNMP()
    {
        try
        {
            byte[] engineId = SNMPUtils.getEngineId("etc/worker_engine.id");
            UdpTransportMapping transport = new DefaultUdpTransportMapping();
            snmp4jSession   = new Snmp(transport);
            USM usm         = new USM(SecurityProtocols.getInstance(),
                                                new OctetString(engineId), 0);
            SecurityModels.getInstance().addSecurityModel(usm);
            agents          = new HashMap<String, AbstractTarget>();
            transport.listen();
        }
        catch (Exception e)
        {
            NChecksLogger.getLogger().severe("SNMP init fail: " +
                                                    e.getMessage() + e);
        }
        INSTANCE = this;
    }

    public static NChecksSNMP getInstance() {return INSTANCE;}

    public Snmp getSnmpSession() {return snmp4jSession;}

    public synchronized void cleanup()
    {
        snmp4jSession.getUSM().removeAllUsers();
        agents = new HashMap<String, AbstractTarget>();
    }

    public synchronized AbstractTarget getTarget(Query query) throws Exception
    {
        return getSnmpTarget(query);
    }

    private AbstractTarget getSnmpTarget(Query query) throws Exception
    {
        String targetid = query.get("target_id").asString();
        AbstractTarget target = agents.get(targetid);
        if (target != null) { return target; }

        target = generateTarget(query);
        if (target.getSecurityModel() != SecurityModel.SECURITY_MODEL_USM)
        {
            agents.put(targetid, target);
            return target;
        } else {
            UsmUser     user     = generateUser(query);
            OctetString username = user.getSecurityName();
            UsmUserEntry oldUser =
                    snmp4jSession.getUSM().getUserTable().getUser(username);
            if (oldUser == null)
            {
                snmp4jSession.getUSM().addUser(user);
                agents.put(targetid, target);
                return target;
            }
            else
            {
                if (SNMPUtils.usmUsersEquals(oldUser.getUsmUser(),user) == true)
                {
                    // same users conf, ok
                    agents.put(targetid, target);
                    return target;
                }
            }
        }
        throw new Exception("User name exists with differents credencials");
    }

    private AbstractTarget generateTarget(Query query) throws Exception, Error
    {
        String  host        = query.get("host").asString();
        int     port        = query.get("snmp_port").asInteger();
        String  seclevel    = query.get("snmp_seclevel").asString();
        String  version     = query.get("snmp_version").asString();
        int     retries     = query.get("snmp_retries").asInteger();
        int     timeout     = query.get("snmp_timeout").asInteger();
        
        Address address = GenericAddress.parse("udp:" + host + "/" + port);

        int seclevelConst = SNMPUtils.getSecLevel(seclevel);

        switch (version)
        {
            case "3":
                String  secname = query.get("snmp_usm_user").asString();
                UserTarget targetV3 = new UserTarget();
                targetV3.setAddress(address);
                targetV3.setRetries(retries);
                targetV3.setTimeout(timeout);
                targetV3.setVersion(SnmpConstants.version3);
                targetV3.setSecurityLevel(seclevelConst);
                targetV3.setSecurityName(new OctetString(secname));
                return targetV3;

            default:
                String  community = query.get("snmp_community").asString();
                CommunityTarget target = new CommunityTarget();
                target.setCommunity(new OctetString(community));
                target.setAddress(address);
                target.setRetries(retries);
                target.setTimeout(timeout);
                if (version.equals("2c")) {
                    target.setVersion(SnmpConstants.version2c);
                } else {
                    target.setVersion(SnmpConstants.version1);
                }
                return target;
        }
    }

    private UsmUser generateUser(Query query) throws Exception, Error
    {

        OID authProtoOid =
                    SNMPUtils.getAuthProto(query.get("snmp_authproto").asString());
        OID privProtoOid =
                    SNMPUtils.getPrivProto(query.get("snmp_privproto").asString());

        OctetString uName =
                        new OctetString(query.get("snmp_usm_user").asString());
        OctetString authkey = 
                        new OctetString(query.get("snmp_authkey").asString());
        OctetString privkey = 
                        new OctetString(query.get("snmp_privkey").asString());
        UsmUser usmuser = new UsmUser(
                                        uName,authProtoOid,authkey,
                                                        privProtoOid,privkey);
        return usmuser;
    }
}


class SNMPUtils
{
    private static char[] hexArray = "0123456789ABCDEF".toCharArray();

    // TODO test utf8 to byte might not work now
    public static byte[] getEngineId(String stringPath)
                                                    throws Exception, Error
    {
        Path path = Paths.get(stringPath);
        if (Files.isRegularFile(path) == true)
        {
            byte[] engineIdDump = Files.readAllBytes(path);
            String engineIdHex = new String(engineIdDump, "UTF-8");
            byte[] engineId = SNMPUtils.hexStringToBytes(engineIdHex);
            return engineId;
        }
        else
        {
            byte[] engineId     = MPv3.createLocalEngineID();
            String engineIdHex  = SNMPUtils.bytesToHexString(engineId);
            byte[] engineIdDump = engineIdHex.getBytes("UTF-8");
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

    private static String bytesToHexString(byte[] bytes) throws Exception,Error
    {
        char[] hexChars = new char[bytes.length * 2];
        for (int j=0; j<bytes.length; j++)
        {
            int v = bytes[j] & 0xFF;
            hexChars[j * 2] = hexArray[v >>> 4];
            hexChars[j * 2 + 1] = hexArray[v & 0x0F];
        }
        String chars = 
                new String(
                        new String(hexChars).getBytes("UTF-8"), "UTF-8");
        return chars;
    }
    
    public static int getSecLevel(String constString) throws Exception
    {
        
        int seclevel;
        switch (constString)
        {
            case "authPriv":
                seclevel = SecurityLevel.AUTH_PRIV; break;
            case "authNoPriv":
                seclevel = SecurityLevel.AUTH_NOPRIV; break;
            case "noAuthNoPriv":
                seclevel = SecurityLevel.NOAUTH_NOPRIV; break;
            default:
                throw new Exception("No such seclevel!");
        }
        return seclevel;
    }
    
    public static OID getAuthProto(String constString) throws Exception
    {

        OID authProtoOid = null;
        switch (constString)
        {
            case "SHA": authProtoOid = AuthSHA.ID; break;
            case "MD5": authProtoOid = AuthMD5.ID; break;
            default:
                throw new Exception("unknown authentication protocol");
        }
        return authProtoOid;
    }

    public static OID getPrivProto(String constString) throws Exception
    {
        OID privProtoOid = null;
        switch (constString)
        {
            case "AES":         privProtoOid = PrivAES128.ID; break;
            case "AES192":      privProtoOid = PrivAES192.ID; break;
            case "AES256":      privProtoOid = PrivAES256.ID; break;
            case "DES":         privProtoOid = PrivDES.ID;    break;
            case "3DES":        privProtoOid = Priv3DES.ID;   break;
            case "AES192_3DES": privProtoOid = PrivAES192With3DESKeyExtension.ID; break;
            case "AES256_3DES": privProtoOid = PrivAES256With3DESKeyExtension.ID; break;
            default:
                throw new Exception("unknown private protocol");
        }

        return privProtoOid;
    }
    

    public static boolean usmUsersEquals(UsmUser a, UsmUser b)
    {
        if (a.toString().equals(b.toString()))
        {
            return true;
        }
        else
        {
            return false;
        }
    }
}
