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

package io.sysmo.snmpm;

import java.io.*;
import java.util.*;
import java.nio.file.*;

import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangByte;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

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

public class Snmpman
{
    // my node name
    private static String selfNodeName   = null;

    // the foreign node name (-sname)
    private static String foreignNodeName = null;

    // the foreign snmpman.erl gen_server pid name
    private static String foreignPidName  = null;

    public static OtpErlangAtom atomReply       = new OtpErlangAtom("reply");
    public static OtpErlangAtom atomOk          = new OtpErlangAtom("ok");
    public static OtpErlangAtom atomTimeout     = new OtpErlangAtom("timeout");
    public static OtpErlangAtom atomError       = new OtpErlangAtom("error");
    public static OtpErlangAtom atomEXIT        = new OtpErlangAtom("EXIT");
    public static OtpErlangAtom atomVarbinds    = new OtpErlangAtom("varbinds");
    public static OtpErlangAtom atomVarbind     = new OtpErlangAtom("varbind");
    public static OtpErlangAtom atomUnknownTarget = new OtpErlangAtom("unknown_target");
    public static OtpErlangAtom atomTargetExist   = new OtpErlangAtom("target_exist");
    public static OtpErlangAtom atomTableRow    = new OtpErlangAtom("table_row");
    public static OtpErlangAtom atomException   = new OtpErlangAtom("exception");
    public static OtpErlangAtom atomWrongOrder  = new OtpErlangAtom("wrong_order");
    public static OtpErlangAtom atomReport      = new OtpErlangAtom("report");
    public static OtpErlangAtom atomTable       = new OtpErlangAtom("table");
    // erlang server
    private static  OtpNode self = null;
    private static  OtpMbox mbox = null;

    // snmp4j
    //static          SnmpmanServer snmpserver;
    private static Snmp                snmp4jSession   = null;
    private static DefaultUdpTransportMapping transport = null;
    private static char[] hexArray = "0123456789ABCDEF".toCharArray();
    private static Map<String, SnmpmanElement> snmpmanElements = 
        new HashMap<String, SnmpmanElement>();

    public static void main(String[] args)
    {
                try
        {
            Properties   prop  = new Properties();
            InputStream  input = new FileInputStream("cfg/snmpman.properties");
            prop.load(input);
            selfNodeName     = prop.getProperty("self_name");
            foreignNodeName  = prop.getProperty("foreign_node");
            foreignPidName   = prop.getProperty("foreign_pid");
        }
        catch(IOException e)
        {
            e.printStackTrace();
            return;
        }

        // may take a wile? /dev/random ?
        try
        {
            byte[] engineId = getEngineId(args[0]);
            transport       = new DefaultUdpTransportMapping();
            snmp4jSession   = new Snmp(transport);
            USM usm         = new USM(SecurityProtocols.getInstance(),
                    new OctetString(engineId), 0);
            SecurityModels.getInstance().addSecurityModel(usm);
            transport.listen();
        }
        catch (Exception|Error e2)
        {
            e2.printStackTrace();
            return;
        }

        // Initialize
        try 
        {
            self = new OtpNode(selfNodeName);
            mbox = self.createMbox();
            if (!self.ping(foreignNodeName, 2000)) 
            { 
                System.out.println("Connection timed out");
                return;
            }
        }
        catch (IOException e1) 
        {
            e1.printStackTrace();
            return;
        }

        
        // when it is ok, inform the erl snmpman process
        acknowledgeOtpConnexion();

        // then begin to loop and wait for calls
        OtpErlangObject call = null;
        while (true) try 
        {
            call = mbox.receive();
            handleMsg(call);
        } 
        catch (OtpErlangExit e) 
        {
            e.printStackTrace();
            break;
        }
        catch (OtpErlangDecodeException e)
        {
            e.printStackTrace();
        }
    }

    private static byte[] getEngineId(String stringPath) throws Exception, Error
    {
        Path path = Paths.get(stringPath);
        if (Files.isRegularFile(path) == true)
        {
            byte[] engineIdDump = Files.readAllBytes(path);
            String engineIdHex = new String(engineIdDump);
            byte[] engineId = hexStringToBytes(engineIdHex);
            return engineId;
        }
        else
        {
            byte[] engineId     = MPv3.createLocalEngineID();
            String engineIdHex  = bytesToHexString(engineId);
            byte[] engineIdDump = engineIdHex.getBytes();
            Files.write(path, engineIdDump);
            return engineId;
        }
    }

    private static void acknowledgeOtpConnexion()
    {
        OtpErlangObject[] msg = new OtpErlangObject[2];
        msg[0] = mbox.self();
        msg[1] = new OtpErlangAtom("snmp4j_running");

        OtpErlangTuple tuple  = new OtpErlangTuple(msg);

        synchronized(mbox)
        {
            mbox.send(foreignPidName, foreignNodeName, tuple);
        }

    }

    /**
     * Send a message to the caller.
     * Used by SnmpmanResponseListener SnmpmanTreeListener and SnmpmanTableListener
     * which are executed in the asynchroneously in another thread.
     */
    public static void sendReply(OtpErlangObject to, OtpErlangObject msg)
    {
        OtpErlangObject[] obj = new OtpErlangObject[3];
        obj[0] = atomReply;
        obj[1] = to;
        obj[2] = msg;
        OtpErlangTuple tuple = new OtpErlangTuple(obj);
        synchronized(mbox)
        {
            mbox.send(foreignPidName, foreignNodeName, tuple);
        }
    }

    private static void handleMsg(OtpErlangObject msg)
    {
        OtpErlangTuple  tuple;
        OtpErlangAtom   command;
        OtpErlangObject caller;
        OtpErlangTuple  payload;
        try
        {
            tuple       = (OtpErlangTuple) msg;
            command     = (OtpErlangAtom)   (tuple.elementAt(0));
            caller      = (OtpErlangObject) (tuple.elementAt(1));
            payload     = (OtpErlangTuple)  (tuple.elementAt(2));
        }
        catch (Exception|Error e)
        {
            e.printStackTrace();
            return;
        }

        try
        {
            switch (command.toString())
            {
                case "get":
                    handleGet(caller, payload);
                    break;

                case "walk_tree":
                    handleWalkTree(caller, payload);
                    break;

                case "walk_table":
                    handleWalkTable(caller, payload);
                    break;

                case "discovery": 
                    handleDiscovery(caller, payload);
                    break;

                case "register_element":
                    handleRegisterElement(caller, payload);
                    break;

                case "unregister_element":
                    handleUnregisterElement(caller, payload);
                    break;

                case "which_elements":
                    handleWhichElements(caller);
                    break;

                case "which_usm_users":
                    handleWhichUSMUsers(caller);
                    break;
            }
        }
        catch (Exception|Error e)
        {
            OtpErlangTuple reply = buildErrorReply(
                new OtpErlangString("Java CATCH: Failed to honour command "
                    + command.toString() + " -> " + e.getMessage())
            );

            sendReply(caller, reply);
        }
    }

    /**
     * Register a new element.
     * High level API form snmpman.
     */
    private static void handleRegisterElement(
            OtpErlangObject caller, OtpErlangTuple confTuple) throws Exception, Error
    {
        RegisterArgs registerArgs   = new RegisterArgs(confTuple);
        SnmpmanElement element      = new SnmpmanElement();

        if (snmpmanElements.get(registerArgs.elementName) != null)
        {
            OtpErlangTuple reply = buildErrorReply(atomTargetExist);
            sendReply(caller, reply);
            return;
        }

        switch (registerArgs.snmpVersion)
        {
            case "3":
                element.setTarget(generateTarget(registerArgs));
                element.setName(registerArgs.elementName);
                if (registerUSMUser(registerArgs) == true)
                {
                    snmpmanElements.put(element.getName(), element);
                    sendReply(caller, atomOk);
                    break;
                }
                else
                {
                    OtpErlangTuple reply = buildErrorReply(
                        new OtpErlangString("USM user definition do not match a predefined entry")
                    );
                    sendReply(caller, reply);
                    break;
                }
            default:
                element.setTarget(generateTarget(registerArgs));
                element.setName(registerArgs.elementName);
                snmpmanElements.put(element.getName(), element);
                sendReply(caller, atomOk);
                break;
        }
    }


    /**
     * Unregister a new element.
     * High level API form snmpman. Delete the target and for snmp v3, the 
     * localized USM user from the USM table.
     */
    private static void handleUnregisterElement(
            OtpErlangObject caller, OtpErlangTuple unregArg) throws Exception, Error
    {
        OtpErlangString elementName = (OtpErlangString) (unregArg.elementAt(0));
        SnmpmanElement element = snmpmanElements.remove(elementName.stringValue());
    
        if (element == null)
        {
            sendReply(caller, buildErrorReply(atomUnknownTarget));
            return;
        }

        if (element.getTarget().getVersion() == SnmpConstants.version3)
        {
            OctetString secUser     = element.getSecurityName();
            boolean deleteUsm = true;
            for (Map.Entry<String, SnmpmanElement> remaining: snmpmanElements.entrySet())
            {
                if (remaining.getValue().getSecurityName().equals(secUser)) {
                    deleteUsm = false;
                    break;
                }
            }
            if (deleteUsm == true)
            {
                snmp4jSession.getUSM().removeAllUsers(secUser);
            }
        }

        sendReply(caller, atomOk);
    }

    private static void handleGet(
            OtpErlangObject caller, OtpErlangTuple tuple) throws Exception, Error
    {
        OtpErlangString elementName     = (OtpErlangString) (tuple.elementAt(0));
        OtpErlangList oidList           = (OtpErlangList) (tuple.elementAt(1));

        if (snmpmanElements.containsKey(elementName.stringValue()) == false) {
            sendReply(caller, buildErrorReply(atomUnknownTarget));
            return;
        }
        
        OID[] oidFinalList = new OID[oidList.arity()];

        for (int i =0; i<oidList.arity(); i++)
        {
            OtpErlangString oidString = (OtpErlangString) (oidList.elementAt(i));
            OID oid = new OID(oidString.stringValue());
            if (oid.isValid() == false)
            {
                sendReply(caller, 
                    buildErrorReply(
                        new OtpErlangString(
                            "invalid OID: " + oidString.stringValue()
                            )
                        )
                    );
                return;
            }
            oidFinalList[i] = oid;
        }

        Target  target  = snmpmanElements.get(elementName.stringValue()).getTarget();
        PDU     pdu     = null;
        if (target.getVersion() == SnmpConstants.version3)
        {
            pdu = new ScopedPDU();
        }
        else
        {
            pdu = new PDU();
        }
        pdu.setType(PDU.GET);

        for (int i=0; i <oidFinalList.length; i++)
        {
            pdu.add(new VariableBinding(oidFinalList[i]));
        }

        ResponseListener listener = new SnmpmanResponseListener(caller);
        snmp4jSession.send(pdu, target, null, listener);
    }


    private static boolean registerUSMUser(
            RegisterArgs registerArgs) throws Exception, Error
    {

        OID authProtoOid = null;
        switch (registerArgs.authProto)
        {
            case "SHA": authProtoOid = AuthSHA.ID; break;
            case "MD5": authProtoOid = AuthMD5.ID; break;
        }

        OID privProtoOid = null;
        switch (registerArgs.privProto)
        {
            case "AES":         privProtoOid = PrivAES128.ID; break;
            case "AES192":      privProtoOid = PrivAES192.ID; break;
            case "AES256":      privProtoOid = PrivAES256.ID; break;
            case "DES":         privProtoOid = PrivDES.ID;    break;
            case "3DES":        privProtoOid = Priv3DES.ID;   break;
            case "AES192_3DES": privProtoOid = PrivAES192With3DESKeyExtension.ID; break;
            case "AES256_3DES": privProtoOid = PrivAES256With3DESKeyExtension.ID; break;
        }

        SecurityProtocols secProtocols = SecurityProtocols.getInstance();


        OctetString uName = new OctetString(registerArgs.secName);
        UsmUser newUsmUser = new UsmUser(
            uName,
            authProtoOid,
            new OctetString (registerArgs.authKey),
            privProtoOid,
            new OctetString (registerArgs.privKey)
        );

        UsmUserEntry existUsmUser = snmp4jSession.getUSM().getUserTable().getUser(uName);
        if (existUsmUser == null)
        {
            System.out.println("user do not exist");
            snmp4jSession.getUSM().addUser(newUsmUser);
            return true;
        }
        else // usm user exist
        {
            System.out.println("user exist");
            if (usmUsersEquals(existUsmUser.getUsmUser(), newUsmUser) == true)
            {
                // same user, do nothing
                return true;
            }
            else
            {
                // same user but different config, it is an error
                System.out.println("are not equals");
                return false;
            }
        }
    }

    private static boolean usmUsersEquals(UsmUser a, UsmUser b)
    {
        System.out.println("a tostring: " + a.toString());
        System.out.println("b tostring: " + b.toString());
        if (a.toString().equals(b.toString()))
        {
            return true;
        }
        else
        {
            return false;
        }
    }

    private static AbstractTarget generateTarget(
                RegisterArgs registerArgs) throws Exception, Error
    {
        Address targetAddress = GenericAddress.parse(
                "udp:" + registerArgs.host + "/" + registerArgs.ipPort);

        int securityLevelConst = 0;
        switch (registerArgs.secLevel)
        {
            case "authPriv":
                securityLevelConst = SecurityLevel.AUTH_PRIV; break;
            case "authNoPriv":
                securityLevelConst = SecurityLevel.AUTH_NOPRIV; break;
            case "noAuthNoPriv":
                securityLevelConst = SecurityLevel.NOAUTH_NOPRIV; break;
        }

        switch (registerArgs.snmpVersion)
        {
            case "3":
                UserTarget targetV3 = new UserTarget();
                targetV3.setAddress(targetAddress);
                targetV3.setRetries(registerArgs.retries);
                targetV3.setTimeout(registerArgs.timeout);
                targetV3.setVersion(SnmpConstants.version3);
                targetV3.setSecurityLevel(securityLevelConst);
                targetV3.setSecurityName(new OctetString(registerArgs.secName));
                return targetV3;

            default:
                CommunityTarget target = new CommunityTarget();
                target.setCommunity(new OctetString(registerArgs.community));
                target.setAddress(targetAddress);
                target.setRetries(registerArgs.retries);
                target.setTimeout(registerArgs.timeout);
                if (registerArgs.snmpVersion.equals("2c")) {
                    target.setVersion(SnmpConstants.version2c);
                } else {
                    target.setVersion(SnmpConstants.version1);
                }
                return target;
        }
    }

    private static void handleDiscovery(
            OtpErlangObject caller, OtpErlangTuple tuple) throws Exception, Error
    {
        OtpErlangString targetIp        = (OtpErlangString) (tuple.elementAt(0));
        OtpErlangLong   targetPort      = (OtpErlangLong)   (tuple.elementAt(2));
        OtpErlangLong   timeout         = (OtpErlangLong)   (tuple.elementAt(3));

        Address targetAddress = GenericAddress.parse(
                "udp:" + targetIp.stringValue() + "/" + targetPort.intValue());

        byte[] engineId = snmp4jSession.discoverAuthoritativeEngineID(
                targetAddress, timeout.intValue());
        if (engineId == null)
        {
            sendReply(caller, buildErrorReply(atomTimeout));
        }
        else
        {
            String hexEngineID = bytesToHexString(engineId);
            OtpErlangString hexString = new OtpErlangString(hexEngineID);
            sendReply(caller, buildOkReply(hexString));
        }
    }

    private static void handleWalkTable(
            OtpErlangObject caller, OtpErlangTuple tuple) throws Exception, Error
    {
        OtpErlangString elementName     = (OtpErlangString) (tuple.elementAt(0));
        OtpErlangList   oidList         = (OtpErlangList)   (tuple.elementAt(1));



        if (snmpmanElements.containsKey(elementName.stringValue()) == false)
        {
            sendReply(caller, buildErrorReply(atomUnknownTarget));
            return;
        }

        OtpErlangObject[]   elements    = oidList.elements();
        OID[]               columnOIDs  = new OID[elements.length];

        for (int i=0; i<elements.length; i++)
        {
            OtpErlangString val = (OtpErlangString) (elements[i]);
            columnOIDs[i] = new OID(val.stringValue());
        }

        OID     lowerBoundIndex = null;
        OID     upperBoundIndex = null;

        Target target = snmpmanElements.get(elementName.stringValue()).getTarget();

        TableUtils tableUtils = new TableUtils(snmp4jSession, new GetNextPDUFactory());

        TableListener listener = new SnmpmanTableListener(caller);
        tableUtils.getTable(target, columnOIDs,
                listener, null, lowerBoundIndex, upperBoundIndex);
    }


    private static void handleWalkTree(
            OtpErlangObject caller, OtpErlangTuple tuple) throws Exception, Error
    {
        OtpErlangString elementName     = (OtpErlangString) (tuple.elementAt(0));
        OtpErlangString oidString       = (OtpErlangString) (tuple.elementAt(1));

        if (snmpmanElements.containsKey(elementName.stringValue()) == false)
        {
            sendReply(caller, buildErrorReply(atomUnknownTarget));
            return;
        }

        OID walkOid = new OID(oidString.stringValue());
        if (walkOid.isValid() == false)
        {
            OtpErlangTuple objTuple = buildErrorReply(
                new OtpErlangString("invalid OID: " + oidString.stringValue())
            );
            sendReply(caller, objTuple);
            return;
        }

        Target target = snmpmanElements.get(elementName.stringValue()).getTarget();
        TreeUtils treeUtils = new TreeUtils(snmp4jSession, new GetNextPDUFactory());
        TreeListener treeListener = new SnmpmanTreeListener(caller);
        treeUtils.getSubtree(target, walkOid, null, treeListener);
    }

    private static void handleWhichElements(OtpErlangObject caller) throws Exception, Error
    {
        int size = snmpmanElements.size();
        OtpErlangObject[] replyObj = new OtpErlangObject[size];
        int i = 0;
        for (String key: snmpmanElements.keySet())
        {
            replyObj[i] = new OtpErlangString(key);
            i += 1;
        }
        OtpErlangList replyList = new OtpErlangList(replyObj);

        sendReply(caller, buildOkReply(replyList));
    }

    private static void handleWhichUSMUsers(OtpErlangObject caller) throws Exception, Error
    {
        List<UsmUserEntry> usmUsers = 
            snmp4jSession.getUSM().getUserTable().getUserEntries();

        int size = usmUsers.size();
        OtpErlangObject[] replyObj = new OtpErlangObject[size];

        Iterator<UsmUserEntry> it = usmUsers.iterator();
        for (int i=0; i<usmUsers.size(); i++)
        {
            replyObj[i] = 
                new OtpErlangString(usmUsers.get(i).getUserName().toString());
        }
        OtpErlangList replyList = new OtpErlangList(replyObj);
        sendReply(caller, buildOkReply(replyList));
    }




    // UTILS  UTILS  UTILS  UTILS  UTILS  UTILS  UTILS  UTILS  UTILS 
    // UTILS  UTILS  UTILS  UTILS  UTILS  UTILS  UTILS  UTILS  UTILS 
    // UTILS  UTILS  UTILS  UTILS  UTILS  UTILS  UTILS  UTILS  UTILS 


    public static OtpErlangTuple buildOkReply(OtpErlangObject msg)
    {
        OtpErlangObject[] valObj   = new OtpErlangObject[2];
        valObj[0] = atomOk;
        valObj[1] = msg;
        OtpErlangTuple valTuple = new OtpErlangTuple(valObj);
        return valTuple;
    }

    public static OtpErlangTuple buildErrorReply(OtpErlangObject msg)
    {
        OtpErlangObject[] valObj   = new OtpErlangObject[2];
        valObj[0] = atomError;
        valObj[1] = msg;
        OtpErlangTuple valTuple = new OtpErlangTuple(valObj);
        return valTuple;
    }

    public static OtpErlangTuple buildBindsTuple(ArrayList<OtpErlangTuple> bindsAcc)
    {
        int vbSize = bindsAcc.size();
        OtpErlangTuple[] bindsAcc1 = new OtpErlangTuple[vbSize];

        int c = 0;
        for (OtpErlangTuple obj: bindsAcc)
        {
            bindsAcc1[c] = obj;
            c += 1;
        }
        OtpErlangList allBinds = new OtpErlangList(bindsAcc1);

        // generate reply vbs = {varbinds, [varbind]}
        OtpErlangObject[] vbValObj   = new OtpErlangObject[2];
        vbValObj[0] = atomVarbinds;
        vbValObj[1] = allBinds;
        OtpErlangTuple vbValTuple = new OtpErlangTuple(vbValObj);


        return buildOkReply(vbValTuple);
    }

    public static OtpErlangTuple buildTableTuple(ArrayList<OtpErlangTuple> bindsAcc)
    {
        int vbSize = bindsAcc.size();
        OtpErlangTuple[] bindsAcc1 = new OtpErlangTuple[vbSize];

        int c = 0;
        for (OtpErlangTuple obj: bindsAcc)
        {
            bindsAcc1[c] = obj;
            c += 1;
        }
        OtpErlangList allBinds = new OtpErlangList(bindsAcc1);

        // generate reply vbs = {varbinds, [varbind]}
        OtpErlangObject[] vbValObj   = new OtpErlangObject[2];
        vbValObj[0] = atomTable;
        vbValObj[1] = allBinds;
        OtpErlangTuple vbValTuple = new OtpErlangTuple(vbValObj);


        return buildOkReply(vbValTuple);
    }


    public static OtpErlangObject getValue(VariableBinding var)
    {
        switch (var.getSyntax())
        {
            case SMIConstants.SYNTAX_COUNTER32:
                return new OtpErlangLong(var.getVariable().toLong());
            case SMIConstants.SYNTAX_COUNTER64:
                return new OtpErlangLong(var.getVariable().toLong());
            case SMIConstants.SYNTAX_GAUGE32:
                return new OtpErlangLong(var.getVariable().toLong());
            case SMIConstants.SYNTAX_INTEGER:
                return new OtpErlangInt(var.getVariable().toInt());
            default:
                return new OtpErlangString(var.getVariable().toString());
        }
    }

    public static OtpErlangString getReport(PDU response) {
        if (response.size() < 1) {
            return new OtpErlangString("REPORT PDU does not contain a variable binding.");
        }

        VariableBinding vb = response.get(0);
        OID oid =vb.getOid();
        if (SnmpConstants.usmStatsUnsupportedSecLevels.equals(oid)) {
            return new OtpErlangString("Unsupported Security Level.");
        }
        else if (SnmpConstants.usmStatsNotInTimeWindows.equals(oid)) {
            return new OtpErlangString("Message not within time window.");
        }
        else if (SnmpConstants.usmStatsUnknownUserNames.equals(oid)) {
            return new OtpErlangString("Unknown user name.");
        }
        else if (SnmpConstants.usmStatsUnknownEngineIDs.equals(oid)) {
            return new OtpErlangString("Unknown engine id.");
        }
        else if (SnmpConstants.usmStatsWrongDigests.equals(oid)) {
            return new OtpErlangString("Wrong digest.");
        }
        else if (SnmpConstants.usmStatsDecryptionErrors.equals(oid)) {
            return new OtpErlangString("Decryption error.");
        }
        else if (SnmpConstants.snmpUnknownSecurityModels.equals(oid)) {
            return new OtpErlangString("Unknown security model.");
        }
        else if (SnmpConstants.snmpInvalidMsgs.equals(oid)) {
            return new OtpErlangString("Invalid message.");
        }
        else if (SnmpConstants.snmpUnknownPDUHandlers.equals(oid)) {
            return new OtpErlangString("Unknown PDU handler.");
        }
        else if (SnmpConstants.snmpUnavailableContexts.equals(oid)) {
            return new OtpErlangString("Unavailable context.");
        }
        else if (SnmpConstants.snmpUnknownContexts.equals(oid)) {
            return new OtpErlangString("Unknown context.");
        }
        else {
            return new OtpErlangString("contains unknown OID ("
                    + oid.toString() + ").");
        }
    }


    public static byte[] hexStringToBytes(String s)
    {
        int len = s.length();
        byte[] data = new byte[len / 2];
        for (int i = 0; i < len; i += 2) {
            data[i / 2] = (byte) ((Character.digit(s.charAt(i), 16) << 4)
                    + Character.digit(s.charAt(i+1), 16));
        }
        return data;
    }

    public static String bytesToHexString(byte[] bytes)
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

class SnmpmanElement
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


class RegisterArgs
{

    public String elementName   = null;
    public String host          = null; 
    public String snmpVersion   = null; 
    public String secLevel      = null; 
    public String secName       = null; 
    public String community     = null; 
    public String authProto     = null; 
    public String authKey       = null; 
    public String privProto     = null; 
    public String privKey       = null; 
    public int    ipPort; 
    public int    retries; 
    public int    timeout; 

    public RegisterArgs(OtpErlangTuple confTuple) throws Exception, Error
    {
        OtpErlangString erlElementName = 
            (OtpErlangString) (confTuple.elementAt(1));
        OtpErlangString erlHost = 
            (OtpErlangString) (confTuple.elementAt(2));
        OtpErlangLong   erlIpPort  = 
            (OtpErlangLong)   (confTuple.elementAt(3));
        OtpErlangString erlSnmpVersion  = 
            (OtpErlangString) (confTuple.elementAt(4));
        OtpErlangString erlSecLevel  = 
            (OtpErlangString) (confTuple.elementAt(5));
        OtpErlangLong   erlRetries  = 
            (OtpErlangLong)   (confTuple.elementAt(6));
        OtpErlangLong   erlTimeout  = 
            (OtpErlangLong)   (confTuple.elementAt(7));
        OtpErlangString erlSecName  = 
            (OtpErlangString) (confTuple.elementAt(8));
        OtpErlangString erlCommunity  = 
            (OtpErlangString) (confTuple.elementAt(9));
        OtpErlangString erlAuthProto  = 
            (OtpErlangString) (confTuple.elementAt(10));
        OtpErlangString erlAuthKey  = 
            (OtpErlangString) (confTuple.elementAt(11));
        OtpErlangString erlPrivProto =
            (OtpErlangString) (confTuple.elementAt(12));
        OtpErlangString erlPrivKey  = 
            (OtpErlangString) (confTuple.elementAt(13));

        elementName = erlElementName.stringValue();
        host        = erlHost.stringValue();
        snmpVersion = erlSnmpVersion.stringValue();
        secLevel    = erlSecLevel.stringValue();
        secName     = erlSecName.stringValue();
        community   = erlCommunity.stringValue();
        authProto   = erlAuthProto.stringValue();
        authKey     = erlAuthKey.stringValue();
        privProto   = erlPrivProto.stringValue();
        privKey     = erlPrivKey.stringValue();
        ipPort      = erlIpPort.intValue();
        retries     = erlRetries.intValue();
        timeout     = erlTimeout.intValue();
    }
}

class SnmpmanResponseListener implements ResponseListener
{
    private OtpErlangObject to = null;

    public  SnmpmanResponseListener(OtpErlangObject caller)
    {
        to = caller;
    }

    public void onResponse(ResponseEvent event) 
    {
        ((Snmp)event.getSource()).cancel(event.getRequest(), this);
        PDU rep = event.getResponse();
        if (rep == null) 
        {
            Snmpman.sendReply(to, Snmpman.buildErrorReply(Snmpman.atomTimeout));
        }
        else if (rep.getType() == PDU.REPORT)
        {
            OtpErlangObject[] snmpReply = new OtpErlangObject[2];
            snmpReply[0]    = Snmpman.atomReport;
            snmpReply[1]    = Snmpman.getReport(rep);
            OtpErlangTuple snmpReplyTuple = new OtpErlangTuple(snmpReply);
            Snmpman.sendReply(to, snmpReplyTuple);
        }
        else 
        {
            Vector<? extends VariableBinding> vbs = rep.getVariableBindings();
            ArrayList<OtpErlangTuple> bindsAcc = new ArrayList<OtpErlangTuple>();
            for (VariableBinding vbv: vbs)
            {
                String   oidVal = vbv.getOid().toString();
                Variable var    = vbv.getVariable();
                int      syntax = var.getSyntax();
                OtpErlangObject varObj = Snmpman.getValue(vbv);

                OtpErlangObject[] a1 = new OtpErlangObject[4];
                a1[0] = Snmpman.atomVarbind;
                a1[1] = new OtpErlangString(oidVal);
                a1[2] = new OtpErlangInt(syntax);
                a1[3] = varObj;
                OtpErlangTuple encapTuple = new OtpErlangTuple(a1);
                bindsAcc.add(encapTuple);
            }
            Snmpman.sendReply(to, Snmpman.buildBindsTuple(bindsAcc));
        }
    }
}

class SnmpmanTreeListener implements TreeListener
{
    private final long       startTime  = System.nanoTime();
    private boolean          finished   = false;
    private OtpErlangObject  to         = null;
    private ArrayList<OtpErlangTuple> bindsAcc = new ArrayList<OtpErlangTuple>();

    public SnmpmanTreeListener(OtpErlangObject caller)
    {
        to = caller;
    }

    public boolean next(TreeEvent event)
    {
        if (event.getVariableBindings() != null)
        {
            VariableBinding[] vbs = event.getVariableBindings();
            for (VariableBinding vb : vbs)
            { 
                String   oidVal = vb.getOid().toString();
                Variable var    = vb.getVariable();
                int      syntax = var.getSyntax();
                OtpErlangObject varObj = Snmpman.getValue(vb);

                OtpErlangObject[] a1 = new OtpErlangObject[4];
                a1[0] = Snmpman.atomVarbind;
                a1[1] = new OtpErlangString(oidVal);
                a1[2] = new OtpErlangInt(syntax);
                a1[3] = varObj;
                OtpErlangTuple encapTuple = new OtpErlangTuple(a1);
                bindsAcc.add(encapTuple);
            }
        }
        return true;
    }

    public void finished(TreeEvent event)
    {
        if (event.getStatus() == RetrievalEvent.STATUS_EXCEPTION)
        {
            Snmpman.sendReply(to, 
                    Snmpman.buildErrorReply(Snmpman.atomException));
        }
        else if (event.getStatus() == RetrievalEvent.STATUS_OK)
        {
            Snmpman.sendReply(to, Snmpman.buildBindsTuple(bindsAcc));
        }
        else if (event.getStatus() == RetrievalEvent.STATUS_REPORT)
        {
            OtpErlangObject[] snmpReply = new OtpErlangObject[2];
            snmpReply[0]    = Snmpman.atomReport;
            snmpReply[1]    = Snmpman.getReport(event.getReportPDU());
            OtpErlangTuple snmpReplyTuple = new OtpErlangTuple(snmpReply);
            Snmpman.sendReply(to, Snmpman.buildErrorReply(snmpReplyTuple));
        }
        else if (event.getStatus() == RetrievalEvent.STATUS_TIMEOUT)
        {
            Snmpman.sendReply(to, Snmpman.buildErrorReply(Snmpman.atomTimeout));
        }
        else if (event.getStatus() == RetrievalEvent.STATUS_WRONG_ORDER)
        {
            Snmpman.sendReply(to, Snmpman.buildErrorReply(Snmpman.atomWrongOrder));
        }

        finished = true;
    }
    
    public boolean isFinished()
    {
        return finished;
    }
}


class SnmpmanTableListener implements TableListener
{
    private boolean         finished;
    private OtpErlangObject to       = null;
    private ArrayList<OtpErlangTuple> tableAcc = new ArrayList<OtpErlangTuple>();

    public SnmpmanTableListener(OtpErlangObject replyTo)
    {
        to = replyTo;
    }

    public boolean next(TableEvent event)
    {
        VariableBinding[] vbs = event.getColumns();
        OtpErlangObject[] tableRow = new OtpErlangObject[vbs.length+1];
        tableRow[0] = Snmpman.atomTableRow;


        for (int i=0; i<vbs.length; i++) {

            tableRow[i+1] = Snmpman.getValue(vbs[i]);
        }
        OtpErlangTuple tupleRow = new OtpErlangTuple(tableRow);
        tableAcc.add(tupleRow);
        return true;
    }

    public void finished(TableEvent event)
    {
        if (event.getStatus() == RetrievalEvent.STATUS_EXCEPTION)
        {
            Snmpman.sendReply(to,
                    Snmpman.buildErrorReply(Snmpman.atomException));
        }
        else if (event.getStatus() == RetrievalEvent.STATUS_OK)
        {
            Snmpman.sendReply(to,
                    Snmpman.buildTableTuple(tableAcc));
        }
        else if (event.getStatus() == RetrievalEvent.STATUS_REPORT)
        {
            OtpErlangObject[] snmpReply = new OtpErlangObject[2];
            snmpReply[0]    = Snmpman.atomReport;
            snmpReply[1]    = Snmpman.getReport(event.getReportPDU());
            OtpErlangTuple snmpReplyTuple = new OtpErlangTuple(snmpReply);
            Snmpman.sendReply(to, Snmpman.buildErrorReply(snmpReplyTuple));
        }
        else if (event.getStatus() == RetrievalEvent.STATUS_TIMEOUT)
        {
            Snmpman.sendReply(to,
                    Snmpman.buildErrorReply(Snmpman.atomTimeout));
        }
        else if (event.getStatus() == RetrievalEvent.STATUS_WRONG_ORDER)
        {
            Snmpman.sendReply(to,
                    Snmpman.buildErrorReply(Snmpman.atomWrongOrder));
        }
        finished = true;
    }

    public boolean isFinished()
    {
        return finished;
    }
}


class GetNextPDUFactory implements PDUFactory
{
    int pduType = PDU.GETNEXT;

    public PDU createPDU(Target target) {
        PDU request;
        if (target.getVersion() == SnmpConstants.version3) {
            request = new ScopedPDU();
            ScopedPDU scopedPDU = (ScopedPDU)request;
        }
        else {
            request = new PDU();
        }
        request.setType(pduType);
        return request;
    }

    public PDU createPDU(MessageProcessingModel messageProcessingModel) {
        return createPDU((Target)null);
    }
}
