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
 * along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
 */

package io.sysmo.jserver;

import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpMbox;

import org.snmp4j.*;
import org.snmp4j.event.*;
import org.snmp4j.mp.*;
import org.snmp4j.security.*;
import org.snmp4j.smi.*;
import org.snmp4j.transport.*;
import org.snmp4j.util.*;
// AES192_3DES AES256_3DES
import org.snmp4j.security.nonstandard.*;


public class SnmpManager implements Runnable
{

    public static final OtpErlangAtom atomReply = new OtpErlangAtom("reply");
    public static final OtpErlangAtom atomOk = new OtpErlangAtom("ok");
    public static final OtpErlangAtom atomReport = new OtpErlangAtom("report");
    public static final OtpErlangAtom atomTable  = new OtpErlangAtom("table");
    public static final OtpErlangAtom atomError = new OtpErlangAtom("error");
    public static final OtpErlangAtom atomTimeout =
                                        new OtpErlangAtom("timeout");
    public static final OtpErlangAtom atomVarbinds =
                                        new OtpErlangAtom("varbinds");
    public static final OtpErlangAtom atomVarbind =
                                        new OtpErlangAtom("varbind");
    public static final OtpErlangAtom atomUnknownTarget =
                                        new OtpErlangAtom("unknown_target");
    public static final OtpErlangAtom atomTargetExist =
                                        new OtpErlangAtom("target_exist");
    public static final OtpErlangAtom atomTableRow =
                                        new OtpErlangAtom("table_row");
    public static final OtpErlangAtom atomException =
                                        new OtpErlangAtom("exception");
    public static final OtpErlangAtom atomWrongOrder =
                                        new OtpErlangAtom("wrong_order");


    private static char[] hexArray = "0123456789ABCDEF".toCharArray();

    private static SnmpManager instance;
    private static final Object lock = new Object();

    private OtpMbox mbox;
    private String nodeName;
    private Snmp snmp4jSession;
    private Map<String, SnmpmanElement> snmpmanElements;
    private Logger logger;

    public SnmpManager(final OtpMbox mbox, final String nodeName)
    {
        SnmpManager.instance = this;
        this.snmpmanElements = new HashMap<>();
        this.nodeName = nodeName;
        this.mbox = mbox;
        this.logger = LoggerFactory.getLogger(SnmpManager.class);
    }

    @Override
    public void run()
    {
        String engineIdFile = FileSystems
                        .getDefault()
                        .getPath("etc", "engine.id")
                        .toString();

        // may take a wile? /dev/random ?
        this.logger.info("build engine id " + this.nodeName);
        try {
            Path engineIdPath = Paths.get(engineIdFile);
            byte[] engineId = this.getEngineId(engineIdPath);
            DefaultUdpTransportMapping transport =
                    new DefaultUdpTransportMapping();
            this.snmp4jSession = new Snmp(transport);
            USM usm = new USM(SecurityProtocols.getInstance(),
                                    new OctetString(engineId), 0);
            SecurityModels.getInstance().addSecurityModel(usm);
            transport.listen();
        } catch (Exception|Error e) {
            this.logger.error(e.toString());
            return;
        }

        this.logger.info("begin to loop " + this.nodeName);
        // then begin to loop and wait for calls
        OtpErlangObject call;
        while (true) try
        {
            call = this.mbox.receive();
            this.handleMsg(call);
        }
        catch (OtpErlangExit|OtpErlangDecodeException e)
        {
            this.logger.error(e.toString());
            break;
        }
    }

    private byte[] getEngineId(Path path) throws Exception, Error
    {
        if (Files.isRegularFile(path)) {
            byte[] engineIdDump = Files.readAllBytes(path);
            String engineIdHex = new String(engineIdDump, "UTF-8");
            return SnmpManager.hexStringToBytes(engineIdHex);
        } else {
            byte[] engineId = MPv3.createLocalEngineID();
            String engineIdHex = SnmpManager.bytesToHexString(engineId);
            byte[] engineIdDump = engineIdHex.getBytes("UTF-8");
            Files.write(path, engineIdDump);
            return engineId;
        }
    }

    /**
     * Send a message to the caller.
     * Used by SnmpmanResponseListener SnmpmanTreeListener and SnmpmanTableListener
     * which are executed in another thread.
     */
    public static void sendReply(
            final OtpErlangObject to, final OtpErlangObject msg)
    {
        OtpErlangObject[] obj = new OtpErlangObject[3];
        obj[0] = SnmpManager.atomReply;
        obj[1] = to;
        obj[2] = msg;
        OtpErlangTuple tuple = new OtpErlangTuple(obj);
        synchronized(SnmpManager.lock)
        {
            SnmpManager.instance.mbox.send(
                    "snmpman", SnmpManager.instance.nodeName, tuple);
        }
    }

    private void handleMsg(final OtpErlangObject msg)
    {
        OtpErlangTuple  tuple;
        OtpErlangAtom   command;
        OtpErlangObject caller;
        OtpErlangTuple  payload;
        try {
            tuple       = (OtpErlangTuple) msg;
            command     = (OtpErlangAtom)   (tuple.elementAt(0));
            caller      =                   (tuple.elementAt(1));
            payload     = (OtpErlangTuple)  (tuple.elementAt(2));
        } catch (Exception|Error e) {
            this.logger.warn(e.toString());
            return;
        }

        try
        {
            switch (command.toString())
            {
                case "get":
                    this.handleGet(caller, payload);
                    break;

                case "walk_tree":
                    this.handleWalkTree(caller, payload);
                    break;

                case "walk_table":
                    this.handleWalkTable(caller, payload);
                    break;

                case "discovery":
                    this.handleDiscovery(caller, payload);
                    break;

                case "register_element":
                    this.handleRegisterElement(caller, payload);
                    break;

                case "unregister_element":
                    this.handleUnregisterElement(caller, payload);
                    break;

                case "which_elements":
                    this.handleWhichElements(caller);
                    break;

                case "which_usm_users":
                    this.handleWhichUSMUsers(caller);
                    break;
                default:
                    throw new Exception("Unknown command: " + command.toString());
            }

        } catch (Exception|Error e) {

            OtpErlangTuple reply = SnmpManager.buildErrorReply(
                    new OtpErlangString("Java CATCH: Failed to honour command "
                            + command.toString() + " -> " + e.getMessage())
            );
            this.logger.warn(e.toString());
            SnmpManager.sendReply(caller, reply);
        }
    }

    /**
     * Register a new element.
     * High level API form snmpman.
     */
    private void handleRegisterElement(
            final OtpErlangObject caller,
            final OtpErlangTuple confTuple) throws Exception, Error
    {
        RegisterArgs registerArgs = new RegisterArgs(confTuple);
        SnmpmanElement element = new SnmpmanElement();

        if (this.snmpmanElements.get(registerArgs.elementName) != null) {
            OtpErlangTuple reply =
                    SnmpManager.buildErrorReply(SnmpManager.atomTargetExist);
            SnmpManager.sendReply(caller, reply);
            return;
        }

        switch (registerArgs.snmpVersion)
        {
            case "3":
                element.setTarget(generateTarget(registerArgs));
                element.setName(registerArgs.elementName);
                if (registerUSMUser(registerArgs)) {
                    this.snmpmanElements.put(element.getName(), element);
                    SnmpManager.sendReply(caller, atomOk);
                    break;
                } else {
                    OtpErlangTuple reply = SnmpManager.buildErrorReply(
                            new OtpErlangString("USM user definition do not match a predefined entry")
                    );
                    SnmpManager.sendReply(caller, reply);
                    break;
                }
            default:
                element.setTarget(this.generateTarget(registerArgs));
                element.setName(registerArgs.elementName);
                this.snmpmanElements.put(element.getName(), element);
                SnmpManager.sendReply(caller, atomOk);
                break;
        }
    }


    /**
     * Unregister a new element.
     * High level API form snmpman. Delete the target and for snmp v3, the
     * localized USM user from the USM table.
     */
    private void handleUnregisterElement(
            final OtpErlangObject caller,
            final OtpErlangTuple unregArg) throws Exception, Error
    {
        OtpErlangString elementName = (OtpErlangString) (unregArg.elementAt(0));
        SnmpmanElement element =
                this.snmpmanElements.remove(elementName.stringValue());

        if (element == null) {
            SnmpManager.sendReply(caller,
                    SnmpManager.buildErrorReply(SnmpManager.atomUnknownTarget));
            return;
        }

        if (element.getTarget().getVersion() == SnmpConstants.version3) {
            OctetString secUser = element.getSecurityName();
            boolean deleteUsm = true;
            for (Map.Entry<String, SnmpmanElement> remaining:
                                        this.snmpmanElements.entrySet())
            {
                if (remaining.getValue().getSecurityName().equals(secUser)) {
                    deleteUsm = false;
                    break;
                }
            }
            if (deleteUsm) {
                this.snmp4jSession.getUSM().removeAllUsers(secUser);
            }
        }

        SnmpManager.sendReply(caller, atomOk);
    }

    private void handleGet(
            final OtpErlangObject caller,
            final OtpErlangTuple tuple) throws Exception, Error
    {
        OtpErlangString elementName = (OtpErlangString) (tuple.elementAt(0));
        OtpErlangList oidList = (OtpErlangList) (tuple.elementAt(1));

        if (!this.snmpmanElements.containsKey(elementName.stringValue())) {
            SnmpManager.sendReply(caller,
                    SnmpManager.buildErrorReply(SnmpManager.atomUnknownTarget));
            return;
        }

        OID[] oidFinalList = new OID[oidList.arity()];

        for (int i =0; i<oidList.arity(); i++)
        {
            OtpErlangString oidString = (OtpErlangString) (oidList.elementAt(i));
            OID oid = new OID(oidString.stringValue());
            if (!oid.isValid())
            {
                sendReply(caller,
                    SnmpManager.buildErrorReply(
                            new OtpErlangString(
                                    "invalid OID: " + oidString.stringValue()
                            )
                    )
                    );
                return;
            }
            oidFinalList[i] = oid;
        }

        Target target = this.snmpmanElements.get(elementName.stringValue()).getTarget();
        PDU pdu;
        if (target.getVersion() == SnmpConstants.version3) {
            pdu = new ScopedPDU();
        } else {
            pdu = new PDU();
        }
        pdu.setType(PDU.GET);

        for (int i=0; i <oidFinalList.length; i++)
        {
            pdu.add(new VariableBinding(oidFinalList[i]));
        }

        ResponseListener listener = new SnmpmanResponseListener(caller);
        this.snmp4jSession.send(pdu, target, null, listener);
    }


    private boolean registerUSMUser(
            final RegisterArgs registerArgs) throws Exception, Error
    {

        OID authProtoOid;
        switch (registerArgs.authProto)
        {
            case "SHA": authProtoOid = AuthSHA.ID; break;
            case "MD5": authProtoOid = AuthMD5.ID; break;
            default:
                throw new Exception("Unknown authentication protocol");
        }

        OID privProtoOid;
        switch (registerArgs.privProto)
        {
            case "AES":         privProtoOid = PrivAES128.ID; break;
            case "AES192":      privProtoOid = PrivAES192.ID; break;
            case "AES256":      privProtoOid = PrivAES256.ID; break;
            case "DES":         privProtoOid = PrivDES.ID;    break;
            case "3DES":        privProtoOid = Priv3DES.ID;   break;
            case "AES192_3DES": privProtoOid = PrivAES192With3DESKeyExtension.ID; break;
            case "AES256_3DES": privProtoOid = PrivAES256With3DESKeyExtension.ID; break;
            default:
                throw new Exception("Unknown private protocol");
        }

        OctetString uName = new OctetString(registerArgs.secName);
        UsmUser newUsmUser = new UsmUser(
            uName,
            authProtoOid,
            new OctetString (registerArgs.authKey),
            privProtoOid,
            new OctetString (registerArgs.privKey)
        );

        UsmUserEntry existUsmUser =
                    this.snmp4jSession.getUSM().getUserTable().getUser(uName);

        if (existUsmUser == null) {
            this.snmp4jSession.getUSM().addUser(newUsmUser);
            return true;
        }  else { // usm user exist
            return this.usmUsersEquals(existUsmUser.getUsmUser(), newUsmUser);
        }
    }

    private boolean usmUsersEquals(UsmUser a, UsmUser b)
    {
        return a.toString().equals(b.toString());
    }

    private AbstractTarget generateTarget(
                final RegisterArgs registerArgs) throws Exception, Error
    {
        Address targetAddress = GenericAddress.parse(
                "udp:" + registerArgs.host + "/" + registerArgs.ipPort);

        int securityLevelConst;
        switch (registerArgs.secLevel)
        {
            case "authPriv":
                securityLevelConst = SecurityLevel.AUTH_PRIV; break;
            case "authNoPriv":
                securityLevelConst = SecurityLevel.AUTH_NOPRIV; break;
            case "noAuthNoPriv":
                securityLevelConst = SecurityLevel.NOAUTH_NOPRIV; break;
            default:
                throw new Exception("Unknown security level");
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

    private void handleDiscovery(
            final OtpErlangObject caller,
            final OtpErlangTuple tuple) throws Exception, Error
    {
        OtpErlangString targetIp = (OtpErlangString) (tuple.elementAt(0));
        OtpErlangLong targetPort = (OtpErlangLong)   (tuple.elementAt(2));
        OtpErlangLong timeout =    (OtpErlangLong)   (tuple.elementAt(3));

        Address targetAddress = GenericAddress.parse(
                "udp:" + targetIp.stringValue() + "/" + targetPort.intValue());

        byte[] engineId = this.snmp4jSession.discoverAuthoritativeEngineID(
                                            targetAddress, timeout.intValue());
        if (engineId == null) {
            SnmpManager.sendReply(
                    caller, SnmpManager.buildErrorReply(SnmpManager.atomTimeout));
        } else {
            String hexEngineID = SnmpManager.bytesToHexString(engineId);
            OtpErlangString hexString = new OtpErlangString(hexEngineID);
            sendReply(caller, SnmpManager.buildOkReply(hexString));
        }
    }

    private void handleWalkTable(
            final OtpErlangObject caller,
            final OtpErlangTuple tuple) throws Exception, Error
    {
        OtpErlangString elementName = (OtpErlangString) (tuple.elementAt(0));
        OtpErlangList oidList =       (OtpErlangList)   (tuple.elementAt(1));



        if (!this.snmpmanElements.containsKey(elementName.stringValue())) {
            SnmpManager.sendReply(caller, SnmpManager.buildErrorReply(atomUnknownTarget));
            return;
        }

        OtpErlangObject[] elements    = oidList.elements();
        OID[]             columnOIDs  = new OID[elements.length];

        for (int i=0; i<elements.length; i++)
        {
            OtpErlangString val = (OtpErlangString) (elements[i]);
            columnOIDs[i] = new OID(val.stringValue());
        }

        OID lowerBoundIndex = null;
        OID upperBoundIndex = null;

        Target target =
                this.snmpmanElements.get(elementName.stringValue()).getTarget();

        TableUtils tableUtils =
                new TableUtils(this.snmp4jSession, new GetNextPDUFactory());

        TableListener listener = new SnmpmanTableListener(caller);
        tableUtils.getTable(target, columnOIDs,
                listener, null, lowerBoundIndex, upperBoundIndex);
    }


    private void handleWalkTree(
            final OtpErlangObject caller,
            final OtpErlangTuple tuple) throws Exception, Error
    {
        OtpErlangString elementName = (OtpErlangString) (tuple.elementAt(0));
        OtpErlangString oidString   = (OtpErlangString) (tuple.elementAt(1));

        if (!snmpmanElements.containsKey(elementName.stringValue())) {
            SnmpManager.sendReply(caller, SnmpManager.buildErrorReply(atomUnknownTarget));
            return;
        }

        OID walkOid = new OID(oidString.stringValue());
        if (!walkOid.isValid()) {
            OtpErlangTuple objTuple = SnmpManager.buildErrorReply(
                    new OtpErlangString("invalid OID: " + oidString.stringValue())
            );
            SnmpManager.sendReply(caller, objTuple);
            return;
        }

        Target target =
                this.snmpmanElements.get(elementName.stringValue()).getTarget();
        TreeUtils treeUtils =
                new TreeUtils(this.snmp4jSession, new GetNextPDUFactory());
        TreeListener treeListener = new SnmpmanTreeListener(caller);
        treeUtils.getSubtree(target, walkOid, null, treeListener);
    }

    private void handleWhichElements(
            final OtpErlangObject caller) throws Exception, Error
    {
        int size = this.snmpmanElements.size();
        OtpErlangObject[] replyObj = new OtpErlangObject[size];
        int i = 0;
        for (String key: this.snmpmanElements.keySet())
        {
            replyObj[i] = new OtpErlangString(key);
            i += 1;
        }
        OtpErlangList replyList = new OtpErlangList(replyObj);

        SnmpManager.sendReply(caller, SnmpManager.buildOkReply(replyList));
    }

    private void handleWhichUSMUsers(
            final OtpErlangObject caller) throws Exception, Error
    {
        List<UsmUserEntry> usmUsers =
            this.snmp4jSession.getUSM().getUserTable().getUserEntries();

        int size = usmUsers.size();
        OtpErlangObject[] replyObj = new OtpErlangObject[size];

        for (int i=0; i<usmUsers.size(); i++)
        {
            replyObj[i] =
                new OtpErlangString(usmUsers.get(i).getUserName().toString());
        }
        OtpErlangList replyList = new OtpErlangList(replyObj);
        SnmpManager.sendReply(caller, SnmpManager.buildOkReply(replyList));
    }

    public static OtpErlangTuple buildOkReply(OtpErlangObject msg)
    {
        OtpErlangObject[] valObj   = new OtpErlangObject[2];
        valObj[0] = atomOk;
        valObj[1] = msg;
        return new OtpErlangTuple(valObj);
    }

    public static OtpErlangTuple buildErrorReply(OtpErlangObject msg)
    {
        OtpErlangObject[] valObj   = new OtpErlangObject[2];
        valObj[0] = atomError;
        valObj[1] = msg;
        return new OtpErlangTuple(valObj);
    }

    public static OtpErlangTuple buildBindsTuple(
            final ArrayList<OtpErlangTuple> bindsAcc)
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
        vbValObj[0] = SnmpManager.atomVarbinds;
        vbValObj[1] = allBinds;
        OtpErlangTuple vbValTuple = new OtpErlangTuple(vbValObj);

        return SnmpManager.buildOkReply(vbValTuple);
    }

    public static OtpErlangTuple buildTableTuple(
            final ArrayList<OtpErlangTuple> bindsAcc)
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
        vbValObj[0] = SnmpManager.atomTable;
        vbValObj[1] = allBinds;
        OtpErlangTuple vbValTuple = new OtpErlangTuple(vbValObj);

        return SnmpManager.buildOkReply(vbValTuple);
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
            hexChars[j * 2] = SnmpManager.hexArray[v >>> 4];
            hexChars[j * 2 + 1] = SnmpManager.hexArray[v & 0x0F];
        }
        return new String(hexChars);
    }
}

class SnmpmanElement
{
    private AbstractTarget target;
    private OctetString securityName;
    private String targetName;

    // target get and set
    public void setTarget(AbstractTarget target) {
        this.target = target;
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

    public RegisterArgs(final OtpErlangTuple confTuple) throws Exception, Error
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

        this.elementName = erlElementName.stringValue();
        this.host        = erlHost.stringValue();
        this.snmpVersion = erlSnmpVersion.stringValue();
        this.secLevel    = erlSecLevel.stringValue();
        this.secName     = erlSecName.stringValue();
        this.community   = erlCommunity.stringValue();
        this.authProto   = erlAuthProto.stringValue();
        this.authKey     = erlAuthKey.stringValue();
        this.privProto   = erlPrivProto.stringValue();
        this.privKey     = erlPrivKey.stringValue();
        this.ipPort      = erlIpPort.intValue();
        this.retries     = erlRetries.intValue();
        this.timeout     = erlTimeout.intValue();
    }
}

class SnmpmanResponseListener implements ResponseListener
{
    private OtpErlangObject to;

    public  SnmpmanResponseListener(final OtpErlangObject to) {
        this.to = to;
    }

    public void onResponse(ResponseEvent event)
    {
        ((Snmp)event.getSource()).cancel(event.getRequest(), this);
        PDU rep = event.getResponse();
        if (rep == null)
        {
            SnmpManager.sendReply(to,
                    SnmpManager.buildErrorReply(SnmpManager.atomTimeout));
        }
        else if (rep.getType() == PDU.REPORT)
        {
            OtpErlangObject[] snmpReply = new OtpErlangObject[2];
            snmpReply[0]    = SnmpManager.atomReport;
            snmpReply[1]    = SnmpManager.getReport(rep);
            OtpErlangTuple snmpReplyTuple = new OtpErlangTuple(snmpReply);
            SnmpManager.sendReply(to, snmpReplyTuple);
        }
        else
        {
            Vector<? extends VariableBinding> vbs = rep.getVariableBindings();
            ArrayList<OtpErlangTuple> bindsAcc = new ArrayList<>();
            for (VariableBinding vbv: vbs)
            {
                String   oidVal = vbv.getOid().toString();
                Variable var    = vbv.getVariable();
                int      syntax = var.getSyntax();
                OtpErlangObject varObj = SnmpManager.getValue(vbv);

                OtpErlangObject[] a1 = new OtpErlangObject[4];
                a1[0] = SnmpManager.atomVarbind;
                a1[1] = new OtpErlangString(oidVal);
                a1[2] = new OtpErlangInt(syntax);
                a1[3] = varObj;
                OtpErlangTuple encapTuple = new OtpErlangTuple(a1);
                bindsAcc.add(encapTuple);
            }
            SnmpManager.sendReply(to, SnmpManager.buildBindsTuple(bindsAcc));
        }
    }
}

class SnmpmanTreeListener implements TreeListener
{
    private boolean finished;
    private OtpErlangObject to;
    private ArrayList<OtpErlangTuple> bindsAcc;

    public SnmpmanTreeListener(OtpErlangObject to)
    {
        this.to = to;
        this.finished = false;
        this.bindsAcc = new ArrayList<>();
    }

    public boolean next(TreeEvent event)
    {
        if (event.getVariableBindings() != null) {
            VariableBinding[] vbs = event.getVariableBindings();
            for (VariableBinding vb : vbs)
            {
                String   oidVal = vb.getOid().toString();
                Variable var    = vb.getVariable();
                int      syntax = var.getSyntax();
                OtpErlangObject varObj = SnmpManager.getValue(vb);

                OtpErlangObject[] a1 = new OtpErlangObject[4];
                a1[0] = SnmpManager.atomVarbind;
                a1[1] = new OtpErlangString(oidVal);
                a1[2] = new OtpErlangInt(syntax);
                a1[3] = varObj;
                OtpErlangTuple encapTuple = new OtpErlangTuple(a1);
                this.bindsAcc.add(encapTuple);
            }
        }
        return true;
    }

    public void finished(TreeEvent event)
    {
        if (event.getStatus() == RetrievalEvent.STATUS_EXCEPTION) {
            SnmpManager.sendReply(to,
                    SnmpManager.buildErrorReply(SnmpManager.atomException));

        } else if (event.getStatus() == RetrievalEvent.STATUS_OK) {
            SnmpManager.sendReply(to,
                    SnmpManager.buildBindsTuple(this.bindsAcc));

        } else if (event.getStatus() == RetrievalEvent.STATUS_REPORT) {
            OtpErlangObject[] snmpReply = new OtpErlangObject[2];
            snmpReply[0]    = SnmpManager.atomReport;
            snmpReply[1]    = SnmpManager.getReport(event.getReportPDU());
            OtpErlangTuple snmpReplyTuple = new OtpErlangTuple(snmpReply);
            SnmpManager.sendReply(to,
                    SnmpManager.buildErrorReply(snmpReplyTuple));

        } else if (event.getStatus() == RetrievalEvent.STATUS_TIMEOUT) {
            SnmpManager.sendReply(to,
                    SnmpManager.buildErrorReply(SnmpManager.atomTimeout));

        } else if (event.getStatus() == RetrievalEvent.STATUS_WRONG_ORDER) {
            SnmpManager.sendReply(to,
                    SnmpManager.buildErrorReply(SnmpManager.atomWrongOrder));
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
    private boolean finished;
    private OtpErlangObject to;
    private ArrayList<OtpErlangTuple> tableAcc;

    public SnmpmanTableListener(OtpErlangObject to)
    {
        this.finished = false;
        this.to = to;
        this.tableAcc = new ArrayList<>();
    }

    public boolean next(TableEvent event)
    {
        VariableBinding[] vbs = event.getColumns();
        OtpErlangObject[] tableRow = new OtpErlangObject[vbs.length+1];
        tableRow[0] = SnmpManager.atomTableRow;


        for (int i=0; i<vbs.length; i++) {

            tableRow[i+1] = SnmpManager.getValue(vbs[i]);
        }
        OtpErlangTuple tupleRow = new OtpErlangTuple(tableRow);
        this.tableAcc.add(tupleRow);
        return true;
    }

    public void finished(TableEvent event)
    {
        if (event.getStatus() == RetrievalEvent.STATUS_EXCEPTION) {
            SnmpManager.sendReply(to,
                    SnmpManager.buildErrorReply(SnmpManager.atomException));

        } else if (event.getStatus() == RetrievalEvent.STATUS_OK) {
            SnmpManager.sendReply(to,
                    SnmpManager.buildTableTuple(this.tableAcc));

        } else if (event.getStatus() == RetrievalEvent.STATUS_REPORT) {
            OtpErlangObject[] snmpReply = new OtpErlangObject[2];
            snmpReply[0]    = SnmpManager.atomReport;
            snmpReply[1]    = SnmpManager.getReport(event.getReportPDU());
            OtpErlangTuple snmpReplyTuple = new OtpErlangTuple(snmpReply);
            SnmpManager.sendReply(to,
                    SnmpManager.buildErrorReply(snmpReplyTuple));

        } else if (event.getStatus() == RetrievalEvent.STATUS_TIMEOUT) {
            SnmpManager.sendReply(to,
                    SnmpManager.buildErrorReply(SnmpManager.atomTimeout));

        } else if (event.getStatus() == RetrievalEvent.STATUS_WRONG_ORDER) {
            SnmpManager.sendReply(to,
                    SnmpManager.buildErrorReply(SnmpManager.atomWrongOrder));
        }

        this.finished = true;
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
        } else {
            request = new PDU();
        }
        request.setType(pduType);
        return request;
    }

    public PDU createPDU(MessageProcessingModel messageProcessingModel) {
        return createPDU((Target)null);
    }
}
