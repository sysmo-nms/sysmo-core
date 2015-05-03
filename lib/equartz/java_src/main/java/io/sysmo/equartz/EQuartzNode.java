package io.sysmo.equartz;
import io.sysmo.equartz.EQuartzMessageHandler;
import java.util.Date;
import java.io.IOException;
import java.nio.file.Path;


import com.ericsson.otp.erlang.*;

public class EQuartzNode extends Thread {

    // TODO singleton
    private static String selfNodeName;

    // the foreign node name (-sname)
    private static String foreignNodeName;

    private static String erlangCookie;

    // the foreign equartz.erl gen_server pid name
    private static String foreignPidName;

    public static final OtpErlangAtom atomReply   = new OtpErlangAtom("reply");
    public static final OtpErlangAtom atomOk      = new OtpErlangAtom("ok");
    public static final OtpErlangAtom atomTrue    = new OtpErlangAtom("true");
    public static final OtpErlangAtom atomFalse   = new OtpErlangAtom("false");
    public static final OtpErlangAtom atomTimeout = new OtpErlangAtom("timeout");
    public static final OtpErlangAtom atomError   = new OtpErlangAtom("error");
    public static final OtpErlangAtom atomFire    = new OtpErlangAtom("fire");
    public static final OtpErlangAtom atomNotConnected = new OtpErlangAtom("not_connected");

    // erlang server
    private static  OtpNode self = null;
    private static  OtpMbox mbox = null;

    private static EQuartzMessageHandler quartzHandler;

    private static String getSelfNodeName() {return selfNodeName;}
    private static String getForeignNodeName() {return foreignNodeName;}
    private static String getForeignPidName() {return foreignPidName;}
    private static String getErlangCookie() {return erlangCookie;}

    private static void setSelfNodeName(String nodeName)
                                                {selfNodeName = nodeName;}
    private static void setForeignNodeName(String foreignName) 
                                                {foreignNodeName = foreignName;}
    private static void setForeignPidName(String foreignPid)
                                                {foreignPidName = foreignPid;}
    private static void setErlangCookie(String cookie)
                                                {erlangCookie = cookie;}

    private static void setMbox(OtpMbox box) { mbox = box; }
    private static void setSelf(OtpNode node) { self = node; }

    public EQuartzNode(String selfName, String fNodeName, String fPidName, String cookie)
    {
        EQuartzNode.setSelfNodeName(selfName);
        EQuartzNode.setForeignNodeName(fNodeName);
        EQuartzNode.setForeignPidName(fPidName);
        EQuartzNode.setErlangCookie(cookie);
    }

    @Override
    public void run() 
    {

        // Initialize otp
        try 
        {
            OtpNode me = new OtpNode(getSelfNodeName(), getErlangCookie());
            setSelf(me);
            setMbox(me.createMbox());
            if (!me.ping(getForeignNodeName(), 2000)) 
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
            quartzHandler.handleTerminate();
            e.printStackTrace();
            break;
        }
        catch (OtpErlangDecodeException e)
        {
            quartzHandler.handleTerminate();
            e.printStackTrace();
        }
    }

    public static void setMsgHandler(EQuartzMessageHandler quartz)
    {
        quartzHandler = quartz;

    }

    private static void handleMsg(OtpErlangObject msg)
    {
        OtpErlangTuple  tuple;
        OtpErlangAtom   command;
        OtpErlangObject caller;
        OtpErlangTuple  payload;
        try
        {
            tuple       = (OtpErlangTuple)  msg;
            command     = (OtpErlangAtom)   (tuple.elementAt(0));
            caller      = (OtpErlangObject) (tuple.elementAt(1));
            payload     = (OtpErlangTuple)  (tuple.elementAt(2));
        }
        catch (Exception|Error e)
        {
            e.printStackTrace();
            return;
        }

        if (quartzHandler == null)
        {
            OtpErlangTuple reply = buildErrorReply(atomNotConnected);
            sendReply(caller, reply);
            return;
        }
        
        quartzHandler.handleMsg(command, caller, payload);
    }

    private static void acknowledgeOtpConnexion()
    {
        OtpErlangObject[] msg = new OtpErlangObject[2];
        msg[0] = mbox.self();
        msg[1] = new OtpErlangAtom("equartz_running");
        OtpErlangTuple tuple  = new OtpErlangTuple(msg);

        mbox.send(foreignPidName, foreignNodeName, tuple);
    }


    private static void handleInit(OtpErlangTuple initMsg)
    {
        OtpErlangString initElement = (OtpErlangString) (initMsg.elementAt(0));
        System.out.println(initElement);
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
    
    public static void fire(
            OtpErlangAtom   m,
            OtpErlangAtom   f,
            OtpErlangString a)
    {
        OtpErlangObject[] obj = new OtpErlangObject[4];
        obj[0] = atomFire;
        obj[1] = m;
        obj[2] = f;
        obj[3] = a;
        OtpErlangTuple tuple = new OtpErlangTuple(obj);
        synchronized(mbox)
        {
            mbox.send(foreignPidName, foreignNodeName, tuple);
        }
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
}
