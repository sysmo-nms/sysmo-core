package io.noctopus.equartz;
import io.noctopus.equartz.EQuartzMessageHandler;
import java.util.Date;
import java.io.IOException;
import java.nio.file.Path;


import com.ericsson.otp.erlang.*;

public class EQuartzNode extends Thread {

    private static String selfNodeName;

    // the foreign node name (-sname)
    private static String foreignNodeName;

    // the foreign equartz.erl gen_server pid name
    private static String foreignPidName;

    public static OtpErlangAtom atomReply   = new OtpErlangAtom("reply");
    public static OtpErlangAtom atomOk      = new OtpErlangAtom("ok");
    public static OtpErlangAtom atomTrue    = new OtpErlangAtom("true");
    public static OtpErlangAtom atomFalse   = new OtpErlangAtom("false");
    public static OtpErlangAtom atomTimeout = new OtpErlangAtom("timeout");
    public static OtpErlangAtom atomError   = new OtpErlangAtom("error");
    public static OtpErlangAtom atomFire    = new OtpErlangAtom("fire");
    public static OtpErlangAtom atomNotConnected = new OtpErlangAtom("not_connected");

    // erlang server
    private static  OtpNode self = null;
    private static  OtpMbox mbox = null;

    private static EQuartzMessageHandler quartzHandler;

    public EQuartzNode(String selfName, String fNodeName, String fPidName)
    {
        selfNodeName    = selfName;
        foreignNodeName = fNodeName;
        foreignPidName  = fPidName;
    }

    @Override
    public void run() 
    {

        // Initialize otp
        try 
        {
            self = new OtpNode(selfNodeName);
            mbox = self.createMbox();
            if (!self.ping(foreignNodeName, 2000)) 
            { 
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
