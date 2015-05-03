package io.sysmo.equartz;
import io.sysmo.equartz.EQuartzMessageHandler;
import java.util.Date;
import java.io.IOException;
import java.nio.file.Path;


import com.ericsson.otp.erlang.*;

public class EQuartzNode extends Thread {


    public static final OtpErlangAtom atomReply   = new OtpErlangAtom("reply");
    public static final OtpErlangAtom atomOk      = new OtpErlangAtom("ok");
    public static final OtpErlangAtom atomTrue    = new OtpErlangAtom("true");
    public static final OtpErlangAtom atomFalse   = new OtpErlangAtom("false");
    public static final OtpErlangAtom atomTimeout = new OtpErlangAtom("timeout");
    public static final OtpErlangAtom atomError   = new OtpErlangAtom("error");
    public static final OtpErlangAtom atomFire    = new OtpErlangAtom("fire");
    public static final OtpErlangAtom atomNotConnected = new OtpErlangAtom("not_connected");


    private String selfNodeName;
    private String foreignNodeName;
    private String erlangCookie;
    private String foreignPidName;
    private OtpNode self;
    private OtpMbox mbox;
    private EQuartzMessageHandler quartzHandler;


    private EQuartzNode() {}
    private static EQuartzNode INSTANCE = new EQuartzNode();

    public static EQuartzNode getInstance() {return INSTANCE;}

    @Override
    public void run() 
    {

        // Initialize otp
        try 
        {
            self = new OtpNode(selfNodeName, erlangCookie);
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

    public void setNodeConfig(String selfName, String fNodeName, String fPidName, String cookie)
    {
        selfNodeName = selfName;
        foreignNodeName = fNodeName;
        foreignPidName = fPidName;
        erlangCookie = cookie;
    }

    public void setMsgHandler(EQuartzMessageHandler quartz)
    {
        quartzHandler = quartz;

    }

    private void handleMsg(OtpErlangObject msg)
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

    private void acknowledgeOtpConnexion()
    {
        OtpErlangObject[] msg = new OtpErlangObject[2];
        msg[0] = mbox.self();
        msg[1] = new OtpErlangAtom("equartz_running");
        OtpErlangTuple tuple  = new OtpErlangTuple(msg);

        mbox.send(foreignPidName, foreignNodeName, tuple);
    }


    private void handleInit(OtpErlangTuple initMsg)
    {
        OtpErlangString initElement = (OtpErlangString) (initMsg.elementAt(0));
        System.out.println(initElement);
    }


    public void sendReply(OtpErlangObject to, OtpErlangObject msg)
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
    
    public void fire(
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
