package io.sysmo.equartz;
import io.sysmo.equartz.JobInternal;
import io.sysmo.equartz.EQuartzNode;
import io.sysmo.equartz.EQuartzMessageHandler;

import java.util.Date;
import java.util.Set;
import java.util.Properties;
import java.util.Scanner;
import java.io.IOException;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.File;
import java.nio.file.Path;


import com.ericsson.otp.erlang.*;

import org.quartz.*;
import org.quartz.impl.StdSchedulerFactory;
import org.quartz.Scheduler;
import org.quartz.impl.matchers.GroupMatcher;
import static org.quartz.JobBuilder.*;
import static org.quartz.TriggerBuilder.*;
import static org.quartz.CronScheduleBuilder.*;
import static org.quartz.SimpleScheduleBuilder.*;


public class EQuartz {
    private static Scheduler        scheduler;
    private static EQuartzNode      enode;

    public static void main(String[] args) {

        // Initialize quartz
        try {
            // Grab the Scheduler instance from the Factory 
            scheduler = StdSchedulerFactory.getDefaultScheduler();

            // and start it off
            scheduler.start();

        }
        catch (SchedulerException e)
        {
            e.printStackTrace();
        }


        // get conf
        String selfNodeName;
        String foreignNodeName;
        String foreignPidName;
        String erlangCookie;
        try
        {
            Properties   prop  = new Properties();
            InputStream  input = new FileInputStream("cfg/equartz.properties");
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

        try
        {
            erlangCookie = new Scanner(new File("cfg/sysmo.cookie"), "UTF-8").useDelimiter("\\Z").next();
            
        }
        catch(IOException e)
        {
            e.printStackTrace();
            return;
        }



        // initialize erlang node
        enode = new EQuartzNode(selfNodeName, foreignNodeName, foreignPidName, erlangCookie);
        enode.setMsgHandler(new MessageHandler());
        enode.start();
        
    }

    public static void callWhichJobs(OtpErlangObject caller)
    {
        Set<JobKey> jobKeys;
        try
        {
            jobKeys = scheduler.getJobKeys(
                GroupMatcher.jobGroupEquals(Scheduler.DEFAULT_GROUP)
            );
        }
        catch (SchedulerException e)
        {
            e.printStackTrace();
            EQuartzNode.sendReply(caller,
                EQuartzNode.buildErrorReply(new OtpErlangString(e.getMessage()))
            );
            return;

        }
        OtpErlangObject[] replyObj = new OtpErlangObject[jobKeys.size()];
        int i = 0;
        for (JobKey jobKey: jobKeys)
        {
            replyObj[i] = new OtpErlangString(jobKey.getName());
            i++;
        }
        OtpErlangList replyList = new OtpErlangList(replyObj);

        EQuartzNode.sendReply(caller, EQuartzNode.buildOkReply(replyList));
    }

    public static void callRegisterSystemJob(
            OtpErlangObject caller,
            OtpErlangTuple  payload)
    {
        EQuartzNode.sendReply(caller, EQuartzNode.buildErrorReply(new OtpErlangString("not implemented")));
    }

    public static void callRegisterInternalJob(
            OtpErlangObject caller,
            OtpErlangTuple  payload)
    {
        OtpErlangString jobName = (OtpErlangString) (payload.elementAt(0));
        OtpErlangString cronDef = (OtpErlangString) (payload.elementAt(1));
        OtpErlangTuple  jobDef  = (OtpErlangTuple)  (payload.elementAt(2));
        OtpErlangAtom   jobMod  = (OtpErlangAtom)   (jobDef.elementAt(0));
        OtpErlangAtom   jobFun  = (OtpErlangAtom)   (jobDef.elementAt(1));
        OtpErlangString jobArg  = (OtpErlangString) (jobDef.elementAt(2));

        JobDetail intJob = newJob(JobInternal.class)
            .withIdentity(jobName.stringValue())
            .build();
        JobDataMap intJobData = intJob.getJobDataMap();
        intJobData.put(JobInternal.MOD, jobMod.toString());
        intJobData.put(JobInternal.FUN, jobFun.toString());
        intJobData.put(JobInternal.ARG, jobArg.stringValue());

        CronTrigger trigger = newTrigger()
            .withIdentity("trigger-" + jobName.stringValue())
            .withSchedule(cronSchedule(cronDef.stringValue()))
            .build();

        try
        {
            scheduler.scheduleJob(intJob, trigger);
        }
        catch (SchedulerException e)
        {
            e.printStackTrace();
            EQuartzNode.sendReply(caller,
                EQuartzNode.buildErrorReply(new OtpErlangString(e.getMessage()))
            );
            return;
        }

        EQuartzNode.sendReply(caller, EQuartzNode.atomOk);
    }

    public static void callDeleteJob(
            OtpErlangObject caller,
            OtpErlangTuple  payload)
    {

        OtpErlangString jobName = (OtpErlangString) (payload.elementAt(0));
        boolean success;
        try
        {
            success = scheduler.deleteJob(new JobKey(jobName.stringValue()));
        }
        catch (SchedulerException e)
        {
            e.printStackTrace();
            EQuartzNode.sendReply(caller,
                EQuartzNode.buildErrorReply(new OtpErlangString(e.getMessage()))
            );
            return;
        }

        System.out.println("job deleted?: " + success);
        if (success == true)
        {
            EQuartzNode.sendReply(caller, EQuartzNode.atomOk);
        }
        else
        {
            EQuartzNode.sendReply(caller,
                EQuartzNode.buildErrorReply(
                    new OtpErlangString(
                        "Job does not exist: " + jobName.stringValue())));
        }
    }

    public static void callJobExists(
            OtpErlangObject caller,
            OtpErlangTuple  payload)
    {
        OtpErlangString jobName = (OtpErlangString) (payload.elementAt(0));
        boolean success;
        try
        {
            success = scheduler.checkExists(new JobKey(jobName.stringValue()));
        }
        catch (SchedulerException e)
        {
            e.printStackTrace();
            EQuartzNode.sendReply(caller,
                EQuartzNode.buildErrorReply(new OtpErlangString(e.getMessage()))
            );
            return;
        }

        if (success == true)
        {
            EQuartzNode.sendReply(caller, EQuartzNode.atomTrue);
        }
        else
        {
            EQuartzNode.sendReply(caller, EQuartzNode.atomFalse);
        }
    }

    public static void callFireNow(
            OtpErlangObject caller,
            OtpErlangTuple  payload)
    {
        OtpErlangString jobName = (OtpErlangString) (payload.elementAt(0));
        JobKey jobKey = new JobKey(jobName.stringValue());
        boolean success;
        try
        {
            success = scheduler.checkExists(jobKey);
        }
        catch (SchedulerException e)
        {
            e.printStackTrace();
            EQuartzNode.sendReply(caller,
                EQuartzNode.buildErrorReply(new OtpErlangString(e.getMessage()))
            );
            return;
        }

        if (success != true)
        {
            EQuartzNode.sendReply(caller,
                EQuartzNode.buildErrorReply(
                    new OtpErlangString("Unknown job " + jobKey)
                )
            );
            return;
        }

        try
        {
            scheduler.triggerJob(jobKey);
            EQuartzNode.sendReply(caller, EQuartzNode.atomOk);
        }
        catch (SchedulerException e)
        {
            e.printStackTrace();
            EQuartzNode.sendReply(caller,
                EQuartzNode.buildErrorReply(new OtpErlangString(e.getMessage()))
            );
            return;
        }
    }

    public static void callTerminate()
    {
        try 
        {
            scheduler.shutdown();
        }
        catch (SchedulerException e)
        {
            e.printStackTrace();
        }
    }
    private static void handleMsg(
            OtpErlangAtom   command,
            OtpErlangObject caller,
            OtpErlangTuple  payload)
    {
        System.out.println("hello" + command);
    }
}

class MessageHandler implements EQuartzMessageHandler {
    public void handleMsg(
            OtpErlangAtom   command,
            OtpErlangObject caller,
            OtpErlangTuple  payload)
    {
        switch (command.toString())
        {
            case "which_jobs":
                EQuartz.callWhichJobs(caller);
                break;
            case "register_system_job":
                EQuartz.callRegisterSystemJob(caller, payload);
                break;
            case "register_internal_job":
                EQuartz.callRegisterInternalJob(caller, payload);
                break;
            case "job_exists":
                EQuartz.callJobExists(caller, payload);
                break;
            case "delete_job":
                EQuartz.callDeleteJob(caller, payload);
                break;
            case "fire_now":
                EQuartz.callFireNow(caller, payload);
                break;
            default:
                EQuartzNode.sendReply(caller,
                    EQuartzNode.buildErrorReply(
                                new OtpErlangString(
                                    "unknown command: " + command.toString()))
                );
                break;
        }
    }

    public void handleTerminate()
    {
        System.out.println("should handle termniate");
        EQuartz.callTerminate();
    }
}
