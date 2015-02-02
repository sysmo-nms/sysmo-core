package io.noctopus.equartz;
import io.noctopus.equartz.EQuartzNode;

import com.ericsson.otp.erlang.*;
import java.util.Date;
import java.util.function.*;
import org.quartz.*;
import static org.quartz.JobBuilder.*;
import static org.quartz.TriggerBuilder.*;
import static org.quartz.SimpleScheduleBuilder.*;


@PersistJobDataAfterExecution
@DisallowConcurrentExecution
public class JobInternal implements Job {
    public static String MOD = "MOD";
    public static String FUN = "FUN";
    public static String ARG = "ARG";

    public void execute(JobExecutionContext context)
        throws JobExecutionException 
    {
        JobDataMap data = context.getJobDetail().getJobDataMap();
        OtpErlangAtom   mod = new OtpErlangAtom(data.getString(MOD));
        OtpErlangAtom   fun = new OtpErlangAtom(data.getString(FUN));
        OtpErlangString arg = new OtpErlangString(data.getString(ARG));
        EQuartzNode.fire(mod, fun, arg);
    }
}
