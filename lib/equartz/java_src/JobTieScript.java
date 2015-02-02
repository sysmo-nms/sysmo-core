package io.noctopus.equartz;


import java.util.Date;
import org.quartz.*;
import static org.quartz.JobBuilder.*;
import static org.quartz.TriggerBuilder.*;
import static org.quartz.SimpleScheduleBuilder.*;


public class JobTieScript implements Job {
    public void execute(JobExecutionContext context) throws JobExecutionException {
        /* 
         * Execute system command. Ensure that the job will be executed to
         * keep foreign system consistant. One --update replace the preceding --update
         * , cancel every action on a script if it is --deleted before having
         * called successfuly --init.
         *
         * 2 - (--init) if return status is 0 ok set tieStript job to initialized
         *  2.1 (--update) if return status is 0 ok
         *   2.1.1 (--delete) if return status is 0 ok, else reschedule 1h 2.1
         *  2.2 (--update) if return status is 1 permanent failure
         *  2.3 (--update) if return status is 2 reschedule 1h
         *   2.3.1 (--update another) cancel later update and restart from 2.3
         *   2.3.2 (--delete) cancel later update and replace with delete
         * 3 - (--init) if return status is 1 it is a permanent failure.
         *  3.1 - (--update) not initialized, do nothing
         *  3.2 - (--delete) not initialized, do nothing
         * 4 - (--init) if return is 2 it is a temporary failure. Reschedule in 1h
         *  4.1 (--update) ignore
         *  4.2 (--delete) ignore and cancel init from 4
         *
         */
        System.out.println("Hello TieScript! - " + new Date());
    }
}
