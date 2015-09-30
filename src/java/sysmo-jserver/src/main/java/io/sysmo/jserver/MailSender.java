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
 * along with Sysmo.  If not, see <http://www.gnu.org/licenses/>.
 */

package io.sysmo.jserver;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpMbox;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.file.Paths;

/**
 * Created by seb on 30/09/15.
 */
public class MailSender implements Runnable {
    private Logger logger;
    private OtpMbox mbox;
    private static MailSender singleton;

    public MailSender(OtpMbox mbox, String etcDir)
    {
        MailSender.singleton = this;
        String confFile = Paths.get(etcDir, "sysmo-mail.properties").toString();

        this.logger = LoggerFactory.getLogger(MailSender.class);
        this.mbox = mbox;

    }

    public static MailSender getInstance() { return MailSender.singleton; }

    @Override
    public void run() {
        // begin to loop and wait for calls
        OtpErlangObject event;
        while (true) try {
            call = this.mbox.receive();
            this.sendMail(event);
        } catch (OtpErlangExit |OtpErlangDecodeException e) {
            this.logger.error(e.getMessage(), e);
            this.mbox.exit("crash");
            break;
        }
    }

    public void sendMail(OtpErlangObject mail)
    {
        this.logger.info("Should send mail: " + mail.toString());
    }
}
