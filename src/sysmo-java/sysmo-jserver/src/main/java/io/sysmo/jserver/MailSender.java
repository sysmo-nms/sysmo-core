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

import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import java.nio.file.Paths;

import java.util.Properties;

/**
 * Created by seb on 30/09/15.
 */
public class MailSender implements Runnable {
    private Logger logger;
    private OtpMbox mbox;
    private static MailSender singleton;
    private InternetAddress from;
    private InternetAddress to;
    private Properties properties;
    private boolean active = true;

    public static MailSender getInstance(
            final OtpMbox mbox,
            final String etcDir)
    {
        MailSender.singleton = new MailSender(mbox, etcDir);
        return MailSender.singleton;
    }

    private MailSender(
            final OtpMbox mbox,
            final String etcDir)
    {
        this.logger = LoggerFactory.getLogger(MailSender.class);
        this.mbox = mbox;

        String confFile = Paths.get(etcDir, "sysmo.properties").toString();
        Properties props = new Properties();
        InputStream input = null;
        try {
            input = new FileInputStream(confFile);
            props.load(input);
        } catch (Exception e) {
            this.logger.info("No config file found....", e);
        } finally {
            try {
                if (input != null) {
                    input.close();
                }
            } catch (IOException ignore) {
                //ignore
            }
        }

        String fromString = props.getProperty("mail_from", "sysmo@localhost");
        String toString   = props.getProperty("mail_to");
        try {
            this.from = new InternetAddress(fromString);
            if (toString != null)
                this.to = new InternetAddress(toString);
            else
               this.active = false;
        } catch (AddressException e) {
            this.logger.error("Mail address exception: ", e);
            this.active = false;
        }

        this.logger.info("Internet addresses ok");

        this.properties = System.getProperties();

        String host = props.getProperty("mail_smtp_server", "localhost");
        this.properties.setProperty("mail.smtp.host", host);

        String user_name = props.getProperty("mail_user_name");
        if (user_name != null)
            this.properties.setProperty("mail.user", user_name);

        String user_password = props.getProperty("mail_user_password");
        if (user_password != null)
            this.properties.setProperty("mail.password", user_password);
        this.logger.info("End MailSender init");
    }


    @Override
    public void run() {
        // begin to loop and wait for calls
        OtpErlangObject event;
        while (true) try {
            event = this.mbox.receive();
            this.sendMail(event);
        } catch (OtpErlangExit |OtpErlangDecodeException e) {
            this.logger.error(e.getMessage(), e);
            this.mbox.exit("crash");
            break;
        }
    }

    public void sendMail(OtpErlangObject event) {
        // TODO
    }

    public static void sendMail(MailEventMessage event) {
        MailSender.singleton.sendMailEvent(event);
    }

    public synchronized void sendMailEvent(MailEventMessage event)
    {
        this.logger.info("Should send mail: " + event.getProbeId());

        if (!this.active) return;

        Session session = Session.getDefaultInstance(this.properties);
        try {
            MimeMessage message = new MimeMessage(session);
            message.setFrom(this.from);
            message.addRecipient(Message.RecipientType.TO, this.to);

            // build the subject
            String subject = "SYSMO " + event.getStatus()
                    + " for:" + event.getHostDisplayName() + " ; "
                    + " probe:" + event.getProbeDisplayName();
            message.setSubject(subject);

            String content = "<h3>" + event.getHostDisplayName() + " "
                    + event.getProbeDisplayName() + "</h3>"
                    + "<p><strong>" + event.getReturnString() + "</strong></p>"
                    + "<ul>"
                    + "<li> check ID: <strong>"
                    + event.getCheckId() + "</strong></li>"
                    + "<li> status: <strong>"
                    + event.getStatus() + "</strong></li>"
                    + "<li> statusCode: <strong>"
                    + String.valueOf(event.getStatusCode()) + "</strong></li>"
                    + "<li> timestamp: <strong>"
                    + event.getTimestamp().toString() + "</strong></li>"
                    + "</ul>"
                    + "<h4>Host info</h4>"
                    + "<ul>"
                    + "<li> name: <strong>" + event.getHostDisplayName()
                    + "</strong></li>"
                    + "<li> location: <strong>" + event.getHostLocation()
                    + "</strong></li>"
                    + "<li> contact: <strong>" + event.getHostContact()
                    + "</strong></li>"
                    + "</ul>";
            message.setContent(content, "text/html");

            Transport.send(message);
        } catch (MessagingException e) {
            this.logger.error("fail to send message", e);
        }
    }
}
