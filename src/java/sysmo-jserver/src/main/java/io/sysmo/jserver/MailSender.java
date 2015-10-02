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

    public MailSender(OtpMbox mbox, String etcDir)
    {
        MailSender.singleton = this;

        this.logger = LoggerFactory.getLogger(MailSender.class);
        this.mbox = mbox;

        String confFile = Paths.get(etcDir, "sysmo-mail.properties").toString();
        Properties props = new Properties();
        try {
            InputStream input = new FileInputStream(confFile);
            props.load(input);
        } catch (Exception e) {
            this.logger.info("No config file found....", e);
        }

        String fromString = props.getProperty("from", "sysmo@localhost");
        String toString   = props.getProperty("to");
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

        String host = props.getProperty("host", "localhost");
        this.properties.setProperty("mail.smtp.host", host);

        String user_name = props.getProperty("user_name");
        if (user_name != null)
            this.properties.setProperty("mail.user", user_name);

        String user_password = props.getProperty("user_password");
        if (user_password != null)
            this.properties.setProperty("mail.password", user_password);
        this.logger.info("End mailsender init");
    }

    public static MailSender getInstance() { return MailSender.singleton; }

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

    public void sendMail(OtpErlangObject mail)
    {
        this.logger.info("Should send mail: " + mail.toString());

        if (!this.active) return;

        Session session = Session.getDefaultInstance(this.properties);
        try {
            MimeMessage message = new MimeMessage(session);
            message.setFrom(this.from);
            message.addRecipient(Message.RecipientType.TO, this.to);
            message.setSubject("test message");
            message.setText("hello world: " + mail.toString());
            Transport.send(message);
        } catch (MessagingException e) {
            this.logger.error("fail to send message", e);
        }
    }
}
