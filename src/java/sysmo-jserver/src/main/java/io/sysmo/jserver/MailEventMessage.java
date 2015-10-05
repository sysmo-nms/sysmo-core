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

import java.sql.Timestamp;

/**
 * Created by seb on 05/10/15.
 */
public class MailEventMessage {
    private String probeId;
    private String checkId;
    private String status;
    private int statusCode;
    private Timestamp timestamp;
    private String returnString;
    private String probeDisplayName;
    private String hostDisplayName;
    private String hostLocation;
    private String hostContact;

    MailEventMessage(
            String probeId,
            String checkId,
            String status,
            int statusCode,
            Timestamp timestamp,
            String returnString,
            String probeDisplayName,
            String hostDisplayName,
            String hostLocation,
            String hostContact) {
        this.probeId = probeId;
        this.checkId = checkId;
        this.status = status;
        this.statusCode = statusCode;
        this.timestamp = timestamp;
        this.returnString = returnString;
        this.probeDisplayName = probeDisplayName;
        this.hostDisplayName = hostDisplayName;
        this.hostLocation = hostLocation;
        this.hostContact = hostContact;
    }

    public String getProbeId() {return this.probeId;}
    public String getCheckId() {return this.checkId;}
    public String getStatus() {return this.status;}
    public int getStatusCode() {return this.statusCode;}
    public Timestamp getTimestamp() {return this.timestamp;}
    public String getReturnString() {return this.returnString;}
    public String getProbeDisplayName() {return this.probeDisplayName;}
    public String getHostDisplayName() {return this.hostDisplayName;}
    public String getHostLocation() {return this.hostLocation;}
    public String getHostContact() {return this.hostContact;}

}
