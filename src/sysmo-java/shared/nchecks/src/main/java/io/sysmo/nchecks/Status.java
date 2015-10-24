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

package io.sysmo.nchecks;

import java.io.Serializable;

/**
 *
 * Created by seb on 23/10/15.
 *
 * Comparable and printable status
 *
 * Status weight:
 * <ul>
 *     <li>OK 10</li>
 *     <li>UNKNOWN 20</li>
 *     <li>WARNING 30</li>
 *     <li>CRITICAL 40</li>
 *     <li>ERROR 50</li>
 * </ul>
 *
 */
public class Status implements Comparable<Status>, Serializable {
    public static final Status OK = new Status("OK", 10);
    public static final Status UNKNOWN = new Status("UNKNOWN",20);
    public static final Status WARNING = new Status("WARNING",30);
    public static final Status CRITICAL = new Status("CRITICAL",40);
    public static final Status ERROR = new Status("ERROR",50);

    private String str;
    private int weight;

    public static Status fromString(String str) {
        switch (str) {
            case "UNKNOWN": return Status.UNKNOWN;
            case "OK": return Status.OK;
            case "ERROR": return Status.ERROR;
            case "WARNING": return Status.WARNING;
            case "CRITICAL": return Status.CRITICAL;
            default: return null;
        }
    }

    private Status(String str, int weight) {
        this.str = str;
        this.weight = weight;
    }

    public int compareTo(Status status) {
        int other = status.getWeight();
        if (this.weight == other) {
            return 0;
        } else if (this.weight < other) {
            return -1;
        } else {
            return 1;
        }
    }

    public boolean equals(Status status) {
        return (status.getWeight() == this.getWeight());
    }

    public int getWeight() {
        return this.weight;
    }

    @Override
    public String toString() {
        return this.str;
    }
}
