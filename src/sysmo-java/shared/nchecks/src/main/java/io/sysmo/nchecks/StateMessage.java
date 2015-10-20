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

/**
 * Created by seb on 20/10/15.
 */
public class StateMessage {
    private String key;
    private byte[] value;
    private int id = 0;
    private String action;

    StateMessage(String key) {
        this.key = key;
        this.action = "get";
    }

    StateMessage(String key, byte[] value) {
        this.key = key;
        this.value = value;
        this.action = "set";
    }

    public byte[] getValue() {
        return this.value;
    }

    public String getKey() {
        return this.key;
    }

    public String getAction() {
        return this.action;
    }

    public int getId() {
        return this.id;
    }

    public void setValue(byte[] value) {
        this.value = value;
    }
}
