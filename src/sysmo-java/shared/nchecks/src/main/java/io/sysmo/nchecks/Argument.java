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

public class Argument
{
    private String argument;

    public Argument() {
        this.argument = "";
    }

    /**
     * Internal use only.
     * @param val the string value of the argument
     */
    public void set(String val) {this.argument = val;}

    /**
     * Return the integer representation of the argument.
     * @return integer representation of the argument
     * @throws NumberFormatException
     */
    public int asInteger() throws NumberFormatException {
        return Integer.parseInt(this.argument);
    }

    /**
     * Return the string value of argument
     * @return the original argument string
     */
    public String asString() {return this.argument;}
}
