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
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;

import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

public class NChecksJRuby {
    private static Logger logger = LoggerFactory.getLogger(NChecksJRuby.class);

    private static NChecksJRuby instance;
    private static final Object lock = new Object();

    public static NChecksJRuby getInstance() { return instance; }

    private String scriptPath;
    private HashMap<String,String> scriptMap;

    public static void startJRuby(String scriptPath) {
        new NChecksJRuby(scriptPath);
    }

    private NChecksJRuby(String scriptPath) {
        NChecksJRuby.instance = this;
        this.scriptPath = scriptPath;
        this.scriptMap = new HashMap<>();
        NChecksJRuby.logger.info("JRuby init with path: " + scriptPath);
    }

    public String getScript(String identifier) throws Exception {
        synchronized (NChecksJRuby.lock) {

            String val = this.scriptMap.get(identifier);

            if (val == null) {
                String script = identifier + ".rb";
                byte[] fileBytes =
                        Files.readAllBytes(Paths.get(scriptPath,script));
                val = new String(fileBytes, "UTF-8");
                this.scriptMap.put(script, val);
            }

            return val;
        }
    }
}
