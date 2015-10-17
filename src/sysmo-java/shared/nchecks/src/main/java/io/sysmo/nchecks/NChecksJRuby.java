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
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Properties;

import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

public class NChecksJRuby {
    private static Logger logger = LoggerFactory.getLogger(NChecksJRuby.class);
    private static JRubyCache cache;

    public static void startJRubyCache(String scriptPath, String etcDir) {
        String nchecksConf = Paths.get(etcDir, "nchecks.properties").toString();
        Boolean should_cache;
        try {
            Properties props = new Properties();
            InputStream input = new FileInputStream(nchecksConf);
            props.load(input);
            should_cache = props.getProperty("cache_ruby_files").equals("true");
        } catch (IOException e) {
            // no config file found
            should_cache = true;
        }

        if (should_cache) {
            NChecksJRuby.cache = new StandardCache(scriptPath);
        } else {
            NChecksJRuby.cache = new NoCache(scriptPath);
        }
        NChecksJRuby.logger.info("JRuby script path: " + scriptPath);
        NChecksJRuby.logger.info("JRuby cache files: " + should_cache.toString());
    }

    public static String getScript(String identifier) throws Exception {
        return NChecksJRuby.cache.getScript(identifier);
    }
}

interface JRubyCache {
    String getScript(String identifier) throws Exception;
}

class StandardCache implements JRubyCache
{
    private String scriptPath;
    private HashMap<String,String> scriptMap;
    private static final Object lock = new Object();

    StandardCache(String scriptPath) {
        this.scriptPath = scriptPath;
        this.scriptMap = new HashMap<>();
    }

    public String getScript(String identifier) throws Exception {
        synchronized (StandardCache.lock) {

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

class NoCache implements JRubyCache
{
    private String scriptPath;
    private static final Object lock = new Object();

    NoCache(String scriptPath) {
        this.scriptPath = scriptPath;
    }

    public String getScript(String identifier) throws Exception {
        synchronized (NoCache.lock) {
            String script = identifier + ".rb";
            byte[] fileBytes = Files.readAllBytes(Paths.get(scriptPath, script));
            return new String(fileBytes, "UTF-8");
        }
    }

}
