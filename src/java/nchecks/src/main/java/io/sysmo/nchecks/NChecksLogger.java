package io.sysmo.nchecks;
import java.util.logging.LogManager;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.logging.FileHandler;
import java.util.logging.SimpleFormatter;

public class NChecksLogger
{
    private static final int LOG_MAX_BYTES = 10000000; // 10MB
    private static final int LOG_MAX_FILES = 5;        // 10MB + 5 max 50MB
    private static final boolean LOG_APPEND = true;
    public  static Logger logger;

    public static Logger start(String file)
    {
        logger = Logger.getLogger(NChecksLogger.class.getName());
        logger.setLevel(Level.INFO);
        LogManager.getLogManager().reset();

        FileHandler handler;
        try {
            handler = new FileHandler(file, LOG_MAX_BYTES, LOG_MAX_FILES, LOG_APPEND);
            handler.setFormatter(new SimpleFormatter());
            logger.addHandler(handler);
        } catch (Exception e) {
            System.out.println("Log to file will not work! " + e);
        }
        return logger;
    }

    public static Logger getLogger()
    {
        return logger;
    }

}
