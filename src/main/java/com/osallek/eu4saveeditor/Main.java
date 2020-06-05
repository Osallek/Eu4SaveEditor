package com.osallek.eu4saveeditor;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.logging.FileHandler;
import java.util.logging.Formatter;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

public class Main {

    public static final Logger LOGGER = Logger.getLogger("");

    public static void main(String[] args) throws IOException {
        LOGGER.setLevel(Level.INFO);
        File logFile = new File("logs.txt");

        if (!logFile.exists()) {
            logFile.createNewFile();
        }

        FileHandler fileTxt = new FileHandler(logFile.getAbsolutePath(), true);
        Formatter formatterTxt = new SimpleFormatter();
        fileTxt.setFormatter(formatterTxt);
//        LOGGER.addHandler(fileTxt);

        PrintStream logStream = new PrintStream(logFile);
//        System.setErr(logStream);
//        System.setOut(logStream);

        Eu4SaveEditor.run(args);
    }
}
