package com.osallek.eu4saveeditor.common;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Properties;

public class Config {

    private Config() {}

    private static final Logger LOGGER = LoggerFactory.getLogger(Config.class);

    private static boolean loaded = false;

    private static final Properties PROPERTIES = new Properties();

    private static final String CONFIG_FILE_PATH = "config.txt";
    private static final String GAME_FOLDER_PROP = "game_folder";
    private static final String SAVE_FOLDER_PROP = "save_folder";
    private static final String SAVE_FILE_PROP = "save_file";

    public static File getGameFolder() {
        if (!loaded) {
            load();
        }

        String gameFolderPath = PROPERTIES.getProperty(GAME_FOLDER_PROP);

        if (gameFolderPath == null) {
            return null;
        }

        File file = new File(gameFolderPath);

        if (file.exists() && file.canRead()) {
            return file;
        }

        return null;
    }

    public static void setGameFolder(File newValue) {
        if (!loaded) {
            load();
        }

        PROPERTIES.setProperty(GAME_FOLDER_PROP, newValue.getAbsolutePath());

        store();
    }

    public static void setSaveFile(File newValue) {
        if (!loaded) {
            load();
        }

        PROPERTIES.setProperty(SAVE_FILE_PROP, newValue.getAbsolutePath());
        PROPERTIES.setProperty(SAVE_FOLDER_PROP, newValue.getParent());

        store();
    }

    public static File getSaveFolder() {
        if (!loaded) {
            load();
        }

        String saveFolderPath = PROPERTIES.getProperty(SAVE_FOLDER_PROP);

        if (saveFolderPath == null) {
            return null;
        }

        File file = new File(saveFolderPath);

        if (file.exists() && file.canRead()) {
            return file;
        }

        return null;
    }

    public static File getSaveFile() {
        if (!loaded) {
            load();
        }

        String saveFolderPath = PROPERTIES.getProperty(SAVE_FILE_PROP);

        if (saveFolderPath == null) {
            return null;
        }

        File file = new File(saveFolderPath);

        if (file.exists() && file.canRead()) {
            return file;
        }

        return null;
    }

    private static void load() {
        File file = new File(CONFIG_FILE_PATH);

        if (!file.exists()) {
            if (Constants.DEFAULT_INSTALLATION_FOLDER.exists() && Constants.DEFAULT_INSTALLATION_FOLDER.canRead()) {
                PROPERTIES.setProperty(GAME_FOLDER_PROP, Constants.DEFAULT_INSTALLATION_FOLDER.getAbsolutePath());
            }

            if (Constants.SAVES_FOLDER.exists() && Constants.SAVES_FOLDER.canRead()) {
                PROPERTIES.setProperty(SAVE_FOLDER_PROP, Constants.SAVES_FOLDER.getAbsolutePath());
            }

            store();
        } else {
            try {
                PROPERTIES.load(Files.newInputStream(file.toPath()));
            } catch (IOException e) {
                LOGGER.error("An error occurred while reading config file: {}", e.getMessage());
            }
        }

        loaded = true;
    }

    private static void store() {
        File file = new File(CONFIG_FILE_PATH);

        try {
            if (!file.exists()) {
                file.createNewFile();
            }

            PROPERTIES.store(Files.newOutputStream(file.toPath()), null);
        } catch (IOException e) {
            LOGGER.error("An error occurred while writing config file: {}", e.getMessage());
        }
    }
}
