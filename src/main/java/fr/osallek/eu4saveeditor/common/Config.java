package fr.osallek.eu4saveeditor.common;

import fr.osallek.eu4parser.Eu4Parser;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import java.util.Properties;
import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Config {

    private Config() {}

    private static final Logger LOGGER = LoggerFactory.getLogger(Config.class);

    private static boolean loaded = false;

    private static final Properties PROPERTIES = new Properties();

    private static final Path CONFIG_FILE_PATH = Constants.EDITOR_FOLDER.toPath().resolve("config").resolve("config.txt");
    private static final String GAME_FOLDER_PROP = "game_folder";
    private static final String SAVE_FOLDER_PROP = "save_folder";

    public static Optional<File> getGameFolder() {
        if (!loaded) {
            load();
        }

        String gameFolderPath = PROPERTIES.getProperty(GAME_FOLDER_PROP);

        if (gameFolderPath == null) {
            return Optional.empty();
        }

        File file = new File(gameFolderPath);

        if (file.exists() && file.canRead()) {
            return Optional.of(file);
        }

        return Optional.empty();
    }

    public static void setGameFolder(File newValue) {
        if (!loaded) {
            load();
        }

        if (newValue != null && newValue.exists()) {
            PROPERTIES.setProperty(GAME_FOLDER_PROP, newValue.getAbsolutePath());
        }

        store();
    }

    public static Optional<File> getSaveFolder() {
        if (!loaded) {
            load();
        }

        String saveFolderPath = PROPERTIES.getProperty(SAVE_FOLDER_PROP);

        if (saveFolderPath == null) {
            return Optional.empty();
        }

        File file = new File(saveFolderPath);

        if (file.exists() && file.canRead()) {
            return Optional.of(file);
        }

        return Optional.empty();
    }

    private static void load() {
        File file = CONFIG_FILE_PATH.toFile();

        if (!file.exists()) {
            tryDetectGameFolder();
            tryDetectSaveFolder();
            store();
        } else {
            try {
                try (InputStream inputStream = Files.newInputStream(file.toPath())) {
                    PROPERTIES.load(inputStream);
                }

                if (!PROPERTIES.containsKey(GAME_FOLDER_PROP)) {
                    tryDetectGameFolder();
                    store();
                }

                if (!PROPERTIES.containsKey(SAVE_FOLDER_PROP)) {
                    tryDetectSaveFolder();
                    store();
                }

            } catch (IOException e) {
                LOGGER.error("An error occurred while reading config file: {}", e.getMessage());
            }
        }

        loaded = true;
    }

    private static void store() {
        File file = CONFIG_FILE_PATH.toFile();

        try {
            if (!file.exists()) {
                FileUtils.forceMkdirParent(file);
            }

            try (OutputStream outputStream = Files.newOutputStream(file.toPath())) {
                PROPERTIES.store(outputStream, null);
            }
        } catch (IOException e) {
            LOGGER.error("An error occurred while writing config file: {}", e.getMessage());
        }
    }

    private static void tryDetectGameFolder() {
        Eu4Parser.detectInstallationFolder().ifPresent(path -> PROPERTIES.setProperty(GAME_FOLDER_PROP, path.toAbsolutePath().toString()));
    }

    private static void tryDetectSaveFolder() {
        if (PROPERTIES.contains(GAME_FOLDER_PROP)) {
            try {
                Path savePath = Eu4Parser.loadSettings(Path.of(PROPERTIES.getProperty(GAME_FOLDER_PROP))).getSavesFolder();

                if (Files.exists(savePath) && Files.isReadable(savePath)) {
                    PROPERTIES.setProperty(SAVE_FOLDER_PROP, savePath.toAbsolutePath().toString());
                }
            } catch (IOException ignored) {
            }
        }

        if (!PROPERTIES.contains(SAVE_FOLDER_PROP)) {
            if (Constants.DEFAULT_SAVES_FOLDER.exists() && Constants.DEFAULT_SAVES_FOLDER.canRead()) {
                PROPERTIES.setProperty(SAVE_FOLDER_PROP, Constants.DEFAULT_SAVES_FOLDER.getAbsolutePath());
            }
        }
    }
}
