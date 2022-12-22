package fr.osallek.eu4saveeditor;

import fr.osallek.eu4saveeditor.common.Config;
import fr.osallek.eu4saveeditor.common.Constants;
import fr.osallek.eu4saveeditor.config.StageReadyEvent;
import javafx.application.Application;
import javafx.application.Platform;
import javafx.stage.Stage;
import org.apache.commons.collections4.MapUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.context.ConfigurableApplicationContext;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Map;
import java.util.Optional;

public class Eu4SaveEditorUiApplication extends Application {

    private static final Logger LOGGER = LoggerFactory.getLogger(Eu4SaveEditorUiApplication.class);

    private ConfigurableApplicationContext applicationContext;

    private Path savePath = null;

    @Override
    public void init() {
        Map<String, String> parameters = getParameters().getNamed();

        if (MapUtils.isNotEmpty(parameters)) {
            LOGGER.info("Running with args {}", Arrays.toString(parameters.entrySet().toArray()));

            if (parameters.containsKey(Constants.GAME_FOLDER_ARG)) {
                Config.setGameFolder(new File(parameters.get(Constants.GAME_FOLDER_ARG)));
            }

            if (parameters.containsKey(Constants.SAVE_FILE_ARG)) {
                Optional.of(Path.of(parameters.get(Constants.SAVE_FILE_ARG)))
                        .filter(Files::exists)
                        .filter(Files::isReadable)
                        .ifPresent(path -> this.savePath = path);
            }
        }

        this.applicationContext = new SpringApplicationBuilder(Eu4SaveEditorApplication.class)
                .initializers(context -> context.getBeanFactory().registerSingleton(this.getClass().getSimpleName(), this)).run();

    }

    @Override
    public void start(Stage stage) {
        this.applicationContext.publishEvent(new StageReadyEvent(stage, savePath));
    }

    @Override
    public void stop() {
        this.applicationContext.close();
        Platform.exit();
    }
}
