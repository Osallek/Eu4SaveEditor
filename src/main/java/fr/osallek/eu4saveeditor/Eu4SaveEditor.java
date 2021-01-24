package fr.osallek.eu4saveeditor;

import fr.osallek.eu4saveeditor.common.Constants;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.stage.Stage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class Eu4SaveEditor extends Application {

    private static final Logger LOGGER = LoggerFactory.getLogger(Eu4SaveEditor.class);

    private static final DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern("HH:mm:ss dd/MM/yyyy");

    @Override
    public void start(Stage stage) throws IOException {
        if (LOGGER.isInfoEnabled()) {
            LOGGER.info("Starting Eu4 Save Editor at: {}", DATE_TIME_FORMATTER.format(LocalDateTime.now()));
        }

        FXMLLoader homeLoader = new FXMLLoader(getClass().getResource(Constants.TEMPLATE_HOME));

        stage.setWidth(1000);
        stage.setHeight(480);
        stage.setTitle("Eu4SaveEditor");
        stage.setScene(new Scene(homeLoader.load()));
        stage.getIcons().addAll(new Image(getClass().getResourceAsStream(Constants.IMAGE_ICON)));
        stage.show();
    }

    @Override
    public void stop() throws Exception {
        if (LOGGER.isInfoEnabled()) {
            LOGGER.info("Stropping Eu4 Save Editor at: {}", DATE_TIME_FORMATTER.format(LocalDateTime.now()));
        }
    }

    public static void run(String[] args) {
        launch();
    }
}
