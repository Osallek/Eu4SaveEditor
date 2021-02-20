package fr.osallek.eu4saveeditor;

import fr.osallek.eu4saveeditor.common.Constants;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import fr.osallek.eu4saveeditor.controller.HomeController;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.stage.Stage;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;

public class Eu4SaveEditor extends Application {

    private static final Logger LOGGER = LoggerFactory.getLogger(Eu4SaveEditor.class);

    private static final DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern("HH:mm:ss dd/MM/yyyy");

    private static String gameFolderArg;

    private static String modsFolderArg;

    private static String saveFileArg;

    public static boolean override;

    private boolean validArgs = true;

    @Override
    public void start(Stage stage) throws IOException {
        if (LOGGER.isInfoEnabled()) {
            LOGGER.info("Starting Eu4 Save Editor at: {}", DATE_TIME_FORMATTER.format(LocalDateTime.now()));
        }

        FXMLLoader homeLoader = new FXMLLoader(Eu4SaveEditor.class.getResource(Constants.TEMPLATE_HOME));

        stage.setWidth(1000);
        stage.setHeight(480);
        stage.setTitle("Eu4SaveEditor");
        stage.setScene(new Scene(homeLoader.load()));
        stage.getIcons().addAll(new Image(Eu4SaveEditor.class.getResourceAsStream(Constants.IMAGE_ICON)));

        HomeController homeController = homeLoader.getController();

        if (StringUtils.isNotBlank(gameFolderArg)) {
            this.validArgs &= homeController.setGameDirectory(gameFolderArg);
        } else {
            this.validArgs = false;
        }

        if (StringUtils.isNotBlank(modsFolderArg)) {
            this.validArgs &= homeController.setModDirectory(modsFolderArg);
        } else {
            this.validArgs = false;
        }

        if (StringUtils.isNotBlank(saveFileArg)) {
            this.validArgs &= homeController.setSelectedSaveFile(saveFileArg);
        } else {
            this.validArgs = false;
        }

        stage.show();

        if (this.validArgs) {
            homeController.handleStartExtract(null);
        }
    }

    @Override
    public void stop() throws Exception {
        if (LOGGER.isInfoEnabled()) {
            LOGGER.info("Stropping Eu4 Save Editor at: {}", DATE_TIME_FORMATTER.format(LocalDateTime.now()));
        }
    }

    public static void run(String[] args) {
        LOGGER.info("Running with args {}", Arrays.toString(args));
        gameFolderArg = Eu4SaveEditorUtils.readArg(args, Constants.GAME_FOLDER_ARG);
        modsFolderArg = Eu4SaveEditorUtils.readArg(args, Constants.MODS_FOLDER_ARG);
        saveFileArg = Eu4SaveEditorUtils.readArg(args, Constants.SAVE_FILE_ARG);
        override = BooleanUtils.toBoolean(Eu4SaveEditorUtils.readArg(args, Constants.OVERRIDE_ARG));
        launch();
    }
}
