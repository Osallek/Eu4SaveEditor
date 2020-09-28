package com.osallek.eu4saveeditor.controller;

import com.osallek.eu4parser.Eu4Parser;
import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4saveeditor.Main;
import com.osallek.eu4saveeditor.common.Config;
import com.osallek.eu4saveeditor.common.Constants;
import com.osallek.eu4saveeditor.common.FileProperty;
import com.osallek.eu4saveeditor.controller.converter.FileStringConverter;
import com.osallek.eu4saveeditor.i18n.MenusI18n;
import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.paint.Color;
import javafx.scene.paint.Paint;
import javafx.scene.text.Text;
import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;
import javafx.stage.Window;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;
import java.util.logging.Level;

public class HomeController implements Initializable {

    private static final Logger LOGGER = LoggerFactory.getLogger(HomeController.class);

    private final FXMLLoader editorLoader = new FXMLLoader(getClass().getResource(Constants.TEMPLATE_EDITOR));

    private final DirectoryChooser gameDirectoryChooser = new DirectoryChooser();

    private final FileChooser saveFileChooser = new FileChooser();

    private FileProperty gameDirectory;

    private FileProperty saveFile;

    private boolean canOpenGameDirectoryChoose = true;

    private boolean canOpenSaveFileChooser = true;

    @FXML
    public Text selectGameFolderText;

    @FXML
    public Text selectSaveFileText;

    @FXML
    private TextField selectedGameDirectory;

    @FXML
    private TextField selectedSaveFile;

    @FXML
    public Button chooseGameFolderButton;

    @FXML
    public Button chooseSaveFileButton;

    @FXML
    private Button startExtractButton;

    @FXML
    private Text infoText;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        this.gameDirectory = new FileProperty(this, "gameDirectory");
        this.saveFile = new FileProperty(this, "saveFile");
        this.selectGameFolderText.setText(MenusI18n.SELECT_GAME_FOLDER_DESC.getForDefaultLocale());
        this.selectSaveFileText.setText(MenusI18n.SELECT_SAVE_FILE_DESC.getForDefaultLocale());
        this.chooseGameFolderButton.setText(MenusI18n.CHOOSE_FOLDER.getForDefaultLocale());
        this.chooseSaveFileButton.setText(MenusI18n.CHOOSE_FILE.getForDefaultLocale());
        this.startExtractButton.setText(MenusI18n.START_EXTRACT.getForDefaultLocale());

        this.gameDirectoryChooser.setTitle(MenusI18n.SELECT_GAME_FOLDER.getForDefaultLocale());
        this.saveFileChooser.setTitle(MenusI18n.SELECT_SAVE_FILE.getForDefaultLocale());
        this.saveFileChooser.getExtensionFilters()
                            .add(new FileChooser.ExtensionFilter(MenusI18n.EU4_EXT_DESC.getForDefaultLocale(), "*.eu4"));

        this.selectedGameDirectory.textProperty()
                                  .bindBidirectional(this.gameDirectory, new FileStringConverter());
        this.selectedSaveFile.textProperty()
                             .bindBidirectional(this.saveFile, new FileStringConverter());

        this.gameDirectoryChooser.setInitialDirectory(Config.getGameFolder());
        this.gameDirectory.set(Config.getGameFolder());

        this.saveFileChooser.setInitialDirectory(Config.getSaveFolder());
        this.selectedSaveFile.setText(Config.getSaveFile() == null ? null : Config.getSaveFile().getAbsolutePath());
        enableStartExtractButton();
    }

    @FXML
    private void handleOpenGameDirectoryChoose(ActionEvent event) {
        if (this.canOpenGameDirectoryChoose) {
            Node eventSource = (Node) event.getSource();
            Window actionStage = eventSource.getScene().getWindow();

            this.gameDirectory.set(this.gameDirectoryChooser.showDialog(actionStage));

            if (this.gameDirectory.getValue() == null) {
                this.startExtractButton.setDisable(true);
                this.selectedGameDirectory.setText(null);
            } else {
                enableStartExtractButton();
                this.selectedGameDirectory.setText(this.gameDirectory.getValue().getPath());
            }
        }
    }

    @FXML
    private void handleOpenSaveFileChoose(ActionEvent event) {
        if (this.canOpenSaveFileChooser) {
            Node eventSource = (Node) event.getSource();
            Window actionStage = eventSource.getScene().getWindow();

            this.saveFile.set(this.saveFileChooser.showOpenDialog(actionStage));

            if (this.saveFile.getValue() == null) {
                this.startExtractButton.setDisable(true);
                this.selectedSaveFile.setText(null);
            } else {
                enableStartExtractButton();
                this.selectedSaveFile.setText(this.saveFile.getValue().getPath());
            }
        }
    }

    @FXML
    private void handleStartExtract(ActionEvent event) {
        this.startExtractButton.setDisable(true);
        this.canOpenGameDirectoryChoose = false;
        this.canOpenSaveFileChooser = false;
        this.infoText.setVisible(true);
        this.infoText.setText(MenusI18n.EXTRACTING.getForDefaultLocale());
        Config.setGameFolder(this.gameDirectory.getValue());
        Config.setSaveFile(this.saveFile.getValue());

        new Thread(() -> {
            Save save;

            try {
                save = Eu4Parser.loadSave(this.gameDirectory.getValue().getAbsolutePath(), this.saveFile.getValue().getAbsolutePath());
            } catch (Exception e) {
                LOGGER.error("An error occurred while extracting the save: {}", e.getMessage(), e);
                Platform.runLater(() -> {
                    this.infoText.setFill(Paint.valueOf(Color.RED.toString()));
                    this.infoText.setText("An error occurred while extracting the save: " + e.getMessage());
                    this.canOpenGameDirectoryChoose = true;
                    this.canOpenSaveFileChooser = true;
                });
                return;
            }

            Platform.runLater(() -> {
                try {
                    Parent editorNode = this.editorLoader.load();
                    ((EditorController) this.editorLoader.getController()).load(save);
                    this.startExtractButton.getScene().setRoot(editorNode);
                    ((EditorController) this.editorLoader.getController()).maximize();
                } catch (IOException e) {
                    LOGGER.error("An error occurred while extracting the save: {}", e.getMessage(), e);
                    this.infoText.setFill(Paint.valueOf(Color.RED.toString()));
                    this.infoText.setText("An error occurred while extracting the save: " + e.getMessage());
                    this.canOpenGameDirectoryChoose = true;
                    this.canOpenSaveFileChooser = true;
                }
            });
        }).start();
    }

    private void enableStartExtractButton() {
        if (this.saveFile.getValue() != null && this.gameDirectory.getValue() != null) {
            this.startExtractButton.setDisable(false);
        }
    }
}
