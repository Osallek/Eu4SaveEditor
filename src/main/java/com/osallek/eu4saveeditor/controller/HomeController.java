package com.osallek.eu4saveeditor.controller;

import com.osallek.eu4parser.Eu4Parser;
import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4saveeditor.common.Constants;
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

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

public class HomeController implements Initializable {

    private final FXMLLoader editorLoader = new FXMLLoader(getClass().getResource(Constants.TEMPLATE_EDITOR));

    private final DirectoryChooser gameDirectoryChooser = new DirectoryChooser();

    private final FileChooser saveFileChooser = new FileChooser();

    private File gameDirectory;

    private File saveFile;

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
        this.gameDirectoryChooser.setTitle(MenusI18n.SELECT_GAME_FOLDER.getForDefaultLocale());
        this.saveFileChooser.setTitle(MenusI18n.SELECT_SAVE_FILE.getForDefaultLocale());
        this.saveFileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter(MenusI18n.EU4_EXT_DESC.getForDefaultLocale(), "*.eu4"));
        this.selectGameFolderText.setText(MenusI18n.SELECT_GAME_FOLDER_DESC.getForDefaultLocale());
        this.selectSaveFileText.setText(MenusI18n.SELECT_SAVE_FILE_DESC.getForDefaultLocale());
        this.chooseGameFolderButton.setText(MenusI18n.CHOOSE_FOLDER.getForDefaultLocale());
        this.chooseSaveFileButton.setText(MenusI18n.CHOOSE_FILE.getForDefaultLocale());
        this.startExtractButton.setText(MenusI18n.START_EXTRACT.getForDefaultLocale());

        if (Constants.DEFAULT_INSTALLATION_FILE.exists()) {
            this.gameDirectoryChooser.setInitialDirectory(Constants.DEFAULT_INSTALLATION_FILE);
        }

        if (Constants.DOCUMENTS_FOLDER.exists()) {
            this.saveFileChooser.setInitialDirectory(Constants.SAVES_FOLDER);
        }
    }

    @FXML
    private void handleOpenGameDirectoryChoose(ActionEvent event) {
        if (this.canOpenGameDirectoryChoose) {
            Node eventSource = (Node) event.getSource();
            Window actionStage = eventSource.getScene().getWindow();

            this.gameDirectory = this.gameDirectoryChooser.showDialog(actionStage);

            if (this.gameDirectory == null) {
                this.startExtractButton.setDisable(true);
                this.selectedGameDirectory.setText(null);
            } else {
                enableStartExtractButton();
                this.selectedGameDirectory.setText(this.gameDirectory.getPath());
            }
        }
    }

    @FXML
    private void handleOpenSaveFileChoose(ActionEvent event) {
        if (this.canOpenSaveFileChooser) {
            Node eventSource = (Node) event.getSource();
            Window actionStage = eventSource.getScene().getWindow();

            this.saveFile = this.saveFileChooser.showOpenDialog(actionStage);

            if (this.saveFile == null) {
                this.startExtractButton.setDisable(true);
                this.selectedSaveFile.setText(null);
            } else {
                enableStartExtractButton();
                this.selectedSaveFile.setText(this.saveFile.getPath());
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
        new Thread(() -> {
            Save save;

            try {
                save = Eu4Parser.loadSave(this.gameDirectory.getAbsolutePath(), this.saveFile.getAbsolutePath());
            } catch (Exception e) {
                e.printStackTrace();
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
                    this.infoText.setFill(Paint.valueOf(Color.RED.toString()));
                    this.infoText.setText("An error occurred while extracting the save: " + e.getMessage());
                    this.canOpenGameDirectoryChoose = true;
                    this.canOpenSaveFileChooser = true;
                }
            });
        }).start();
    }

    private void enableStartExtractButton() {
        if (this.saveFile != null && this.gameDirectory != null) {
            this.startExtractButton.setDisable(false);
        }
    }
}
