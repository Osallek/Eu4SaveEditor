package com.osallek.eu4saveeditor.controller;

import com.osallek.eu4parser.Eu4Parser;
import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4saveeditor.common.Constants;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.text.Text;
import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;
import javafx.stage.Window;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;

public class HomeController implements Initializable {

    private final DirectoryChooser gameDirectoryChooser = new DirectoryChooser();

    private final FileChooser saveFileChooser = new FileChooser();

    private File gameDirectory;

    private File saveFile;

    @FXML
    private Button startExtractButton;

    @FXML
    private Text selectedGameDirectory;

    @FXML
    private Text selectedSaveFile;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        this.startExtractButton.setDisable(true);
        this.gameDirectoryChooser.setTitle("EuIV game folder");

        if (Constants.DEFAULT_INSTALLATION_FILE.exists()) {
            this.gameDirectoryChooser.setInitialDirectory(Constants.DEFAULT_INSTALLATION_FILE);
        }

        if (Constants.DOCUMENTS_FOLDER.exists()) {
            this.saveFileChooser.setInitialDirectory(Constants.SAVES_FOLDER);
        }
    }

    @FXML
    private void handleOpenGameDirectoryChoose(ActionEvent event) {
        Node eventSource = (Node) event.getSource();
        Window actionStage = eventSource.getScene().getWindow();

        this.gameDirectory = this.gameDirectoryChooser.showDialog(actionStage);

        if (this.gameDirectory == null) {
            this.startExtractButton.setDisable(true);
            this.selectedGameDirectory.setText(null);
        } else {
            enableStartExtractButton();
            this.selectedGameDirectory.setText("Selected folder: " + this.gameDirectory);
        }
    }

    @FXML
    private void handleOpenSaveFileChoose(ActionEvent event) {
        Node eventSource = (Node) event.getSource();
        Window actionStage = eventSource.getScene().getWindow();

        this.saveFile = this.saveFileChooser.showOpenDialog(actionStage);

        if (this.saveFile == null) {
            this.startExtractButton.setDisable(true);
            this.selectedSaveFile.setText(null);
        } else {
            enableStartExtractButton();
            this.selectedSaveFile.setText("Selected save file: " + this.saveFile);
        }
    }

    @FXML
    private void handleStartExtract(ActionEvent event) throws IOException {
        this.startExtractButton.setDisable(true);
        Save save = Eu4Parser.loadSave(this.gameDirectory.getAbsolutePath(), this.saveFile.getAbsolutePath());
        this.startExtractButton.setDisable(false);
    }

    private void enableStartExtractButton() {
        if (this.saveFile != null && this.gameDirectory != null) {
            this.startExtractButton.setDisable(false);
        }
    }
}
