package com.osallek.eu4saveeditor.controller;

import com.osallek.eu4parser.model.save.Save;
import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.text.Text;
import javafx.stage.Stage;

import java.net.URL;
import java.util.ResourceBundle;

public class EditorController implements Initializable {

    private Save save;

    @FXML
    private Text title;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        this.title.getScene();
        Platform.runLater(() -> ((Stage) this.title.getScene().getWindow()).setMaximized(true));
    }

    public Save getSave() {
        return save;
    }

    public void setSave(Save save) {
        this.save = save;
    }
}
