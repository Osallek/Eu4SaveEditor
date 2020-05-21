package com.osallek.eu4saveeditor;

import com.osallek.eu4saveeditor.common.Constants;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.stage.Stage;

import java.io.IOException;

public class Eu4SaveEditor extends Application {

    @Override
    public void start(Stage stage) throws IOException {
        FXMLLoader homeLoader = new FXMLLoader(getClass().getResource(Constants.TEMPLATE_HOME));

        getClass().getPackage().getImplementationTitle();
        stage.setWidth(1000);
        stage.setHeight(480);
        stage.setTitle("WhatIsEuIV Extractor");
        stage.setScene(new Scene(homeLoader.load()));
        stage.getIcons().addAll(new Image(Constants.IMAGE_ICON));
        stage.show();
    }

    public static void run(String[] args) {
        launch();
    }
}
