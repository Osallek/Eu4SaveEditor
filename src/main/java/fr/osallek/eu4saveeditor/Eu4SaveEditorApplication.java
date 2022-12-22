package fr.osallek.eu4saveeditor;

import javafx.application.Application;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class Eu4SaveEditorApplication {

    //Todo args
    public static void main(String[] args) {
        Application.launch(Eu4SaveEditorUiApplication.class, args);
    }

}
