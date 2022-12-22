package fr.osallek.eu4saveeditor.config;

import javafx.stage.Stage;
import org.springframework.context.ApplicationEvent;

import java.nio.file.Path;

public class StageReadyEvent extends ApplicationEvent {

    private final Path savePath;

    public StageReadyEvent(Stage stage, Path savePath) {
        super(stage);
        this.savePath = savePath;
    }

    public Stage getStage() {
        return ((Stage) getSource());
    }

    public Path getSavePath() {
        return savePath;
    }
}
