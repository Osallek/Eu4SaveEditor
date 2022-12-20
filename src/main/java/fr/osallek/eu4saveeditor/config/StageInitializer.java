package fr.osallek.eu4saveeditor.config;

import fr.osallek.eu4saveeditor.common.Constants;
import fr.osallek.eu4saveeditor.controller.HomeController;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.stage.Stage;
import org.kordamp.bootstrapfx.BootstrapFX;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;

@Component
public class StageInitializer implements ApplicationListener<StageReadyEvent> {

    private static final Logger LOGGER = LoggerFactory.getLogger(StageInitializer.class);

    private final HomeController mainController;

    public StageInitializer(HomeController homeController) {
        this.mainController = homeController;
    }

    @Override
    public void onApplicationEvent(StageReadyEvent event) {
        try {
            Scene scene = new Scene(this.mainController.getScene());
            scene.getStylesheets().add(BootstrapFX.bootstrapFXStylesheet());
            scene.getStylesheets().add(getClass().getResource("/styles/style.css").toExternalForm());

            Stage stage = event.getStage();
            stage.setMaximized(true);
            stage.setScene(scene);
            stage.setTitle("Eu4 Save Editor");
            stage.getIcons().add(new Image(getClass().getResourceAsStream(Constants.IMAGE_ICON)));
            stage.show();
        } catch (Exception e) {
            LOGGER.error("{}", e.getMessage(), e);
        }
    }
}
