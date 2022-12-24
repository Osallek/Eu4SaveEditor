package fr.osallek.eu4saveeditor.controller;

import fr.osallek.eu4saveeditor.common.Config;
import fr.osallek.eu4saveeditor.common.Constants;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import fr.osallek.eu4saveeditor.common.FileProperty;
import fr.osallek.eu4saveeditor.common.ReadSaveTask;
import fr.osallek.eu4saveeditor.config.ApplicationProperties;
import fr.osallek.eu4saveeditor.controller.converter.FileStringConverter;
import fr.osallek.eu4saveeditor.controller.object.BootstrapColumn;
import fr.osallek.eu4saveeditor.controller.object.BootstrapPane;
import fr.osallek.eu4saveeditor.controller.object.BootstrapRow;
import fr.osallek.eu4saveeditor.controller.object.LocalSaveListCell;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.concurrent.WorkerStateEvent;
import javafx.event.ActionEvent;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.TextField;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.scene.text.TextAlignment;
import javafx.stage.DirectoryChooser;
import javafx.stage.Window;
import org.apache.commons.collections4.CollectionUtils;
import org.kordamp.bootstrapfx.scene.layout.Panel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.MessageSource;
import org.springframework.stereotype.Component;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

@Component
public class HomeController {

    private static final Logger LOGGER = LoggerFactory.getLogger(HomeController.class);

    private final ApplicationProperties properties;

    private final MessageSource messageSource;

    private final EditorController editorController;

    private final DirectoryChooser gameDirectoryChooser = new DirectoryChooser();

    private final FileProperty gameDirectory = new FileProperty(this, "gameDirectory");

    private final BooleanProperty loading = new SimpleBooleanProperty(false);

    private BootstrapPane root;

    private TextField selectedGameDirectory;

    private ComboBox<Path> localSavesCombo;

    private Button startExtractButton;

    private Text progressText;

    private ProgressBar progressBar;

    public HomeController(ApplicationProperties properties, MessageSource messageSource, EditorController editorController) {
        this.properties = properties;
        this.messageSource = messageSource;
        this.editorController = editorController;
    }

    public void initialize(Path savePath) {
        this.root = new BootstrapPane();
        this.root.setPadding(new Insets(50));
        this.root.setVgap(25);
        this.root.setHgap(25);
        this.root.setBackground(new Background(new BackgroundFill(null, null, null)));

        BootstrapRow titleRow = new BootstrapRow(true);
        Label title = new Label("Eu4 Save Editor");
        title.setAlignment(Pos.TOP_CENTER);
        title.setMaxWidth(Double.MAX_VALUE);
        title.getStyleClass().add("h1");
        titleRow.addColumn(new BootstrapColumn(title, new int[] {12, 12, 10, 8, 6}));

        this.startExtractButton = new Button();
        this.startExtractButton.onActionProperty().set(this::handleStartExtract);
        this.startExtractButton.setDisable(true);
        this.startExtractButton.setAlignment(Pos.CENTER);
        this.startExtractButton.setText(this.messageSource.getMessage("ose.start-extract", null, Constants.LOCALE));

        VBox extract = new VBox();
        extract.setAlignment(Pos.CENTER);
        extract.getChildren().add(this.startExtractButton);

        BootstrapRow extractRow = new BootstrapRow(true);
        extractRow.addColumn(new BootstrapColumn(extract, new int[] {12, 12, 10, 8, 6}));

        BootstrapRow gameDirectoryRow = new BootstrapRow(true);
        Panel gameDirectoryPanel = new Panel();
        gameDirectoryPanel.getStyleClass().add("panel-default");

        Label gameDirectoryTitleLabel = new Label(this.messageSource.getMessage("ose.game-directory", null, Constants.LOCALE));
        gameDirectoryTitleLabel.getStyleClass().addAll("h5", "b");
        gameDirectoryPanel.setHeading(gameDirectoryTitleLabel);

        this.gameDirectoryChooser.setTitle(this.messageSource.getMessage("ose.game-directory", null, Constants.LOCALE));

        Button chooseGameFolderButton = new Button();
        chooseGameFolderButton.onActionProperty().set(this::handleOpenGameDirectoryChoose);
        chooseGameFolderButton.setText(this.messageSource.getMessage("ose.choose-folder", null, Constants.LOCALE));
        chooseGameFolderButton.disableProperty().bind(this.loading);

        this.selectedGameDirectory = new TextField();
        this.selectedGameDirectory.setEditable(false);
        this.selectedGameDirectory.prefWidth(Double.POSITIVE_INFINITY);
        this.selectedGameDirectory.textProperty().bindBidirectional(this.gameDirectory, new FileStringConverter());
        HBox.setHgrow(this.selectedGameDirectory, Priority.ALWAYS);

        HBox gameDirectoryHbox = new HBox();
        gameDirectoryHbox.setSpacing(3);
        gameDirectoryHbox.setAlignment(Pos.CENTER_LEFT);
        gameDirectoryHbox.getChildren().add(this.selectedGameDirectory);
        gameDirectoryHbox.getChildren().add(chooseGameFolderButton);

        gameDirectoryPanel.setBody(gameDirectoryHbox);

        gameDirectoryRow.addColumn(new BootstrapColumn(gameDirectoryPanel, new int[] {12, 12, 10, 8, 6}));

        BootstrapRow localSavesRow = new BootstrapRow(true);
        Panel localSavesPanel = new Panel();
        localSavesPanel.getStyleClass().add("panel-default");

        Label localSavesTitleLabel = new Label(this.messageSource.getMessage("ose.local-saves", null, Constants.LOCALE));
        localSavesTitleLabel.getStyleClass().addAll("h5", "b");
        localSavesPanel.setHeading(localSavesTitleLabel);

        List<Path> localSaves = Config.getSaveFolder().map(Eu4SaveEditorUtils::getSaves).orElse(new ArrayList<>());

        if (CollectionUtils.isNotEmpty(localSaves)) {
            this.localSavesCombo = new ComboBox<>(FXCollections.observableArrayList(localSaves));
            this.localSavesCombo.setVisibleRowCount(20);
            this.localSavesCombo.setCellFactory(param -> new LocalSaveListCell(Config.getSaveFolder().map(File::toPath).orElse(null)));
            this.localSavesCombo.setButtonCell(new LocalSaveListCell(Config.getSaveFolder().map(File::toPath).orElse(null)));
            this.localSavesCombo.setPromptText(this.messageSource.getMessage("ose.local-saves.choose", null, Constants.LOCALE));
            this.localSavesCombo.disableProperty().bind(this.loading);

            localSavesPanel.setBody(this.localSavesCombo);
        } else {
            localSavesPanel.setBody(new Text(this.messageSource.getMessage("ose.saves.none", null, Constants.LOCALE)));
        }

        localSavesRow.addColumn(new BootstrapColumn(localSavesPanel, new int[] {12, 12, 10, 8, 6}));

        this.startExtractButton.disableProperty().bind(this.localSavesCombo.getSelectionModel().selectedItemProperty().isNull()
                                                                           .or(this.gameDirectory.isNull()).or(this.loading));

        this.progressText = new Text();
        this.progressText.setVisible(false);
        this.progressText.setTextAlignment(TextAlignment.CENTER);

        this.progressBar = new ProgressBar(0);
        this.progressBar.setVisible(false);
        this.progressBar.getStyleClass().add("progress-bar-primary");
        this.progressBar.setMaxWidth(Double.MAX_VALUE);

        VBox info = new VBox(10);
        info.setAlignment(Pos.CENTER);
        info.getChildren().add(this.progressText);
        info.getChildren().add(this.progressBar);

        BootstrapRow infoRow = new BootstrapRow(true);
        infoRow.addColumn(new BootstrapColumn(info, new int[] {12, 12, 10, 8, 6}));

        BootstrapRow versionRow = new BootstrapRow(true);
        Label version = new Label(this.messageSource.getMessage("ose.version", new Object[] {this.properties.getVersion(), this.properties.getGameVersion()}, Constants.LOCALE));
        version.setAlignment(Pos.BOTTOM_CENTER);
        version.setMaxWidth(Double.MAX_VALUE);
        version.getStyleClass().add("h6");
        versionRow.addColumn(new BootstrapColumn(version, new int[] {12, 12, 10, 8, 6}));

        this.root.addRow(titleRow);
        this.root.addRow(gameDirectoryRow);
        this.root.addRow(localSavesRow);
        this.root.addRow(extractRow);
        this.root.addRow(infoRow);
        this.root.addRow(versionRow);

        Config.getGameFolder().ifPresent(this.gameDirectoryChooser::setInitialDirectory);
        Config.getGameFolder().ifPresent(this.gameDirectory::set);

        if (savePath != null) {
            setSelectedSaveFile(savePath);
            handleStartExtract(null);
        }
    }

    public GridPane getScene(Path savePath) {
        if (this.root == null) {
            initialize(savePath);
        }

        return this.root;
    }

    public void setSelectedSaveFile(Path saveFile) {
        if (Files.exists(saveFile) && Files.isReadable(saveFile) && Files.isRegularFile(saveFile) && this.localSavesCombo.getItems().contains(saveFile)) {
            this.localSavesCombo.getSelectionModel().select(this.localSavesCombo.getItems().indexOf(saveFile));
        }
    }

    private void handleOpenGameDirectoryChoose(ActionEvent event) {
        if (!this.loading.get()) {
            Node eventSource = (Node) event.getSource();
            Window actionStage = eventSource.getScene().getWindow();

            chooseGameDirectory(this.gameDirectoryChooser.showDialog(actionStage));
        }
    }

    private boolean chooseGameDirectory(File file) {
        this.gameDirectory.set(file);

        if (this.gameDirectory.getValue() == null) {
            this.selectedGameDirectory.setText(null);
            return false;
        } else {
            this.selectedGameDirectory.setText(this.gameDirectory.getValue().getPath());
            return true;
        }
    }

    public void handleStartExtract(ActionEvent actionEvent) {
        this.loading.set(true);

        ReadSaveTask task = new ReadSaveTask(this.gameDirectory, this.localSavesCombo.getSelectionModel().getSelectedItem(), this.messageSource);
        this.progressText.setVisible(true);
        this.progressText.setFill(Color.BLACK);
        this.progressText.textProperty().bind(task.titleProperty());
        this.progressBar.setVisible(true);
        this.progressBar.progressProperty().bind(task.progressProperty());

        task.setOnFailed(event -> taskError(task.getException()));

        task.addEventFilter(WorkerStateEvent.WORKER_STATE_SUCCEEDED, event -> {
            Config.setGameFolder(this.gameDirectory.getValue());
            this.progressBar.progressProperty().unbind();
            this.progressBar.setProgress(1);

            try {
                this.startExtractButton.getScene()
                                       .setRoot(this.editorController.load(task.getValue(),
                                                                           this.localSavesCombo.getSelectionModel().getSelectedItem().toFile()));
                this.editorController.maximize();
            } catch (Exception e) {
                taskError(e);
            }
        });

        new Thread(task).start();
    }

    private void taskError(Throwable e) {
        LOGGER.error("{} {}", this.messageSource.getMessage("ose.error.extracting", null, Constants.LOCALE), e.getMessage(), e);
        this.progressText.setFill(Color.RED);
        this.progressText.textProperty().unbind();
        this.progressText.setText(this.messageSource.getMessage("ose.error.extracting", null, Constants.LOCALE) + e.getMessage());
        this.loading.set(false);
    }
}
