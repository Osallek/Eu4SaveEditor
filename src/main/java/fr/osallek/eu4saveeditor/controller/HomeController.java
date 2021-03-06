package fr.osallek.eu4saveeditor.controller;

import fr.osallek.eu4parser.model.game.localisation.Eu4Language;
import fr.osallek.eu4saveeditor.Eu4SaveEditor;
import fr.osallek.eu4saveeditor.common.Config;
import fr.osallek.eu4saveeditor.common.Constants;
import fr.osallek.eu4saveeditor.common.FileProperty;
import fr.osallek.eu4saveeditor.common.ReadSaveTask;
import fr.osallek.eu4saveeditor.controller.converter.FileStringConverter;
import fr.osallek.eu4saveeditor.i18n.MenusI18n;
import javafx.concurrent.WorkerStateEvent;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.control.Button;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.TextField;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;
import javafx.stage.Window;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.jar.Manifest;

public class HomeController implements Initializable {

    private static final Logger LOGGER = LoggerFactory.getLogger(HomeController.class);

    private final FXMLLoader editorLoader = new FXMLLoader(Eu4SaveEditor.class.getResource(Constants.TEMPLATE_EDITOR));

    private final DirectoryChooser gameDirectoryChooser = new DirectoryChooser();

    private final DirectoryChooser modDirectoryChooser = new DirectoryChooser();

    private final FileChooser saveFileChooser = new FileChooser();

    private final FileProperty gameDirectory = new FileProperty(this, "gameDirectory");

    private final FileProperty modDirectory = new FileProperty(this, "modDirectory");

    private final FileProperty saveFile = new FileProperty(this, "saveFile");

    private boolean canOpenGameDirectoryChoose = true;

    private boolean canOpenModDirectoryChoose = true;

    private boolean canOpenSaveFileChooser = true;

    @FXML
    public Text selectGameFolderText;

    @FXML
    public Text selectModFolderText;

    @FXML
    public Text selectSaveFileText;

    @FXML
    private TextField selectedGameDirectory;

    @FXML
    private TextField selectedModDirectory;

    @FXML
    private TextField selectedSaveFile;

    @FXML
    public Button chooseGameFolderButton;

    @FXML
    public Button chooseModFolderButton;

    @FXML
    public Button chooseSaveFileButton;

    @FXML
    private Button startExtractButton;

    @FXML
    private Text infoText;

    @FXML
    private ProgressBar progressBar;

    @FXML
    private Text versionText;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        this.selectGameFolderText.setText(MenusI18n.SELECT_GAME_FOLDER_DESC.getForDefaultLocale());
        this.selectModFolderText.setText(MenusI18n.SELECT_MOD_FOLDER_DESC.getForDefaultLocale());
        this.selectSaveFileText.setText(MenusI18n.SELECT_SAVE_FILE_DESC.getForDefaultLocale());

        this.chooseGameFolderButton.setText(MenusI18n.CHOOSE_FOLDER.getForDefaultLocale());
        this.chooseModFolderButton.setText(MenusI18n.CHOOSE_FOLDER.getForDefaultLocale());
        this.chooseSaveFileButton.setText(MenusI18n.CHOOSE_FILE.getForDefaultLocale());

        this.startExtractButton.setText(MenusI18n.START_EXTRACT.getForDefaultLocale());

        this.gameDirectoryChooser.setTitle(MenusI18n.SELECT_GAME_FOLDER.getForDefaultLocale());
        this.modDirectoryChooser.setTitle(MenusI18n.SELECT_GAME_FOLDER.getForDefaultLocale());

        this.saveFileChooser.setTitle(MenusI18n.SELECT_SAVE_FILE.getForDefaultLocale());
        this.saveFileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter(MenusI18n.EU4_EXT_DESC.getForDefaultLocale(), "*.eu4"));

        this.selectedGameDirectory.textProperty().bindBidirectional(this.gameDirectory, new FileStringConverter());
        this.selectedModDirectory.textProperty().bindBidirectional(this.modDirectory, new FileStringConverter());
        this.selectedSaveFile.textProperty().bindBidirectional(this.saveFile, new FileStringConverter());

        this.gameDirectoryChooser.setInitialDirectory(Config.getGameFolder());
        this.gameDirectory.set(Config.getGameFolder());

        this.modDirectoryChooser.setInitialDirectory(Config.getModFolder());
        this.modDirectory.set(Config.getModFolder());

        this.saveFileChooser.setInitialDirectory(Config.getSaveFolder());
        this.selectedSaveFile.setText(Config.getSaveFile() == null ? null : Config.getSaveFile().getAbsolutePath());

        try {
            Manifest manifest = new Manifest(Eu4SaveEditor.class.getResourceAsStream("META-INF/MANIFEST.MF"));
            this.versionText.setText("Version " + manifest.getMainAttributes().getValue("Implementation-Version")
                                     + " | Supported game version: " + manifest.getMainAttributes().getValue("Supported-Version"));
        } catch (IOException ignored) {
        }

        enableStartExtractButton();
    }

    public boolean setGameDirectory(String gameDirectory) {
        File file = new File(gameDirectory);

        if (file.exists() && file.canRead() && file.isDirectory()) {
            return chooseGameDirectory(file);
        }

        return false;
    }

    public boolean setModDirectory(String modDirectory) {
        File file = new File(modDirectory);

        if (file.exists() && file.canRead() && file.isDirectory()) {
            return chooseModDirectory(file);
        }

        return false;
    }

    public boolean setSelectedSaveFile(String selectedSaveFile) {
        File file = new File(selectedSaveFile);

        if (file.exists() && file.canRead() && file.isFile()) {
            return chooseSaveFile(file);
        }

        return false;
    }

    @FXML
    private void handleOpenGameDirectoryChoose(ActionEvent event) {
        if (this.canOpenGameDirectoryChoose) {
            Node eventSource = (Node) event.getSource();
            Window actionStage = eventSource.getScene().getWindow();

            chooseGameDirectory(this.gameDirectoryChooser.showDialog(actionStage));
        }
    }

    private boolean chooseGameDirectory(File file) {
        this.gameDirectory.set(file);

        if (this.gameDirectory.getValue() == null) {
            this.startExtractButton.setDisable(true);
            this.selectedGameDirectory.setText(null);
            return false;
        } else {
            enableStartExtractButton();
            this.selectedGameDirectory.setText(this.gameDirectory.getValue().getPath());
            return true;
        }
    }

    @FXML
    private void handleOpenModDirectoryChoose(ActionEvent event) {
        if (this.canOpenModDirectoryChoose) {
            Node eventSource = (Node) event.getSource();
            Window actionStage = eventSource.getScene().getWindow();

            chooseModDirectory(this.modDirectoryChooser.showDialog(actionStage));
        }
    }

    private boolean chooseModDirectory(File file) {
        this.modDirectory.set(file);

        if (this.modDirectory.getValue() == null) {
            this.startExtractButton.setDisable(true);
            this.selectedModDirectory.setText(null);
            return false;
        } else {
            enableStartExtractButton();
            this.selectedModDirectory.setText(this.modDirectory.getValue().getPath());
            return true;
        }
    }

    @FXML
    private void handleOpenSaveFileChoose(ActionEvent event) {
        if (this.canOpenSaveFileChooser) {
            Node eventSource = (Node) event.getSource();
            Window actionStage = eventSource.getScene().getWindow();

            chooseSaveFile(this.saveFileChooser.showOpenDialog(actionStage));

            if (this.saveFile.getValue() == null) {
                this.startExtractButton.setDisable(true);
                this.selectedSaveFile.setText(null);
            } else {
                enableStartExtractButton();
                this.selectedSaveFile.setText(this.saveFile.getValue().getPath());
            }
        }
    }

    private boolean chooseSaveFile(File file) {
        this.saveFile.set(file);

        if (this.saveFile.getValue() == null) {
            this.startExtractButton.setDisable(true);
            this.selectedSaveFile.setText(null);
            return false;
        } else {
            enableStartExtractButton();
            this.selectedSaveFile.setText(this.saveFile.getValue().getPath());
            return true;
        }
    }

    @FXML
    public void handleStartExtract(ActionEvent actionEvent) {
        this.startExtractButton.setDisable(true);
        this.canOpenGameDirectoryChoose = false;
        this.canOpenModDirectoryChoose = false;
        this.canOpenSaveFileChooser = false;
        Config.setGameFolder(this.gameDirectory.getValue());
        Config.setModFolder(this.modDirectory.getValue());
        Config.setSaveFile(this.saveFile.getValue());

        ReadSaveTask task = new ReadSaveTask(this.gameDirectory, this.modDirectory, this.saveFile, Eu4Language.getByLocale(Locale.getDefault()));
        task.setOnFailed(event -> {
            LOGGER.error("An error occurred while extracting the save: {}", task.getException().getMessage(), task.getException());
            this.infoText.setFill(Color.RED);
            this.infoText.textProperty().unbind();
            this.infoText.setText("An error occurred while extracting the save: " + task.getException().getMessage());
            this.canOpenGameDirectoryChoose = true;
            this.canOpenModDirectoryChoose = true;
            this.canOpenSaveFileChooser = true;
        });

        task.addEventFilter(WorkerStateEvent.WORKER_STATE_SUCCEEDED, event -> {
            try {
                this.progressBar.progressProperty().unbind();
                this.progressBar.setProgress(1);
                Parent editorNode = this.editorLoader.load();
                ((EditorController) this.editorLoader.getController()).load(task.getValue(), this.saveFile.get());
                this.startExtractButton.getScene().setRoot(editorNode);
                ((EditorController) this.editorLoader.getController()).maximize();
            } catch (IOException e) {
                LOGGER.error("An error occurred while extracting the save: {}", e.getMessage(), e);
                this.infoText.setFill(Color.RED);
                this.infoText.setText("An error occurred while extracting the save: " + e.getMessage());
                this.canOpenGameDirectoryChoose = true;
                this.canOpenModDirectoryChoose = true;
                this.canOpenSaveFileChooser = true;
            }
        });

        this.infoText.setVisible(true);
        this.infoText.setFill(Color.BLACK);
        this.infoText.textProperty().bind(task.titleProperty());
        this.progressBar.setVisible(true);
        this.progressBar.progressProperty().bind(task.progressProperty());

        new Thread(task).start();
    }

    private void enableStartExtractButton() {
        if (this.saveFile.getValue() != null && this.gameDirectory.getValue() != null) {
            this.startExtractButton.setDisable(false);
        }
    }
}
