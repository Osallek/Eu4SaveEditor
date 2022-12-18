package fr.osallek.eu4saveeditor.controller;

import fr.osallek.clausewitzparser.common.ClausewitzUtils;
import fr.osallek.eu4parser.common.Eu4Utils;
import fr.osallek.eu4parser.model.game.Culture;
import fr.osallek.eu4parser.model.game.TradeGood;
import fr.osallek.eu4parser.model.game.TradeNode;
import fr.osallek.eu4parser.model.game.localisation.Eu4Language;
import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4parser.model.save.SaveReligion;
import fr.osallek.eu4parser.model.save.country.SaveCountry;
import fr.osallek.eu4parser.model.save.province.SaveProvince;
import fr.osallek.eu4saveeditor.Eu4SaveEditor;
import fr.osallek.eu4saveeditor.common.Config;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import fr.osallek.eu4saveeditor.common.WriteSaveTask;
import fr.osallek.eu4saveeditor.controller.converter.ProvinceCountryCallBack;
import fr.osallek.eu4saveeditor.controller.converter.ProvinceCountryStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.SuggestionProvider;
import fr.osallek.eu4saveeditor.controller.mapview.DrawableProvince;
import fr.osallek.eu4saveeditor.controller.mapview.MapViewContainer;
import fr.osallek.eu4saveeditor.controller.mapview.MapViewType;
import fr.osallek.eu4saveeditor.controller.pane.ZoomableScrollPane;
import fr.osallek.eu4saveeditor.i18n.MenusI18n;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.time.format.DateTimeFormatter;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javafx.animation.PauseTransition;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.concurrent.WorkerStateEvent;
import javafx.fxml.FXML;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.geometry.Rectangle2D;
import javafx.scene.ImageCursor;
import javafx.scene.Node;
import javafx.scene.canvas.Canvas;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.control.Tooltip;
import javafx.scene.image.Image;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.paint.Paint;
import javafx.scene.text.Text;
import javafx.scene.text.TextAlignment;
import javafx.stage.FileChooser;
import javafx.stage.Stage;
import javafx.util.Duration;
import javax.imageio.ImageIO;
import org.apache.commons.io.FilenameUtils;
import org.controlsfx.control.MaskerPane;
import org.controlsfx.control.textfield.AutoCompletionBinding;
import org.controlsfx.control.textfield.TextFields;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class EditorController {

    private static final Logger LOGGER = LoggerFactory.getLogger(EditorController.class);

    public static SaveCountry dummyCountry;

    private static final DateTimeFormatter PRETTY_DATE_FORMAT = DateTimeFormatter.ofPattern("dd MMMM yyyy");

    private final FileChooser saveFileChooser = new FileChooser();

    private File saveFile;

    private Save save;

    private SaveProvince selectedProvince;

    private final Tooltip tooltip = new Tooltip();

    private SaveProvince[][] provincesMap;

    private Map<Integer, DrawableProvince> drawableProvinces;

    private final BooleanProperty mouseMoving = new SimpleBooleanProperty();

    private final PauseTransition mousePauseTransition = new PauseTransition(Duration.millis(150));

    private double mouseProvinceImageX;

    private double mouseProvinceImageY;

    private double mouseSceneX;

    private double mouseSceneY;

    private boolean wasDragging;

    private MapViewContainer mapViewContainer;

    private ImageCursor imageCursor;

    private ZoomableScrollPane provincesPane;

    private Canvas provincesCanvas;

    private ObservableList<SaveProvince> cities;

    private ObservableList<SaveCountry> playableCountries;

    private ObservableList<SaveCountry> countriesAlive;

    private ObservableList<Culture> cultures;

    private ObservableList<SaveReligion> religions;

    private ObservableList<SaveReligion> playableReligions;

    private ObservableList<TradeGood> tradeGoods;

    private ObservableList<TradeNode> tradeNodes;

    private AutoCompletionBinding<Object> autoCompletionBinding;

    private SaveProvince autoCompletedProvince;

    private SaveCountry autoCompletedCountry;

    private StackPane root;

    private Text title;

    private Button saveButton;

    private TextField searchTextField;

    private Button searchButton;

    private VBox editPane;

    private BorderPane pane;

    private MaskerPane masker;

    public void initialize() {
        this.root = new StackPane();
        this.masker = new MaskerPane();
        this.masker.setVisible(false);

        this.saveFileChooser.setTitle(MenusI18n.SAVE_AS.getForDefaultLocale());
        this.saveFileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter(MenusI18n.EU4_EXT_DESC.getForDefaultLocale(), "*.eu4"));
        Config.getSaveFolder().ifPresent(this.saveFileChooser::setInitialDirectory);

        this.provincesCanvas = new Canvas();
        this.provincesCanvas.setOnMouseReleased(this::onMouseReleasedProvinceImageView);
        this.provincesCanvas.setOnMouseMoved(this::onMouseMovedProvinceImageView);
        this.provincesCanvas.setOnDragDetected(this::onDragDetected);

        this.provincesPane = new ZoomableScrollPane(this.provincesCanvas);
        this.provincesPane.setTooltip(this.tooltip);
        this.provincesPane.setFitToHeight(true);
        this.provincesPane.setFitToWidth(true);
        this.provincesPane.setFocusTraversable(true);
        this.provincesPane.setStyle("-fx-focus-color: transparent; -fx-faint-focus-color: transparent;");

        this.mousePauseTransition.setOnFinished(e -> this.mouseMoving.set(false));
        this.mouseMoving.addListener((obs, wasMoving, isNowMoving) -> {
            if (Boolean.FALSE.equals(isNowMoving)) {
                SaveProvince province = this.provincesMap[(int) this.mouseProvinceImageX][(int) this.mouseProvinceImageY];

                if (province != null) {
                    this.tooltip.setText(ClausewitzUtils.removeQuotes(province.getName()) + " (" + province.getId() + ")");
                    this.tooltip.setAnchorX(this.mouseSceneX + 20);
                    this.tooltip.setAnchorY(this.mouseSceneY - 20);
                }
            }
        });

        this.title = new Text();
        this.title.setTextAlignment(TextAlignment.CENTER);
        this.title.setStyle("-fx-font-size: 16px; -fx-font-weight: bold");

        this.saveButton = new Button();
        this.saveButton.onMouseClickedProperty().set(this::onClickExportButton);
        this.saveButton.setStyle("-fx-font-weight: bold");

        this.searchTextField = new TextField();
        this.searchTextField.setMinWidth(100);
        this.searchTextField.setPrefWidth(250);

        this.searchButton = new Button("Go");

        HBox searchBox = new HBox();
        searchBox.getChildren().add(this.searchTextField);
        searchBox.getChildren().add(this.searchButton);

        HBox titleBox = new HBox();
        titleBox.setAlignment(Pos.CENTER);
        titleBox.getChildren().add(this.saveButton);

        HBox top = new HBox(10);
        top.setAlignment(Pos.CENTER);
        top.setPadding(new Insets(3, 0, 3, 0));
        top.getChildren().add(titleBox);
        top.getChildren().add(searchBox);

        this.editPane = new VBox(5);
        this.editPane.setAlignment(Pos.TOP_CENTER);
        this.editPane.setMinWidth(300);
        this.editPane.setPrefWidth(600);
        this.editPane.maxHeight(600);
        this.editPane.setPadding(new Insets(5, 5, 5, 10));

        this.pane = new BorderPane();
        this.pane.setCenter(this.provincesPane);
        this.pane.setRight(this.editPane);
        this.pane.setTop(top);

        this.root.getChildren().add(this.pane);
        this.root.getChildren().add(this.masker);
    }

    @FXML
    public void onMouseReleasedProvinceImageView(MouseEvent event) {
        if (this.wasDragging) {
            this.wasDragging = false;
        } else {
            if (MouseButton.PRIMARY.equals(event.getButton())) {
                SaveProvince province = this.provincesMap[(int) event.getX()][(int) event.getY()];

                if (province != null) {
                    selectProvince(province);
                }
            }
        }
    }

    @FXML
    public void onDragDetected(MouseEvent event) {
        this.wasDragging = true;
    }

    @FXML
    public void onMouseMovedProvinceImageView(MouseEvent event) {
        this.mouseMoving.set(true);
        this.mouseProvinceImageX = event.getX();
        this.mouseProvinceImageY = event.getY();
        this.mouseSceneX = event.getSceneX();
        this.mouseSceneY = event.getScreenY();
        this.mousePauseTransition.playFromStart();
    }

    @FXML
    public void onClickExportButton(MouseEvent mouseEvent) {
        if (MouseButton.PRIMARY.equals(mouseEvent.getButton())) {
            File file = Eu4SaveEditor.override ? this.saveFile : this.saveFileChooser.showSaveDialog(((Node) mouseEvent.getSource()).getScene().getWindow());

            if (file != null) {
                WriteSaveTask task = new WriteSaveTask(this.save, file, Eu4Language.getByLocale(Locale.getDefault()));

                task.setOnFailed(event -> {
                    LOGGER.error("Can't write save {} ! ", task.getException().getMessage(), task.getException());
                    this.title.setText("Can't write save ! " + task.getException().getLocalizedMessage());
                    this.title.setFill(Paint.valueOf(Color.RED.toString()));
                });

                task.addEventFilter(WorkerStateEvent.WORKER_STATE_SUCCEEDED, event -> {
                    this.masker.setVisible(false);
                    this.masker.textProperty().unbind();
                    this.masker.progressProperty().unbind();
                    this.masker.setText("Please Wait...");
                    this.masker.setProgress(-1.0);
                });

                this.masker.setVisible(true);
                this.masker.textProperty().bind(task.titleProperty());
                this.masker.progressProperty().bind(task.progressProperty());

                new Thread(task).start();
            }
        }
    }

    public Pane load(Save save, File saveFile) {
        this.save = save;
        this.saveFile = saveFile;
        int extIndex = FilenameUtils.indexOfExtension(this.save.getName());
        this.saveFileChooser.setInitialFileName(this.save.getName().substring(0, extIndex) + "_edit" + this.save.getName().substring(extIndex));
        Config.getSaveFolder().ifPresent(this.saveFileChooser::setInitialDirectory);
        initialize();

        try {
            EditorController.dummyCountry = this.save.getCountry(Eu4Utils.DEFAULT_TAG);
            BufferedImage provinceImage = ImageIO.read(this.save.getGame().getProvincesImage());
            setTitle();

            this.provincesCanvas.setWidth(provinceImage.getWidth());
            this.provincesCanvas.setHeight(provinceImage.getHeight());

            this.provincesMap = new SaveProvince[provinceImage.getWidth()][provinceImage.getHeight()];

            for (int x = 0; x < provinceImage.getWidth(); x++) {
                for (int y = 0; y < provinceImage.getHeight(); y++) {
                    int[] rgb = provinceImage.getRaster().getPixel(x, y, (int[]) null);
                    SaveProvince province = this.save.getProvinceByColor(rgb[0], rgb[1], rgb[2]);
                    this.provincesMap[x][y] = province;
                }
            }

            this.cities = FXCollections.observableArrayList(this.provincesMap[0][0].getSave().getCities());
            this.playableCountries = FXCollections.observableArrayList(this.provincesMap[0][0].getSave().getPlayableCountries());
            this.countriesAlive = new FilteredList<>(this.playableCountries, SaveCountry::isAlive);
            this.cultures = FXCollections.observableArrayList(this.save.getGame().getCultures());
            this.tradeGoods = FXCollections.observableArrayList(this.save.getGame().getTradeGoods());
            this.tradeNodes = FXCollections.observableArrayList(this.save.getGame().getTradeNodes());
            this.religions = this.save.getReligions()
                                      .getReligions()
                                      .values()
                                      .stream()
                                      .sorted(Comparator.comparing(r -> Eu4SaveEditorUtils.localize(r.getName(), save.getGame()), Eu4Utils.COLLATOR))
                                      .collect(Collectors.toCollection(FXCollections::observableArrayList));
            this.playableReligions = this.save.getReligions()
                                              .getReligions()
                                              .values()
                                              .stream()
                                              .filter(saveReligion -> saveReligion.getGameReligion() != null)
                                              .sorted(Comparator.comparing(r -> Eu4SaveEditorUtils.localize(r.getName(), save.getGame()), Eu4Utils.COLLATOR))
                                              .collect(Collectors.toCollection(FXCollections::observableArrayList));

            this.drawableProvinces = new HashMap<>();
            for (int x = 0; x < this.provincesMap.length; x++) {
                for (int y = 0; y < this.provincesMap[x].length; y++) {
                    SaveProvince province = this.provincesMap[x][y];
                    int startY = y;
                    while (y < this.provincesMap[x].length && Objects.equals(this.provincesMap[x][y], province)) {
                        y++;
                    }
                    y--;

                    if (province != null) {
                        DrawableProvince drawableProvince = this.drawableProvinces.getOrDefault(province.getId(), new DrawableProvince(province));
                        drawableProvince.addRectangle(x, startY, 1, y - startY);
                        this.drawableProvinces.put(province.getId(), drawableProvince);
                    } else {
                        DrawableProvince drawableProvince = this.drawableProvinces.getOrDefault(null, new DrawableProvince(null));
                        drawableProvince.addRectangle(x, startY, 1, y - startY);
                        this.drawableProvinces.put(null, drawableProvince);
                    }
                }
            }

            for (int x = 1; x < this.provincesMap.length; x++) {
                for (int y = 1; y < this.provincesMap[x].length; y++) {
                    SaveProvince province = this.provincesMap[x][y];

                    if (province != null
                        && (!Objects.equals(province, this.provincesMap[x - 1][y]) || !Objects.equals(province, this.provincesMap[x][y - 1]))) {
                        this.drawableProvinces.get(province.getId()).addBorder(x, y - 1);
                    }
                }
            }

            this.saveButton.setText(Eu4SaveEditorUtils.localize("SAVE", save.getGame()));
            this.searchTextField.setPromptText(MenusI18n.SEARCH.getForDefaultLocale());
            this.autoCompletionBinding = TextFields.bindAutoCompletion(this.searchTextField,
                                                                       SuggestionProvider.create(new ProvinceCountryCallBack(),
                                                                                                 Stream.concat(this.save.getProvinces()
                                                                                                                        .values()
                                                                                                                        .stream()
                                                                                                                        .filter(Predicate.not(SaveProvince::isImpassable)),
                                                                                                               this.save.getCountries()
                                                                                                                        .values()
                                                                                                                        .stream()
                                                                                                                        .filter(SaveCountry::isAlive))
                                                                                                       .collect(Collectors.toList())),
                                                                       new ProvinceCountryStringConverter());
            this.autoCompletionBinding.setOnAutoCompleted(event -> {
                if (SaveProvince.class.equals(event.getCompletion().getClass())) {
                    this.autoCompletedProvince = (SaveProvince) event.getCompletion();
                    this.autoCompletedCountry = null;
                } else if (SaveCountry.class.equals(event.getCompletion().getClass())) {
                    this.autoCompletedCountry = (SaveCountry) event.getCompletion();
                    this.autoCompletedProvince = null;
                } else {
                    this.autoCompletedProvince = null;
                    this.autoCompletedCountry = null;
                }
            });

            this.searchButton.setOnAction(event -> {
                if (this.autoCompletedProvince != null) {
                    selectProvince(this.autoCompletedProvince);
                    moveToProvince(this.autoCompletedProvince);
                } else if (this.autoCompletedCountry != null) {
                    selectProvince(this.autoCompletedCountry.getCapital());
                    moveToProvince(this.autoCompletedCountry.getCapital());
                }
            });

            this.mapViewContainer = new MapViewContainer(this.provincesMap, this.drawableProvinces, this.provincesCanvas, this.editPane, this.save,
                                                         this.playableCountries, this.countriesAlive, this.cultures, this.religions, this.playableReligions,
                                                         this.tradeGoods, this.tradeNodes, this.cities);
            this.mapViewContainer.registerMapView(MapViewType.COUNTRIES_MAP_VIEW);
            this.mapViewContainer.selectMapView(MapViewType.COUNTRIES_MAP_VIEW);
            this.mapViewContainer.draw();
        } catch (IOException e) {
            LOGGER.error("Can't load terrain image ! Make sure your game files are not corrupted {} !", e.getMessage(), e);
            this.title.setText("Can't load terrain image ! Make sure your game files are not corrupted !");
            this.title.setFill(Paint.valueOf(Color.RED.toString()));
        }

        return this.root;
    }

    public void maximize() {
        ((Stage) this.provincesPane.getScene().getWindow()).setMaximized(true);
        if (this.provincesCanvas.getWidth() > this.provincesPane.getViewportBounds().getWidth()) {
            if (this.drawableProvinces.get(this.save.getPlayedCountry().getCapitalId()) != null
                && this.drawableProvinces.get(this.save.getPlayedCountry().getCapitalId()).getRectangles() != null) {
                moveToProvince(this.save.getPlayedCountry().getCapital());
            }
        }

        if (this.drawableProvinces.get(this.save.getPlayedCountry().getCapitalId()) != null
            && this.drawableProvinces.get(this.save.getPlayedCountry().getCapitalId()).getRectangles() != null) {
            Rectangle2D rectangle = this.drawableProvinces.get(this.save.getPlayedCountry().getCapitalId())
                                                          .getRectangles()
                                                          .get(0);

            onMouseReleasedProvinceImageView(new MouseEvent(MouseEvent.MOUSE_RELEASED, rectangle.getMinX(), rectangle.getMinY(),
                                                            rectangle.getMinX(), rectangle.getMinY(), MouseButton.PRIMARY, 1, false, false,
                                                            false, false, true, false, false, false, false, false,
                                                            null));
        }

        this.imageCursor = new ImageCursor(new Image(this.save.getGame().getNormalCursorImage().toURI().toString()));
        this.provincesPane.getScene().setCursor(this.imageCursor);
        this.provincesCanvas.setCursor(this.imageCursor);
    }

    private void setTitle() {
        this.title.setText(this.save.getName() + " (" + PRETTY_DATE_FORMAT.format(this.save.getDate()) + ")");
    }

    private void moveToProvince(SaveProvince province) {
        double x;
        double y;
        double provinceY = this.provincesCanvas.getHeight() - province.getCityY();

        if (province.getCityX() < this.provincesPane.getWidth() / 2) {
            x = 0;
        } else if (province.getCityX() > (this.provincesCanvas.getWidth() - this.provincesPane.getWidth() / 2)) {
            x = 1;
        } else {
            x = (province.getCityX() - this.provincesPane.getWidth() / 2) / (this.provincesCanvas.getWidth() - this.provincesPane.getWidth());
        }

        if (provinceY < this.provincesPane.getHeight() / 2) {
            y = 0;
        } else if (provinceY > (this.provincesCanvas.getHeight() - this.provincesPane.getHeight() / 2)) {
            y = 1;
        } else {
            y = (provinceY - this.provincesPane.getHeight() / 2) / (this.provincesCanvas.getHeight() - this.provincesPane.getHeight());
        }

        this.provincesPane.setHvalue(this.provincesPane.getHmax() * x);
        this.provincesPane.setVvalue(this.provincesPane.getVmax() * y);
    }

    private void selectProvince(SaveProvince province) {
        if (this.selectedProvince != province) {
            this.selectedProvince = province;
            this.mapViewContainer.onProvinceSelected(this.selectedProvince);
        }
    }
}
