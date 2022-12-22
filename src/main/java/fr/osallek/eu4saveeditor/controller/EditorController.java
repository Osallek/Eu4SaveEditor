package fr.osallek.eu4saveeditor.controller;

import fr.osallek.eu4parser.common.Eu4Utils;
import fr.osallek.eu4parser.model.game.Culture;
import fr.osallek.eu4parser.model.game.TradeGood;
import fr.osallek.eu4parser.model.game.TradeNode;
import fr.osallek.eu4parser.model.game.localisation.Eu4Language;
import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4parser.model.save.SaveReligion;
import fr.osallek.eu4parser.model.save.country.SaveCountry;
import fr.osallek.eu4parser.model.save.province.SaveProvince;
import fr.osallek.eu4saveeditor.common.Config;
import fr.osallek.eu4saveeditor.common.Constants;
import fr.osallek.eu4saveeditor.common.WriteSaveTask;
import fr.osallek.eu4saveeditor.controller.converter.CountryStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.CultureStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.ProvinceCountryCallBack;
import fr.osallek.eu4saveeditor.controller.converter.ProvinceCountryStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.ProvinceIdStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.ProvinceStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.SaveReligionStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.SuggestionProvider;
import fr.osallek.eu4saveeditor.controller.converter.TradeGoodStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.TradeNodeStringConverter;
import fr.osallek.eu4saveeditor.controller.mapview.CountriesMapView;
import fr.osallek.eu4saveeditor.controller.mapview.MapViewContainer;
import fr.osallek.eu4saveeditor.controller.mapview.MapViewType;
import fr.osallek.eu4saveeditor.controller.pane.ZoomableScrollPane;
import javafx.animation.PauseTransition;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.concurrent.WorkerStateEvent;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
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
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.io.FilenameUtils;
import org.controlsfx.control.MaskerPane;
import org.controlsfx.control.textfield.AutoCompletionBinding;
import org.controlsfx.control.textfield.TextFields;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.MessageSource;
import org.springframework.stereotype.Component;

import javax.imageio.ImageIO;
import java.awt.Polygon;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Comparator;
import java.util.Locale;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Component
public class EditorController {

    private static final Logger LOGGER = LoggerFactory.getLogger(EditorController.class);

    public static SaveCountry dummyCountry;

    private final FileChooser saveFileChooser = new FileChooser();

    private final MessageSource messageSource;

    private File saveFile;

    private Save save;

    private SaveProvince selectedProvince;

    private final Tooltip tooltip = new Tooltip();

    private SaveProvince[][] provincesMap;

    private final BooleanProperty mouseMoving = new SimpleBooleanProperty();

    private final PauseTransition mousePauseTransition = new PauseTransition(Duration.millis(150));

    private double mouseProvinceImageX;

    private double mouseProvinceImageY;

    private double mouseSceneX;

    private double mouseSceneY;

    private boolean wasDragging;

    private MapViewContainer mapViewContainer;

    private ZoomableScrollPane provincesPane;

    private Canvas provincesCanvas;

    private SaveProvince autoCompletedProvince;

    private SaveCountry autoCompletedCountry;

    private StackPane root;

    private Text title;

    private Button saveButton;

    private TextField searchTextField;

    private Button searchButton;

    private VBox editPane;

    private MaskerPane masker;

    public EditorController(MessageSource messageSource) {
        this.messageSource = messageSource;
    }

    public void initialize() {
        this.root = new StackPane();
        this.masker = new MaskerPane();
        this.masker.setVisible(false);

        this.saveFileChooser.setTitle(this.messageSource.getMessage("ose.save-as", null, Constants.LOCALE));
        this.saveFileChooser.getExtensionFilters()
                            .add(new FileChooser.ExtensionFilter(this.messageSource.getMessage("ose.eu-4-ext-desc", null, Constants.LOCALE), "*.eu4"));
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
                    this.tooltip.setText(ProvinceIdStringConverter.INSTANCE.toString(province));
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

        this.searchButton = new Button(this.messageSource.getMessage("ose.go", null, Constants.LOCALE));

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

        BorderPane pane = new BorderPane();
        pane.setCenter(this.provincesPane);
        pane.setRight(this.editPane);
        pane.setTop(top);

        this.root.getChildren().add(pane);
        this.root.getChildren().add(this.masker);
    }

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

    public void onDragDetected(MouseEvent event) {
        this.wasDragging = true;
    }

    public void onMouseMovedProvinceImageView(MouseEvent event) {
        this.mouseMoving.set(true);
        this.mouseProvinceImageX = event.getX();
        this.mouseProvinceImageY = event.getY();
        this.mouseSceneX = event.getSceneX();
        this.mouseSceneY = event.getScreenY();
        this.mousePauseTransition.playFromStart();
    }

    public void onClickExportButton(MouseEvent mouseEvent) {
        if (MouseButton.PRIMARY.equals(mouseEvent.getButton())) {
            File file = this.saveFileChooser.showSaveDialog(((Node) mouseEvent.getSource()).getScene().getWindow());

            if (file != null) {
                WriteSaveTask task = new WriteSaveTask(this.save, file, Eu4Language.getByLocale(Locale.getDefault()));

                task.setOnFailed(event -> {
                    LOGGER.error(this.messageSource.getMessage("ose.error.cant-write", new Object[] {task.getException().getMessage()}, Constants.LOCALE), task.getException());
                    this.title.setText(this.messageSource.getMessage("ose.error.cant-write", new Object[] {task.getException().getLocalizedMessage()}, Constants.LOCALE));
                    this.title.setFill(Paint.valueOf(Color.RED.toString()));
                });

                task.addEventFilter(WorkerStateEvent.WORKER_STATE_SUCCEEDED, event -> {
                    this.masker.setVisible(false);
                    this.masker.textProperty().unbind();
                    this.masker.progressProperty().unbind();
                    this.masker.setText(this.messageSource.getMessage("ose.wait", null, Constants.LOCALE));
                    this.masker.setProgress(-1.0);
                });

                this.masker.setVisible(true);
                this.masker.textProperty().bind(task.titleProperty());
                this.masker.progressProperty().bind(task.progressProperty());

                new Thread(task).start();
            }
        }
    }

    public Pane load(Save save, File saveFile) throws IOException {
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

            ObservableList<SaveProvince> cities = FXCollections.observableArrayList(this.save.getCities())
                                                               .sorted(Comparator.comparing(ProvinceStringConverter.INSTANCE::toString, Eu4Utils.COLLATOR));
            ObservableList<SaveCountry> playableCountries = FXCollections.observableArrayList(this.save.getPlayableCountries())
                                                                         .sorted(Comparator.comparing(CountryStringConverter.INSTANCE::toString, Eu4Utils.COLLATOR));
            ObservableList<SaveCountry> countriesAlive = playableCountries.filtered(SaveCountry::isAlive);
            ObservableList<Culture> cultures = FXCollections.observableArrayList(this.save.getGame().getCultures())
                                                            .sorted(Comparator.comparing(CultureStringConverter.INSTANCE::toString, Eu4Utils.COLLATOR));
            ObservableList<TradeGood> tradeGoods = FXCollections.observableArrayList(this.save.getGame().getTradeGoods())
                                                                .sorted(Comparator.comparing(TradeGoodStringConverter.INSTANCE::toString, Eu4Utils.COLLATOR));
            ObservableList<TradeNode> tradeNodes = FXCollections.observableArrayList(this.save.getGame().getTradeNodes())
                                                                .sorted(Comparator.comparing(TradeNodeStringConverter.INSTANCE::toString, Eu4Utils.COLLATOR));
            ObservableList<SaveReligion> religions = this.save.getReligions()
                                                              .getReligions()
                                                              .values()
                                                              .stream()
                                                              .sorted(Comparator.comparing(SaveReligionStringConverter.INSTANCE::toString, Eu4Utils.COLLATOR))
                                                              .collect(Collectors.toCollection(FXCollections::observableArrayList));
            ObservableList<SaveReligion> playableReligions = religions.filtered(saveReligion -> saveReligion.getGameReligion() != null);

            this.saveButton.setText(this.messageSource.getMessage("ose.save", null, Constants.LOCALE));
            this.searchTextField.setPromptText(this.messageSource.getMessage("ose.search", null, Constants.LOCALE));
            AutoCompletionBinding<Object> autoCompletionBinding = TextFields.bindAutoCompletion(this.searchTextField,
                                                                                                SuggestionProvider.create(new ProvinceCountryCallBack(),
                                                                                                                          Stream.concat(this.save.getProvinces()
                                                                                                                                                 .values()
                                                                                                                                                 .stream()
                                                                                                                                                 .filter(Predicate.not(SaveProvince::isImpassable)),
                                                                                                                                        this.save.getCountries()
                                                                                                                                                 .values()
                                                                                                                                                 .stream()
                                                                                                                                                 .filter(SaveCountry::isAlive))
                                                                                                                                .toList()),
                                                                                                new ProvinceCountryStringConverter());
            autoCompletionBinding.setOnAutoCompleted(event -> {
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

            this.mapViewContainer = new MapViewContainer(this.messageSource, this.provincesMap, this.provincesCanvas, this.editPane, this.save,
                                                         playableCountries, countriesAlive, cultures, religions, playableReligions,
                                                         tradeGoods, tradeNodes, cities);
            this.mapViewContainer.registerMapView(MapViewType.COUNTRIES_MAP_VIEW, new CountriesMapView(this.mapViewContainer, this.save, this.messageSource));
            this.mapViewContainer.selectMapView(MapViewType.COUNTRIES_MAP_VIEW);
            this.mapViewContainer.draw();
        } catch (IOException e) {
            LOGGER.error("{} {}", this.messageSource.getMessage("ose.error.terrain", null, Constants.LOCALE), e.getMessage(), e);
            this.title.setText(this.messageSource.getMessage("ose.error.terrain", null, Constants.LOCALE));
            this.title.setFill(Paint.valueOf(Color.RED.toString()));
            throw e;
        }

        return this.root;
    }

    public void maximize() {
        ((Stage) this.provincesPane.getScene().getWindow()).setMaximized(true);
        ((Stage) this.provincesPane.getScene().getWindow()).setTitle("Eu4 Save Editor - " + this.title.getText());
        if (this.provincesCanvas.getWidth() > this.provincesPane.getViewportBounds().getWidth()) {
            moveToProvince(this.save.getPlayedCountry().getCapital());
        }

        Map<Polygon, Boolean> borders = this.save.getGame().getBorders().get(this.save.getGame().getProvince(this.save.getPlayedCountry().getCapitalId()));

        if (MapUtils.isNotEmpty(borders)) {
            onMouseReleasedProvinceImageView(new MouseEvent(MouseEvent.MOUSE_RELEASED,
                                                            borders.keySet().iterator().next().xpoints[0],
                                                            borders.keySet().iterator().next().ypoints[0],
                                                            borders.keySet().iterator().next().xpoints[0],
                                                            borders.keySet().iterator().next().ypoints[0],
                                                            MouseButton.PRIMARY, 1, false, false,
                                                            false, false, true, false, false, false, false, false,
                                                            null));
        }

        ImageCursor imageCursor = new ImageCursor(new Image(this.save.getGame().getNormalCursorImage().toURI().toString()));
        this.provincesPane.getScene().setCursor(imageCursor);
        this.provincesCanvas.setCursor(imageCursor);
    }

    private void setTitle() {
        this.title.setText(this.save.getName() + " (" + Constants.PRETTY_DATE_FORMAT.format(this.save.getDate()) + ")");
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
