package fr.osallek.eu4saveeditor.controller;

import com.sun.javafx.geom.Rectangle;
import fr.osallek.eu4parser.common.Eu4Utils;
import fr.osallek.eu4parser.model.game.Culture;
import fr.osallek.eu4parser.model.game.TradeGood;
import fr.osallek.eu4parser.model.game.localisation.Eu4Language;
import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4parser.model.save.SaveReligion;
import fr.osallek.eu4parser.model.save.country.Country;
import fr.osallek.eu4parser.model.save.province.SaveProvince;
import fr.osallek.eu4saveeditor.common.Constants;
import fr.osallek.eu4saveeditor.common.WriteSaveTask;
import fr.osallek.eu4saveeditor.controller.mapview.DrawableProvince;
import fr.osallek.eu4saveeditor.controller.mapview.MapViewContainer;
import fr.osallek.eu4saveeditor.controller.mapview.MapViewType;
import fr.osallek.eu4saveeditor.controller.pane.ZoomableScrollPane;
import fr.osallek.eu4saveeditor.i18n.MenusI18n;
import javafx.animation.PauseTransition;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.concurrent.WorkerStateEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.ImageCursor;
import javafx.scene.Node;
import javafx.scene.canvas.Canvas;
import javafx.scene.control.Button;
import javafx.scene.control.Tooltip;
import javafx.scene.image.Image;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.paint.Paint;
import javafx.scene.text.Text;
import javafx.stage.FileChooser;
import javafx.stage.Stage;
import javafx.util.Duration;
import org.controlsfx.control.MaskerPane;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.time.format.DateTimeFormatter;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.ResourceBundle;
import java.util.stream.Collectors;

public class EditorController implements Initializable {

    private static final Logger LOGGER = LoggerFactory.getLogger(EditorController.class);

    public static Country dummyCountry;

    private static final DateTimeFormatter PRETTY_DATE_FORMAT = DateTimeFormatter.ofPattern("dd MMMM yyyy");

    private final FileChooser saveFileChooser = new FileChooser();

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

    private ObservableList<Country> playableCountries;

    private ObservableList<Country> countriesAlive;

    private ObservableList<Culture> cultures;

    private ObservableList<SaveReligion> religions;

    private ObservableList<SaveReligion> playableReligions;

    private ObservableList<TradeGood> tradeGoods;

    @FXML
    private Text title;

    @FXML
    private Button saveButton;

    @FXML
    private VBox editPane;

    @FXML
    private BorderPane pane;

    @FXML
    private MaskerPane masker;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        this.saveFileChooser.setTitle(MenusI18n.SAVE_AS.getForDefaultLocale());
        this.saveFileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter(MenusI18n.EU4_EXT_DESC.getForDefaultLocale(), "*.eu4"));

        if (Constants.DOCUMENTS_FOLDER.exists()) {
            this.saveFileChooser.setInitialDirectory(Constants.SAVES_FOLDER);
        }

        this.provincesCanvas = new Canvas();
        this.provincesCanvas.setOnMouseReleased(this::onMouseReleasedProvinceImageView);
        this.provincesCanvas.setOnMouseMoved(this::onMouseMovedProvinceImageView);
        this.provincesCanvas.setOnDragDetected(this::onDragDetected);

        this.provincesPane = new ZoomableScrollPane(this.provincesCanvas);
        this.provincesPane.setTooltip(this.tooltip);
        this.provincesPane.setFocusTraversable(true);
        this.provincesPane.setStyle("-fx-focus-color: transparent; -fx-faint-focus-color: transparent;");

        this.mousePauseTransition.setOnFinished(e -> this.mouseMoving.set(false));
        this.mouseMoving.addListener((obs, wasMoving, isNowMoving) -> {
            if (Boolean.FALSE.equals(isNowMoving)) {
                SaveProvince province = this.provincesMap[(int) this.mouseProvinceImageX][(int) this.mouseProvinceImageY];

                if (province != null) {
                    this.tooltip.setText(province.getName() + " (" + province.getId() + ")");
                    this.tooltip.setAnchorX(this.mouseSceneX + 20);
                    this.tooltip.setAnchorY(this.mouseSceneY - 20);
                }
            }
        });

        this.pane.setCenter(this.provincesPane);
    }

    @FXML
    public void onMouseReleasedProvinceImageView(MouseEvent event) {
        if (this.wasDragging) {
            this.wasDragging = false;
        } else {
            if (MouseButton.PRIMARY.equals(event.getButton())) {
                SaveProvince province = this.provincesMap[(int) event.getX()][(int) event.getY()];

                if (province != null) {
                    this.selectedProvince = province;
                    this.mapViewContainer.onProvinceSelected(this.selectedProvince);
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
            File file = this.saveFileChooser.showSaveDialog(((Node) mouseEvent.getSource()).getScene().getWindow());

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

    public void load(Save save) {
        this.save = save;
        int extIndex = this.save.getName().lastIndexOf('.');
        this.saveFileChooser.setInitialFileName(this.save.getName().substring(0, extIndex) + "_edit" + this.save.getName().substring(extIndex));

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

            this.playableCountries = FXCollections.observableArrayList(this.provincesMap[0][0].getSave()
                                                                                              .getPlayableCountries());

            this.countriesAlive = new FilteredList<>(this.playableCountries, Country::isAlive);
            this.cultures = FXCollections.observableArrayList(this.save.getGame().getCultures());
            this.tradeGoods = FXCollections.observableArrayList(this.save.getGame().getTradeGoods());
            this.religions = this.save.getReligions()
                                      .getReligions()
                                      .values()
                                      .stream()
                                      .sorted(Comparator.comparing(SaveReligion::getLocalizedName, Eu4Utils.COLLATOR))
                                      .collect(Collectors.toCollection(FXCollections::observableArrayList));
            this.playableReligions = this.save.getReligions()
                                              .getReligions()
                                              .values()
                                              .stream()
                                              .filter(saveReligion -> saveReligion.getGameReligion() != null)
                                              .sorted(Comparator.comparing(r -> r.getGameReligion().getLocalizedName(), Eu4Utils.COLLATOR))
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

            this.saveButton.setText(this.save.getGame().getLocalisation("SAVE"));

            this.mapViewContainer = new MapViewContainer(this.provincesMap, this.drawableProvinces, this.provincesCanvas, this.editPane, this.save,
                                                         this.playableCountries, this.countriesAlive, this.cultures, this.religions, this.playableReligions,
                                                         this.tradeGoods, this.cities);
            this.mapViewContainer.registerMapView(MapViewType.COUNTRIES_MAP_VIEW);
            this.mapViewContainer.selectMapView(MapViewType.COUNTRIES_MAP_VIEW);
            this.mapViewContainer.draw();
        } catch (IOException e) {
            LOGGER.error("Can't load terrain image ! Make sure your game files are not corrupted {} !", e.getMessage(), e);
            this.title.setText("Can't load terrain image ! Make sure your game files are not corrupted !");
            this.title.setFill(Paint.valueOf(Color.RED.toString()));
        }
    }

    public void maximize() {
        ((Stage) this.provincesPane.getScene().getWindow()).setMaximized(true);
        if (this.provincesCanvas.getWidth() > this.provincesPane.getViewportBounds().getWidth()) {
            if (this.drawableProvinces.get(this.save.getPlayedCountry().getCapitalId()) != null
                && this.drawableProvinces.get(this.save.getPlayedCountry().getCapitalId()).getRectangles() != null) {
                Rectangle rectangle = this.drawableProvinces.get(this.save.getPlayedCountry().getCapitalId())
                                                            .getRectangles()
                                                            .get(0);
                this.provincesPane.setHvalue(
                        this.provincesPane.getHmax() * (rectangle.x / this.provincesCanvas.getWidth()));

                this.provincesPane.setVvalue(
                        this.provincesPane.getVmax() * (rectangle.y / this.provincesCanvas.getHeight()));
            }
        }

        if (this.drawableProvinces.get(this.save.getPlayedCountry().getCapitalId()) != null
            && this.drawableProvinces.get(this.save.getPlayedCountry().getCapitalId()).getRectangles() != null) {
            Rectangle rectangle = this.drawableProvinces.get(this.save.getPlayedCountry().getCapitalId())
                                                        .getRectangles()
                                                        .get(0);

            onMouseReleasedProvinceImageView(new MouseEvent(MouseEvent.MOUSE_RELEASED, rectangle.x, rectangle.y,
                                                            rectangle.x, rectangle.y, MouseButton.PRIMARY, 1, false, false,
                                                            false, false, true, false, false, false, false, false,
                                                            null));
        }

        this.imageCursor = new ImageCursor(new Image(this.save.getGame()
                                                              .getNormalCursorImage()
                                                              .toURI()
                                                              .toString()));

        this.provincesCanvas.getScene().setCursor(this.imageCursor);
        this.provincesCanvas.setCursor(this.imageCursor);
    }

    private void setTitle() {
        this.title.setText(this.save.getName() + " (" + PRETTY_DATE_FORMAT.format(this.save.getDate()) + ")");
    }
}
