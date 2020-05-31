package com.osallek.eu4saveeditor.controller;

import com.osallek.eu4parser.Eu4Parser;
import com.osallek.eu4parser.model.game.Culture;
import com.osallek.eu4parser.model.game.Religion;
import com.osallek.eu4parser.model.game.TradeGood;
import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4parser.model.save.province.SaveProvince;
import com.osallek.eu4saveeditor.common.Constants;
import com.osallek.eu4saveeditor.controller.mapview.AbstractMapView;
import com.osallek.eu4saveeditor.controller.mapview.CountriesMapView;
import com.osallek.eu4saveeditor.controller.mapview.MapViewType;
import com.osallek.eu4saveeditor.controller.pane.ZoomableScrollPane;
import com.osallek.eu4saveeditor.i18n.MenusI18n;
import javafx.animation.PauseTransition;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
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

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.EnumMap;
import java.util.Map;
import java.util.ResourceBundle;

public class EditorController implements Initializable {

    private static final DateFormat PRETTY_DATE_FORMAT = new SimpleDateFormat("dd MMMM yyyy");

    private final FileChooser saveFileChooser = new FileChooser();

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

    private final Map<MapViewType, AbstractMapView> mapViews = new EnumMap<>(MapViewType.class);

    private AbstractMapView selectedMapView;

    private ImageCursor imageCursor;

    private ZoomableScrollPane provincesPane;

    private Canvas provincesCanvas;

    private ObservableList<Country> playableCountries;

    private ObservableList<Culture> cultures;

    private ObservableList<Religion> religions;

    private ObservableList<TradeGood> tradeGoods;

    @FXML
    private Text title;

    @FXML
    private Button saveButton;

    @FXML
    private VBox editPane;

    @FXML
    private BorderPane pane;

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

        this.mousePauseTransition.setOnFinished(e -> this.mouseMoving.set(false));
        this.mouseMoving.addListener((obs, wasMoving, isNowMoving) -> {
            if (Boolean.FALSE.equals(isNowMoving)) {
                SaveProvince province = this.provincesMap[(int) this.mouseProvinceImageX][(int) this.mouseProvinceImageY];

                this.tooltip.setText(province.getName() + " (" + province.getId() + ")");
                this.tooltip.setAnchorX(this.mouseSceneX + 20);
                this.tooltip.setAnchorY(this.mouseSceneY - 20);
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
                this.selectedProvince = this.provincesMap[(int) event.getX()][(int) event.getY()];
                this.selectedMapView.onProvinceSelected(this.selectedProvince);
                this.selectedMapView.setSelected(true);
                this.mapViews.values().forEach(abstractMapView -> {
                    if (!abstractMapView.equals(this.selectedMapView)) {
                        abstractMapView.setSelected(false);
                    }
                });
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
    public void onClickExportButton(MouseEvent event) {
        if (MouseButton.PRIMARY.equals(event.getButton())) {
            try {
                File file = this.saveFileChooser.showSaveDialog(((Node) event.getSource()).getScene().getWindow());

                if (file != null) {
                    Eu4Parser.writeSave(this.save, file.toString());
                }
            } catch (IOException e) {
                this.title.setText("Can't write save ! " + e.getLocalizedMessage());
                this.title.setFill(Paint.valueOf(Color.RED.toString()));
            }
        }
    }

    public void load(Save save) {
        this.save = save;
        int extIndex = this.save.getName().lastIndexOf('.');
        this.saveFileChooser.setInitialFileName(this.save.getName().substring(0, extIndex) + "_edit" + this.save.getName().substring(extIndex));

        try {
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

            this.playableCountries = FXCollections.observableArrayList(this.provincesMap[0][0].getSave()
                                                                                              .getPlayableCountries());
            this.cultures = FXCollections.observableArrayList(this.provincesMap[0][0].getSave()
                                                                                     .getGame()
                                                                                     .getCultures());
            this.religions = FXCollections.observableArrayList(this.provincesMap[0][0].getSave()
                                                                                      .getGame()
                                                                                      .getReligions());
            this.tradeGoods = FXCollections.observableArrayList(this.provincesMap[0][0].getSave()
                                                                                       .getGame()
                                                                                       .getTradeGoods());

            this.saveButton.setText(this.save.getGame().getLocalisation("SAVE"));

            this.mapViews.put(MapViewType.COUNTRIES_MAP_VIEW,
                              new CountriesMapView(this.provincesMap, this.provincesCanvas, this.editPane, this.save,
                                                   this.playableCountries, this.cultures, this.religions, this.tradeGoods));
            this.selectedMapView = this.mapViews.get(MapViewType.COUNTRIES_MAP_VIEW);
            this.selectedMapView.draw();
        } catch (IOException e) {
            this.title.setText("Can't load terrain image ! Make sure your game files are not corrupted !");
            this.title.setFill(Paint.valueOf(Color.RED.toString()));
        }
    }

    public void maximize() {
        ((Stage) this.provincesPane.getScene().getWindow()).setMaximized(true);
        if (this.provincesCanvas.getWidth() > this.provincesPane.getViewportBounds().getWidth()) {
            this.provincesPane.setHvalue(
                    this.provincesPane.getHmax() * (2850 / this.provincesCanvas.getWidth()));
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
