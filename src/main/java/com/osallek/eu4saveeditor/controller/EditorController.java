package com.osallek.eu4saveeditor.controller;

import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.province.SaveProvince;
import com.osallek.eu4saveeditor.controller.mapview.AbstractMapView;
import com.osallek.eu4saveeditor.controller.mapview.CountriesMapView;
import com.osallek.eu4saveeditor.controller.mapview.MapViewType;
import com.osallek.eu4saveeditor.controller.mapview.ProvincesMapView;
import javafx.animation.PauseTransition;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.ImageCursor;
import javafx.scene.canvas.Canvas;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.Tooltip;
import javafx.scene.image.Image;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.paint.Paint;
import javafx.scene.text.Text;
import javafx.stage.Stage;
import javafx.util.Duration;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.EnumMap;
import java.util.Map;
import java.util.ResourceBundle;

public class EditorController implements Initializable {

    private static final DateFormat PRETTY_DATE_FORMAT = new SimpleDateFormat("dd MMMM yyyy");

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

    private Map<MapViewType, AbstractMapView> mapViews = new EnumMap<>(MapViewType.class);

    private AbstractMapView selectedMapView;

    private ImageCursor imageCursor;

    @FXML
    private Text title;

    @FXML
    private ScrollPane provincesPane;

    @FXML
    private Canvas provincesCanvas;

    @FXML
    private VBox editPane;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        this.provincesPane.setTooltip(this.tooltip);
        this.provincesPane.setPannable(true);

        this.mousePauseTransition.setOnFinished(e -> this.mouseMoving.set(false));
        this.mouseMoving.addListener((obs, wasMoving, isNowMoving) -> {
            if (Boolean.FALSE.equals(isNowMoving)) {
                SaveProvince province = this.provincesMap[(int) this.mouseProvinceImageX][(int) this.mouseProvinceImageY];

                this.tooltip.setText(province.getName() + " (" + province.getId() + ")");
                this.tooltip.setAnchorX(this.mouseSceneX + 20);
                this.tooltip.setAnchorY(this.mouseSceneY - 20);
            }
        });
    }

    @FXML
    public void onMouseReleasedProvinceImageView(MouseEvent event) {
        if (this.wasDragging) {
            this.wasDragging = false;
        } else {
            if (MouseButton.PRIMARY.equals(event.getButton())) {
                this.selectedProvince = this.provincesMap[(int) event.getX()][(int) event.getY()];
                this.selectedMapView.onProvinceSelected(this.selectedProvince, this.editPane);
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
    public void onKeyPressedProvinceImageView(KeyEvent keyEvent) {
        if (KeyCode.C.equals(keyEvent.getCode())) {
            if (!selectedMapView.equals(this.mapViews.get(MapViewType.COUNTRIES_MAP_VIEW))) {
                this.selectedMapView = this.mapViews.get(MapViewType.COUNTRIES_MAP_VIEW);
                this.selectedMapView.draw();
            }
        } else if (KeyCode.P.equals(keyEvent.getCode())) {
            if (!selectedMapView.equals(this.mapViews.get(MapViewType.PROVINCES_MAP_VIEW))) {
                this.selectedMapView = this.mapViews.get(MapViewType.PROVINCES_MAP_VIEW);
                this.selectedMapView.draw();
            }
        }
    }

    public void load(Save save) {
        this.save = save;

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

            this.mapViews.put(MapViewType.PROVINCES_MAP_VIEW,
                              new ProvincesMapView(this.provincesMap, this.provincesCanvas, this.save));
            this.mapViews.put(MapViewType.COUNTRIES_MAP_VIEW,
                              new CountriesMapView(this.provincesMap, this.provincesCanvas, this.save));
            this.selectedMapView = this.mapViews.get(MapViewType.PROVINCES_MAP_VIEW);
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
    }

    private void setTitle() {
        this.title.setText(this.save.getName() + " (" + PRETTY_DATE_FORMAT.format(this.save.getDate()) + ")");
    }
}
