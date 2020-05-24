package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.province.Province;
import javafx.scene.canvas.Canvas;
import javafx.scene.image.Image;
import javafx.scene.layout.VBox;

public class ProvincesMapView extends AbstractMapView {

    public ProvincesMapView(Province[][] provincesMap, Canvas canvas, Save save) {
        super(provincesMap, canvas, save, MapViewType.PROVINCES_MAP_VIEW);
    }

    @Override
    public void draw() {
        this.canvas.getGraphicsContext2D()
                   .drawImage(new Image(this.save.getGame().getProvincesImage().toURI().toString()), 0, 0);
    }

    @Override
    public void onProvinceSelected(Province province, VBox editPane) {
        editPane.setVisible(true);
        editPane.setMinWidth(300);
        editPane.setPrefWidth(450);
        editPane.setMaxWidth(600);
        editPane.getChildren().clear();
        editPane.getChildren().add(new ProvincePropertySheet(editPane, province));
    }
}
