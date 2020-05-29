package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.province.SaveProvince;
import javafx.geometry.Point2D;
import javafx.scene.canvas.Canvas;
import javafx.scene.image.PixelWriter;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public abstract class AbstractMapView {

    protected final SaveProvince[][] provincesMap;

    protected final Canvas canvas;

    protected final Save save;

    protected final MapViewType type;

    private List<Point2D> borders;

    public AbstractMapView(SaveProvince[][] provincesMap, Canvas canvas, Save save, MapViewType type) {
        this.provincesMap = provincesMap;
        this.canvas = canvas;
        this.save = save;
        this.type = type;
    }

    public void drawProvincesBorders() {
        if (this.borders == null) {
            this.borders = new ArrayList<>();
            for (int x = 1; x < this.provincesMap.length; x++) {
                for (int y = 1; y < this.provincesMap[x].length; y++) {
                    SaveProvince province = this.provincesMap[x][y];
                    if (!province.equals(this.provincesMap[x - 1][y])
                        || !province.equals(this.provincesMap[x][y - 1])) {
                        this.borders.add(new Point2D(x, y));
                    }
                }
            }
        }

        PixelWriter pixelWriter = this.canvas.getGraphicsContext2D().getPixelWriter();
        this.borders.forEach(point2D -> pixelWriter.setColor((int) point2D.getX(), (int) point2D.getY(), Color.BLACK));
    }

    public abstract void draw();

    public abstract void onProvinceSelected(SaveProvince province, VBox editPane);

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof AbstractMapView)) {
            return false;
        }
        AbstractMapView that = (AbstractMapView) o;
        return type == that.type;
    }

    @Override
    public int hashCode() {
        return Objects.hash(type);
    }
}
