package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.eu4parser.model.save.province.SaveProvince;
import javafx.geometry.Point2D;
import javafx.scene.shape.Rectangle;

import java.util.ArrayList;
import java.util.List;

public class DrawableProvince {

    private final SaveProvince province;

    private final List<Rectangle> rectangles;

    private final List<Point2D> borders;

    public DrawableProvince(SaveProvince province) {
        this.province = province;
        this.rectangles = new ArrayList<>();
        this.borders = new ArrayList<>();
    }

    public void addRectangle(int x, int y, int width, int height) {
        this.rectangles.add(new Rectangle(x, y, width, height));
    }

    public void addBorder(int x, int y) {
        this.borders.add(new Point2D(x, y));
    }

    public SaveProvince getProvince() {
        return province;
    }

    public List<Rectangle> getRectangles() {
        return rectangles;
    }

    public List<Point2D> getBorders() {
        return borders;
    }
}
