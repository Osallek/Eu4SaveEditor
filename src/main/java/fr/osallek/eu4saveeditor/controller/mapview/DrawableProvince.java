package fr.osallek.eu4saveeditor.controller.mapview;

import fr.osallek.eu4parser.model.save.province.SaveProvince;
import com.sun.javafx.geom.Rectangle;
import javafx.geometry.Point2D;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class DrawableProvince {

    private final SaveProvince province;

    private List<Rectangle> rectangles;

    private Set<Point2D> borders;

    public DrawableProvince(SaveProvince province) {
        this.province = province;
    }

    public void addRectangle(int x, int y, int width, int height) {
        if (this.rectangles == null) {
            this.rectangles = new ArrayList<>();
        }

        this.rectangles.add(new Rectangle(x, y, width, height));
    }

    public void addBorder(int x, int y) {
        if (this.borders == null) {
            this.borders = new HashSet<>();
        }

        this.borders.add(new Point2D(x, y));
    }

    public SaveProvince getProvince() {
        return province;
    }

    public List<Rectangle> getRectangles() {
        return this.rectangles == null ? new ArrayList<>() : this.rectangles;
    }

    public Set<Point2D> getBorders() {
        return this.borders == null ? new HashSet<>() : this.borders;
    }
}
