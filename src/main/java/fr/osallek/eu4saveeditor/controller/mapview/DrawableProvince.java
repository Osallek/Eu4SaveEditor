package fr.osallek.eu4saveeditor.controller.mapview;

import fr.osallek.eu4parser.model.save.province.SaveProvince;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import javafx.geometry.Point2D;
import javafx.geometry.Rectangle2D;

public class DrawableProvince {

    private final SaveProvince province;

    private List<Rectangle2D> rectangles;

    private Set<Point2D> borders;

    public DrawableProvince(SaveProvince province) {
        this.province = province;
    }

    public void addRectangle(int x, int y, int width, int height) {
        if (width > 0 && height > 0) {
            if (this.rectangles == null) {
                this.rectangles = new ArrayList<>();
            }

            this.rectangles.add(new Rectangle2D(x, y, width, height));
        }
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

    public List<Rectangle2D> getRectangles() {
        return this.rectangles == null ? new ArrayList<>() : this.rectangles;
    }

    public Set<Point2D> getBorders() {
        return this.borders == null ? new HashSet<>() : this.borders;
    }
}
