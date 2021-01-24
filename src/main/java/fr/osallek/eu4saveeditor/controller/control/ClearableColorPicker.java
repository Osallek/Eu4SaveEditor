package fr.osallek.eu4saveeditor.controller.control;

import javafx.beans.property.ObjectProperty;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.ColorPicker;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.paint.Color;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;

import java.util.function.Supplier;

public class ClearableColorPicker extends HBox {

    private final ColorPicker colorPicker;

    private final Button button;

    private final Supplier<Color> clearSupplier;

    public ClearableColorPicker(ColorPicker colorPicker) {
        this(colorPicker, null);
    }

    public ClearableColorPicker(ColorPicker colorPicker, Supplier<Color> clearSupplier) {
        this.colorPicker = colorPicker;
        this.colorPicker.setMaxWidth(Double.MAX_VALUE);
        HBox.setHgrow(this.colorPicker, Priority.ALWAYS);

        this.button = new Button("", new Glyph("FontAwesome", FontAwesome.Glyph.CLOSE));
        this.button.setAlignment(Pos.TOP_CENTER);

        this.clearSupplier = clearSupplier;
        if (this.clearSupplier != null) {
            this.button.setOnMouseReleased(e -> reset());
        }

        getChildren().add(this.colorPicker);
        getChildren().add(this.button);
    }

    public ColorPicker getColorPicker() {
        return colorPicker;
    }

    public Color getSelectedValue() {
        return this.colorPicker.getValue();
    }

    public void select(Color u) {
        this.colorPicker.setValue(u);
    }

    public void setSupplier(Supplier<Color> clearSupplier) {
        if (clearSupplier != null) {
            this.button.setOnMouseReleased(e -> this.select(clearSupplier.get()));
        }
    }

    public EventHandler<ActionEvent> getOnAction() {
        return this.colorPicker.getOnAction();
    }

    public void setOnAction(EventHandler<ActionEvent> onAction) {
        this.colorPicker.setOnAction(onAction);
    }

    public ObjectProperty<Color> valueProperty() {
        return this.colorPicker.valueProperty();
    }

    public Supplier<Color> getClearSupplier() {
        return clearSupplier;
    }

    public void reset() {
        this.select(clearSupplier.get());
    }
}
