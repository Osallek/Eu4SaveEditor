package com.osallek.eu4saveeditor.controller.item;

import javafx.beans.binding.Bindings;
import javafx.beans.value.ObservableValue;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.Slider;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;

import java.util.function.DoubleSupplier;

public class ClearableSlider extends HBox {

    private final Slider slider;

    private final Label label;

    private final Button button;

    public ClearableSlider(double min, double max, double value, DoubleSupplier clearSupplier) {
        this.slider = new Slider(min, max, value);
        HBox.setHgrow(this.slider, Priority.ALWAYS);

        this.label = new Label(Double.toString(this.slider.getValue()));
        this.label.textProperty().bind(Bindings.format("%.3f", this.slider.valueProperty()));
        this.label.setMinWidth(50);
        HBox.setHgrow(this.label, Priority.ALWAYS);

        this.button = new Button("", new Glyph("FontAwesome", FontAwesome.Glyph.CLOSE));
        this.button.setAlignment(Pos.TOP_CENTER);
        this.button.setOnMouseReleased(e -> this.setValue(clearSupplier.getAsDouble()));

        this.setAlignment(Pos.CENTER_LEFT);
        this.setSpacing(5);
        getChildren().add(this.slider);
        getChildren().add(this.label);
        getChildren().add(this.button);
    }

    public Slider getSlider() {
        return slider;
    }

    public double getValue() {
        return this.slider.getValue();
    }

    public void setValue(double value) {
        this.slider.setValue(value);
    }

    public ObservableValue<Double> getObservableValue() {
        return this.slider.valueProperty().asObject();
    }
}
