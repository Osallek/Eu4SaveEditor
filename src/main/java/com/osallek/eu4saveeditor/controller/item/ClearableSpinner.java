package com.osallek.eu4saveeditor.controller.item;

import javafx.beans.property.ObjectProperty;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Spinner;
import javafx.scene.control.SpinnerValueFactory;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;

import java.util.function.IntSupplier;

public class ClearableSpinner extends HBox {

    private final Spinner<Integer> spinner;

    private final Button button;

    public ClearableSpinner(int min, int max, int value, int step, IntSupplier clearSupplier) {
        this.spinner = new Spinner<>(min, max, value, step);
        HBox.setHgrow(this.spinner, Priority.ALWAYS);

        this.button = new Button("", new Glyph("FontAwesome", FontAwesome.Glyph.CLOSE));
        this.button.setAlignment(Pos.TOP_CENTER);
        this.button.setOnMouseReleased(e -> this.setValue(clearSupplier.getAsInt()));

        Pane centerPane = new Pane();
        HBox.setHgrow(centerPane, Priority.ALWAYS);

        getChildren().add(this.spinner);
        getChildren().add(centerPane);
        getChildren().add(this.button);
    }

    public Spinner<Integer> getSpinner() {
        return spinner;
    }

    public int getValue() {
        return this.spinner.getValue();
    }

    public void setValue(int value) {
        this.spinner.getValueFactory().setValue(value);
    }

    public final ObjectProperty<SpinnerValueFactory<Integer>> valueFactoryProperty() {
        return this.spinner.valueFactoryProperty();
    }
}
