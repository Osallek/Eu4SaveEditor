package com.osallek.eu4saveeditor.controller.control;

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

import java.util.function.Supplier;

public abstract class ClearableSpinner<T> extends HBox {

    protected Spinner<T> spinner;

    protected Button button;

    public Spinner<T> getSpinner() {
        return spinner;
    }

    public T getValue() {
        return this.spinner.getValue();
    }

    public void setValue(T value) {
        this.spinner.getValueFactory().setValue(value);
    }

    public final ObjectProperty<SpinnerValueFactory<T>> valueFactoryProperty() {
        return this.spinner.valueFactoryProperty();
    }

    protected void fill(Supplier<T> clearSupplier) {
        this.button = new Button("", new Glyph("FontAwesome", FontAwesome.Glyph.CLOSE));
        this.button.setAlignment(Pos.TOP_CENTER);
        this.button.setOnMouseReleased(e -> this.setValue(clearSupplier.get()));

        Pane centerPane = new Pane();
        HBox.setHgrow(centerPane, Priority.ALWAYS);

        getChildren().add(this.spinner);
        getChildren().add(centerPane);
        getChildren().add(this.button);
    }
}
