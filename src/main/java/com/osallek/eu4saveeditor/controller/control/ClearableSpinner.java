package com.osallek.eu4saveeditor.controller.control;

import javafx.beans.property.ObjectProperty;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Control;
import javafx.scene.control.Spinner;
import javafx.scene.control.SpinnerValueFactory;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;

import java.util.function.Supplier;

public abstract class ClearableSpinner<T> extends HBox {

    protected Spinner<T> spinner;

    protected Button button;

    public ClearableSpinner() {
        super(5);
    }

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

    public void setSupplier(Supplier<T> clearSupplier) {
        if (clearSupplier != null) {
            this.button.setOnMouseReleased(e -> this.setValue(clearSupplier.get()));
        }
    }

    protected void fill(Supplier<T> clearSupplier) {
        fill(clearSupplier, null);
    }

    protected void fill(Supplier<T> clearSupplier, Node centerNode) {
        this.button = new Button("", new Glyph("FontAwesome", FontAwesome.Glyph.CLOSE));
        this.button.setAlignment(Pos.TOP_CENTER);
        setSupplier(clearSupplier);

        if (centerNode == null) {
            centerNode = new Pane();
        }

        if (!(centerNode instanceof Pane) && !(centerNode instanceof Control)) {
            GridPane pane = new GridPane();
            pane.getChildren().add(centerNode);
            pane.setAlignment(Pos.CENTER);
            centerNode = pane;
        }

        centerNode.maxWidth(Double.MAX_VALUE);
        HBox.setHgrow(centerNode, Priority.ALWAYS);

        getChildren().add(this.spinner);
        getChildren().add(centerNode);
        getChildren().add(this.button);
    }
}
