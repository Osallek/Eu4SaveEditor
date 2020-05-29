package com.osallek.eu4saveeditor.controller.control;

import javafx.collections.ObservableList;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;
import javafx.util.StringConverter;
import org.controlsfx.control.CheckComboBox;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;

import java.util.List;
import java.util.function.Supplier;

public class ClearableCheckComboBox<U> extends HBox {

    private final CheckComboBox<U> checkComboBox;

    private final Button button;

    public ClearableCheckComboBox(Supplier<List<U>> clearSupplier) {
        this.getStyleClass().add("clearable-check-combo");
        this.checkComboBox = new CheckComboBox<>();
        this.checkComboBox.getStyleClass().add("check-combo-box");
        HBox.setHgrow(this.checkComboBox, Priority.ALWAYS);

        this.button = new Button("", new Glyph("FontAwesome", FontAwesome.Glyph.CLOSE));
        this.button.setAlignment(Pos.TOP_CENTER);
        this.button.setOnMouseReleased(e -> {
            this.clearChecks();
            clearSupplier.get().forEach(this::check);
        });

        Pane centerPane = new Pane();
        HBox.setHgrow(centerPane, Priority.ALWAYS);

        getChildren().add(this.checkComboBox);
        getChildren().add(centerPane);
        getChildren().add(this.button);
    }

    public CheckComboBox<U> getCheckComboBox() {
        return checkComboBox;
    }

    public ObservableList<U> getSelectedValues() {
        return this.checkComboBox.getCheckModel().getCheckedItems();
    }

    public void check(U u) {
        this.checkComboBox.getCheckModel().check(u);
    }

    public void clearCheck(U u) {
        this.checkComboBox.getCheckModel().clearCheck(this.checkComboBox.getCheckModel().getItemIndex(u));
    }

    public void clearChecks() {
        this.checkComboBox.getCheckModel().clearChecks();
    }

    public void setConverter(StringConverter<U> converter) {
        this.checkComboBox.setConverter(converter);
    }

    public StringConverter<U> getConverter() {
        return this.checkComboBox.getConverter();
    }

    public final ObservableList<U> getItems() {
        return this.checkComboBox.getItems();
    }
}
