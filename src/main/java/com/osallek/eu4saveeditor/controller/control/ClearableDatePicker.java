package com.osallek.eu4saveeditor.controller.control;

import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.DatePicker;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;

import java.time.LocalDate;
import java.util.function.Supplier;

public class ClearableDatePicker extends HBox {

    private final DatePicker datePicker;
    private final Button button;

    private final Supplier<LocalDate> clearSupplier;

    public ClearableDatePicker(LocalDate date) {
        this(date, null);
    }

    public ClearableDatePicker(LocalDate date, Supplier<LocalDate> clearSupplier) {
        this.datePicker = date == null ? new DatePicker() : new DatePicker(dateToLocalDate(date));
        this.datePicker.setMaxWidth(Double.MAX_VALUE);
        HBox.setHgrow(this.datePicker, Priority.ALWAYS);

        this.button = new Button("", new Glyph("FontAwesome", FontAwesome.Glyph.CLOSE));
        this.button.setAlignment(Pos.TOP_CENTER);

        this.clearSupplier = clearSupplier;
        if (this.clearSupplier != null) {
            this.button.setOnMouseReleased(e -> reset());
        }

        getChildren().add(this.datePicker);
        getChildren().add(this.button);
    }

    public DatePicker getDatePicker() {
        return datePicker;
    }

    public LocalDate getValue() {
        return this.datePicker.getValue();
    }

    public void setValue(LocalDate date) {
        this.datePicker.setValue(dateToLocalDate(date));
    }

    public void setSupplier(Supplier<LocalDate> clearSupplier) {
        if (clearSupplier != null) {
            this.button.setOnMouseReleased(e -> this.setValue(clearSupplier.get()));
        }
    }

    public EventHandler<ActionEvent> getOnAction() {
        return this.datePicker.getOnAction();
    }

    public void setOnAction(EventHandler<ActionEvent> onAction) {
        this.datePicker.setOnAction(onAction);
    }

    public Supplier<LocalDate> getClearSupplier() {
        return clearSupplier;
    }

    public void reset() {
        this.setValue(clearSupplier.get());
    }

    private LocalDate dateToLocalDate(LocalDate date) {
        return date;
    }
}
