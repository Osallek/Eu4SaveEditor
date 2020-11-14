package com.osallek.eu4saveeditor.controller.pane;

import javafx.geometry.Pos;
import javafx.scene.control.ContentDisplay;
import javafx.scene.control.DateCell;
import javafx.scene.control.DatePicker;
import javafx.scene.control.TableCell;
import javafx.util.converter.LocalDateStringConverter;

import java.time.LocalDate;
import java.time.chrono.Chronology;
import java.time.format.FormatStyle;
import java.util.Locale;

public class DatePickerCell<S> extends TableCell<S, LocalDate> {

    private final LocalDate startDate;

    private final LocalDate endDate;

    private DatePicker datePicker;

    public DatePickerCell() {
        this(null, null);
    }

    public DatePickerCell(LocalDate startDate, LocalDate endDate) {
        this.startDate = startDate;
        this.endDate = endDate;
    }

    @Override
    public void updateItem(LocalDate item, boolean empty) {
        super.updateItem(item, empty);

        if (empty) {
            setText(null);
            setGraphic(null);
        } else {
            if (isEditing()) {
                if (this.datePicker != null) {
                    this.datePicker.setValue(getItem());
                }

                setText(null);
                setGraphic(this.datePicker);
                setContentDisplay(ContentDisplay.GRAPHIC_ONLY);
            } else {
                if (this.datePicker == null) {
                    setText(new LocalDateStringConverter(FormatStyle.SHORT, null, Chronology.ofLocale(Locale.getDefault(Locale.Category.FORMAT)))
                                    .toString(item));
                } else {
                    setText(this.datePicker.getConverter().toString(item));
                }

                setGraphic(null);
                setContentDisplay(ContentDisplay.TEXT_ONLY);
            }
        }
    }

    private void createDatePicker() {
        this.datePicker = new DatePicker(getItem());
        this.datePicker.setEditable(true);
        this.datePicker.setShowWeekNumbers(false);
        this.datePicker.setOnAction(t -> {
            if ((endDate != null && this.datePicker.getValue().compareTo(endDate) <= 0)
                && (startDate != null && this.datePicker.getValue().compareTo(startDate) >= 0)) {
                commitEdit(this.datePicker.getValue());
            } else {
                this.datePicker.setValue(getItem());
            }
        });
        this.datePicker.setDayCellFactory(d -> new DateCell() {
            @Override
            public void updateItem(LocalDate item, boolean empty) {
                super.updateItem(item, empty);
                setDisable((endDate != null && item.isAfter(endDate)) || (startDate != null && item.isBefore(startDate)));
            }
        });
        setAlignment(Pos.CENTER);
    }

    @Override
    public void startEdit() {
        if (!isEmpty()) {
            super.startEdit();
            createDatePicker();
            setText(this.datePicker.getConverter().toString(getItem()));
            setGraphic(this.datePicker);
            setContentDisplay(ContentDisplay.GRAPHIC_ONLY);
        }
    }

    @Override
    public void cancelEdit() {
        super.cancelEdit();
        setContentDisplay(ContentDisplay.TEXT_ONLY);
        setText(this.datePicker.getConverter().toString(getItem()));
        setGraphic(null);
    }

    public DatePicker getDatePicker() {
        return datePicker;
    }

    public void setDatePicker(DatePicker datePicker) {
        this.datePicker = datePicker;
    }
}
