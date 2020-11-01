package com.osallek.eu4saveeditor.controller.propertyeditor.item;

import com.osallek.eu4saveeditor.controller.control.ClearableDatePicker;
import com.osallek.eu4saveeditor.i18n.SheetCategory;
import javafx.beans.value.ObservableValue;
import org.controlsfx.control.PropertySheet;

import java.time.LocalDate;
import java.util.Optional;

public class ClearableDatePickerItem implements PropertySheet.Item {

    private final String category;

    private final String name;

    private final String description;

    private final ClearableDatePicker datePicker;

    private boolean editable;

    public ClearableDatePickerItem(SheetCategory category, String name, ClearableDatePicker datePicker) {
        this(category.getForDefaultLocale(), name, datePicker);
    }

    public ClearableDatePickerItem(String category, String name, ClearableDatePicker datePicker) {
        this(category, name, null, datePicker, true);
    }

    public ClearableDatePickerItem(String category, String name, String description, ClearableDatePicker datePicker, boolean editable) {
        this.category = category;
        this.name = name;
        this.description = description;
        this.datePicker = datePicker;
        this.editable = editable;
    }

    @Override
    public Class<?> getType() {
        return ClearableDatePickerItem.class;
    }

    @Override
    public String getCategory() {
        return this.category;
    }

    @Override
    public String getName() {
        return this.name;
    }

    @Override
    public String getDescription() {
        return this.description;
    }

    @Override
    public Object getValue() {
        return this.datePicker.getValue();
    }

    @Override
    public void setValue(Object value) {
        this.datePicker.setValue((LocalDate) value);
    }

    @Override
    public Optional<ObservableValue<? extends Object>> getObservableValue() {
        return Optional.empty();
    }

    @Override
    public boolean isEditable() {
        return this.editable;
    }

    public void setEditable(boolean editable) {
        this.editable = editable;
    }

    public ClearableDatePicker getDatePicker() {
        return this.datePicker;
    }

    public LocalDate getTrueValue() {
        return this.datePicker.getValue();
    }

    public void setValue(LocalDate date) {
        this.datePicker.setValue(date);
    }
}
