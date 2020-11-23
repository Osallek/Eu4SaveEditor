package com.osallek.eu4saveeditor.controller.propertyeditor.item;

import com.osallek.eu4saveeditor.controller.control.ClearableDatePicker;
import com.osallek.eu4saveeditor.i18n.SheetCategory;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import java.time.LocalDate;
import java.util.Optional;
import java.util.function.Supplier;

public class ClearableDatePickerItem implements CustomItem<LocalDate> {

    private final String category;

    private final String name;

    private final String description;

    private final ClearableDatePicker datePicker;

    private final BooleanProperty editable;

    public ClearableDatePickerItem(SheetCategory category, String name, LocalDate date, Supplier<LocalDate> clearSupplier) {
        this(category.getForDefaultLocale(), name, date, clearSupplier);
    }

    public ClearableDatePickerItem(String category, String name, LocalDate date, Supplier<LocalDate> clearSupplier) {
        this(category, name, null, date, clearSupplier, null, null, new SimpleBooleanProperty(true));
    }

    public ClearableDatePickerItem(String category, String name, LocalDate date, Supplier<LocalDate> clearSupplier, LocalDate startDate, LocalDate endDate) {
        this(category, name, null, date, clearSupplier, startDate, endDate, new SimpleBooleanProperty(true));
    }

    public ClearableDatePickerItem(String category, String name, String description, LocalDate date, Supplier<LocalDate> clearSupplier, LocalDate startDate,
            LocalDate endDate, BooleanProperty editable) {
        this.category = category;
        this.name = name;
        this.description = description;
        this.datePicker = new ClearableDatePicker(date, clearSupplier, startDate, endDate);
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
    public ObservableList<LocalDate> getChoices() {
        return null;
    }

    @Override
    public BooleanProperty isEditable() {
        return this.editable;
    }

    public void setEditable(boolean editable) {
        this.editable.set(editable);
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
