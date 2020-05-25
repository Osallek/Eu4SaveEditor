package com.osallek.eu4saveeditor.controller.propertyeditor.item;

import com.osallek.eu4saveeditor.controller.item.ClearableSpinner;
import com.osallek.eu4saveeditor.controller.mapview.SheetCategory;
import javafx.beans.property.ObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import java.util.Optional;
import java.util.function.IntSupplier;

public class ClearableSpinnerItem implements CustomItem<Integer> {

    private final String category;

    private final String name;

    private final ClearableSpinner spinner;

    private final boolean editable;

    public ClearableSpinnerItem(SheetCategory category, String name, int min, int max, int value, IntSupplier supplier) {
        this(category, name, new ClearableSpinner(min, max, value, 1, supplier), true);
    }

    public ClearableSpinnerItem(SheetCategory category, String name, ClearableSpinner spinner) {
        this(category, name, spinner, true);
    }

    public ClearableSpinnerItem(SheetCategory category, String name, ClearableSpinner spinner, boolean editable) {
        this.category = category.getForDefaultLocale();
        this.name = name;
        this.spinner = spinner;
        this.editable = editable;
    }

    @Override
    public Class<?> getType() {
        return ClearableSpinnerItem.class;
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
        return null;
    }

    @Override
    public Object getValue() {
        return this.spinner.getValue();
    }

    @Override
    public void setValue(Object value) {
        this.spinner.setValue((int) value);
    }

    @Override
    public ObservableList<Integer> getChoices() {
        return null;
    }

    @Override
    public Optional<ObservableValue<? extends Object>> getObservableValue() {
        return Optional.empty();
    }

    @Override
    public boolean isEditable() {
        return this.editable;
    }

    public ClearableSpinner getSpinner() {
        return this.spinner;
    }

    public int getIntValue() {
        return this.spinner.getValue();
    }

    public ObjectProperty<Integer> valueProperty() {
        return this.spinner.getSpinner().getValueFactory().valueProperty();
    }
}
