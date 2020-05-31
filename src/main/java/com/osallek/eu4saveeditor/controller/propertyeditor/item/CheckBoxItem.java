package com.osallek.eu4saveeditor.controller.propertyeditor.item;

import com.osallek.eu4saveeditor.controller.mapview.SheetCategory;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import java.util.Optional;

public class CheckBoxItem implements CustomItem<Void> {

    private final String category;

    private final String name;

    private boolean value;

    private boolean editable;

    public CheckBoxItem(SheetCategory category, String name, boolean value) {
        this(category, name, value, true);
    }

    public CheckBoxItem(SheetCategory category, String name, boolean value, boolean editable) {
        this.category = category.getForDefaultLocale();
        this.name = name;
        this.value = value;
        this.editable = editable;
    }

    @Override
    public Class<?> getType() {
        return boolean.class;
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
        return this.name;
    }

    @Override
    public Object getValue() {
        return this.value;
    }

    @Override
    public void setValue(Object value) {
        this.value = ((boolean) value);
    }

    @Override
    public ObservableList<Void> getChoices() {
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

    public void setEditable(boolean editable) {
        this.editable = editable;
    }

    public boolean isSelected() {
        return this.value;
    }
}
