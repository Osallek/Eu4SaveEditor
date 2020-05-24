package com.osallek.eu4saveeditor.controller.propertyeditor.item;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import java.util.Optional;

public class CheckBoxItem implements CustomItem<Void> {

    private final String category;

    private final String name;

    private boolean value;

    private final boolean editable;

    public CheckBoxItem(String category, String name, boolean value) {
        this(category, name, value, true);
    }

    public CheckBoxItem(String category, String name, boolean value, boolean editable) {
        this.category = category;
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
        return null;
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

    public boolean isSelected() {
        return this.value;
    }

    /*    public final ObjectProperty<EventHandler<ActionEvent>> onActionProperty() {
        return this.textField.onActionProperty();
    }

    public final EventHandler<ActionEvent> getOnAction() {
        return onActionProperty().get();
    }

    public final void setOnAction(EventHandler<ActionEvent> value) {
        onActionProperty().set(value);
    }*/
}
