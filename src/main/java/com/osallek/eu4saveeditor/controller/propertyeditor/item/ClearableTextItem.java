package com.osallek.eu4saveeditor.controller.propertyeditor.item;

import com.osallek.eu4saveeditor.controller.mapview.SheetCategory;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import java.util.Optional;
import java.util.function.Supplier;

public class ClearableTextItem implements CustomItem<Void> {

    private final String category;

    private final String name;

    private String value;

    private final Supplier<String> supplier;

    private final boolean editable;

    public ClearableTextItem(SheetCategory category, String name, String value, Supplier<String> clearSupplier) {
        this(category, name, value, clearSupplier, true);
    }

    public ClearableTextItem(SheetCategory category, String name, String value, Supplier<String> clearSupplier, boolean editable) {
        this.category = category.getForDefaultLocale();
        this.name = name;
        this.value = value;
        this.supplier = clearSupplier;
        this.editable = editable;
    }

    @Override
    public Class<?> getType() {
        return ClearableTextItem.class;
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
        this.value = ((String) value);
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

    public Supplier<String> getSupplier() {
        return supplier;
    }

    public String getText() {
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
