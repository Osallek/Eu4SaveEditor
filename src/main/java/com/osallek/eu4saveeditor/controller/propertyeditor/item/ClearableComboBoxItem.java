package com.osallek.eu4saveeditor.controller.propertyeditor.item;

import com.osallek.eu4saveeditor.controller.item.ClearableComboBox;
import com.osallek.eu4saveeditor.controller.mapview.SheetCategory;
import javafx.beans.property.ObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;
import javafx.util.StringConverter;

import java.util.Optional;

public class ClearableComboBoxItem<U> implements CustomItem<U> {

    private final String category;

    private final String name;

    private final ObservableList<U> values;

    private U value;

    private final ClearableComboBox<U> comboBox;

    private final boolean editable;

    private EventHandler<ActionEvent> onAction;

    private StringConverter<U> converter;

    private Callback<ListView<U>, ListCell<U>> cellFactory;

    public ClearableComboBoxItem(SheetCategory category, String name, ObservableList<U> values, U value, ClearableComboBox<U> comboBox) {
        this(category, name, values, value, comboBox, true);
    }

    public ClearableComboBoxItem(SheetCategory category, String name, ObservableList<U> values, U value, ClearableComboBox<U> comboBox, boolean editable) {
        this.category = category.getForDefaultLocale();
        this.name = name;
        this.values = values;
        this.value = value;
        this.comboBox = comboBox;
        this.editable = editable;
    }

    @Override
    public Class<?> getType() {
        return ClearableComboBoxItem.class;
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
        this.value = ((U) value);
    }

    @Override
    public ObservableList<U> getChoices() {
        return this.values;
    }

    @Override
    public Optional<ObservableValue<? extends Object>> getObservableValue() {
        return Optional.empty();
    }

    @Override
    public boolean isEditable() {
        return this.editable;
    }

    public ClearableComboBox<U> getComboBox() {
        return this.comboBox;
    }

    public U getSelectedValue() {
        return this.value;
    }

    public void select(U u) {
        this.comboBox.getComboBox().getSelectionModel().select(u);
    }

    public EventHandler<ActionEvent> getOnAction() {
        return this.onAction;
    }

    public void setOnAction(EventHandler<ActionEvent> onAction) {
        this.onAction = onAction;
    }

    public ObjectProperty<U> valueProperty() {
        return this.comboBox.getComboBox().valueProperty();
    }

    public void setConverter(StringConverter<U> converter) {
        this.converter = converter;
    }

    public StringConverter<U> getConverter() {
        return this.converter;
    }

    public void setCellFactory(Callback<ListView<U>, ListCell<U>> cellFactory) {
        this.cellFactory = cellFactory;
    }

    public Callback<ListView<U>, ListCell<U>> getCellFactory() {
        return this.cellFactory;
    }
}
