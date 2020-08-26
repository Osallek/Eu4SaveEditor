package com.osallek.eu4saveeditor.controller.propertyeditor.item;

import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;
import javafx.util.StringConverter;

import java.util.Optional;

public class ComboBoxItem<U> implements CustomItem<U> {

    private final String category;

    private final String name;

    private final ObservableList<U> values;

    private U value;

    private final ComboBox<U> comboBox;

    private final BooleanProperty editable;

    private EventHandler<ActionEvent> onAction;

    private StringConverter<U> converter;

    private Callback<ListView<U>, ListCell<U>> cellFactory;

    public ComboBoxItem(String category, String name, ObservableList<U> values, U value, ComboBox<U> comboBox) {
        this(category, name, values, value, comboBox, new SimpleBooleanProperty(true));
    }

    public ComboBoxItem(String category, String name, ObservableList<U> values, U value, ComboBox<U> comboBox, BooleanProperty editable) {
        this.category = category;
        this.name = name;
        this.values = values;
        this.value = value;
        this.comboBox = comboBox;
        this.editable = editable;
    }

    @Override
    public Class<?> getType() {
        return ComboBoxItem.class;
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
    public BooleanProperty isEditable() {
        return this.editable;
    }

    public ComboBox<U> getComboBox() {
        return comboBox;
    }

    public U getSelectedValue() {
        return this.value;
    }

    public void select(U u) {
        this.comboBox.getSelectionModel().select(u);
    }

    public EventHandler<ActionEvent> getOnAction() {
        return this.onAction;
    }

    public void setOnAction(EventHandler<ActionEvent> onAction) {
        this.onAction = onAction;
    }

    public ObjectProperty<U> valueProperty() {
        return this.comboBox.valueProperty();
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
