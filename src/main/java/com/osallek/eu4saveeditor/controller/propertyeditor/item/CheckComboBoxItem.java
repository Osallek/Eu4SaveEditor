package com.osallek.eu4saveeditor.controller.propertyeditor.item;

import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;
import javafx.util.StringConverter;
import org.controlsfx.control.CheckComboBox;

import java.util.Optional;

public class CheckComboBoxItem<U> implements CustomItem<U> {

    private final String category;

    private final String name;

    private final ObservableList<U> values;

    private ObservableList<U> selectedValues;

    private final CheckComboBox<U> checkComboBox;

    private final BooleanProperty editable;

    private EventHandler<ActionEvent> onAction;

    private StringConverter<U> converter;

    private Callback<ListView<U>, ListCell<U>> cellFactory;

    public CheckComboBoxItem(String category, String name, ObservableList<U> values, ObservableList<U> selectedValues, CheckComboBox<U> checkComboBox) {
        this(category, name, values, selectedValues, checkComboBox, new SimpleBooleanProperty(true));
    }

    public CheckComboBoxItem(String category, String name, ObservableList<U> values, ObservableList<U> selectedValues, CheckComboBox<U> checkComboBox, BooleanProperty editable) {
        this.category = category;
        this.name = name;
        this.values = values;
        this.selectedValues = selectedValues;
        this.checkComboBox = checkComboBox;
        this.editable = editable;
    }

    @Override
    public Class<?> getType() {
        return CheckComboBoxItem.class;
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
        return this.selectedValues;
    }

    @Override
    public void setValue(Object value) {
        this.selectedValues = ((ObservableList<U>) value);
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

    public CheckComboBox<U> getCheckComboBox() {
        return checkComboBox;
    }

    public ObservableList<U> getSelectedValues() {
        return this.selectedValues;
    }

    public void check(U u) {
        this.checkComboBox.getCheckModel().check(u);
    }

    public void clearCheck(U u) {
        this.checkComboBox.getCheckModel().clearCheck(this.checkComboBox.getCheckModel().getItemIndex(u));
    }

    public EventHandler<ActionEvent> getOnAction() {
        return this.onAction;
    }

    public void setOnAction(EventHandler<ActionEvent> onAction) {
        this.onAction = onAction;
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
