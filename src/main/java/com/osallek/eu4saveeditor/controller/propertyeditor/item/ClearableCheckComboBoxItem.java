package com.osallek.eu4saveeditor.controller.propertyeditor.item;

import com.osallek.eu4saveeditor.controller.control.ClearableCheckComboBox;
import com.osallek.eu4saveeditor.controller.mapview.SheetCategory;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;
import javafx.util.StringConverter;

import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;

public class ClearableCheckComboBoxItem<U> implements CustomItem<U> {

    private final String category;

    private final String name;

    private final ObservableList<U> values;

    private ObservableList<U> selectedValues;

    private final ClearableCheckComboBox<U> checkComboBox;

    private final boolean editable;

    private EventHandler<ActionEvent> onAction;

    private StringConverter<U> converter;

    private Callback<ListView<U>, ListCell<U>> cellFactory;

    public ClearableCheckComboBoxItem(SheetCategory category, String name, ObservableList<U> values, ClearableCheckComboBox<U> checkComboBox) {
        this(category, name, values, null, checkComboBox, true);
    }

    public ClearableCheckComboBoxItem(SheetCategory category, String name, ObservableList<U> values, ObservableList<U> selectedValues, ClearableCheckComboBox<U> checkComboBox) {
        this(category, name, values, selectedValues, checkComboBox, true);
    }

    public ClearableCheckComboBoxItem(SheetCategory category, String name, ObservableList<U> values, ObservableList<U> selectedValues, ClearableCheckComboBox<U> checkComboBox, boolean editable) {
        this.category = category.getForDefaultLocale();
        this.name = name;
        this.values = values;
        this.selectedValues = selectedValues;
        this.checkComboBox = checkComboBox;
        this.editable = editable;
    }

    @Override
    public Class<?> getType() {
        return ClearableCheckComboBoxItem.class;
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
    public boolean isEditable() {
        return this.editable;
    }

    public ClearableCheckComboBox<U> getCheckComboBox() {
        return this.checkComboBox;
    }

    public ObservableList<U> getSelectedValues() {
        return this.selectedValues;
    }

    public void check(U u) {
        this.checkComboBox.check(u);
    }

    public void clearCheck(U u) {
        this.checkComboBox.clearCheck(u);
    }

    public void setSupplier(Supplier<List<U>> clearSupplier) {
        if (clearSupplier != null) {
            this.checkComboBox.setSupplier(clearSupplier);
        }
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
