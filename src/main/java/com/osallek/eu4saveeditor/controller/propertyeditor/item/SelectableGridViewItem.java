package com.osallek.eu4saveeditor.controller.propertyeditor.item;

import com.osallek.eu4saveeditor.controller.control.SelectableGridView;
import com.osallek.eu4saveeditor.controller.mapview.SheetCategory;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.collections.ObservableSet;

import java.io.File;
import java.util.Optional;
import java.util.function.Function;

public class SelectableGridViewItem<U> implements CustomItem<U> {

    private final String category;

    private final ObservableList<U> values;

    private ObservableSet<U> selectedValues;

    private final SelectableGridView<U> selectableGridView;

    private final boolean editable;

    private Function<U, String> textFunction;

    public SelectableGridViewItem(SheetCategory category, SelectableGridView<U> selectableGridView) {
        this(category, selectableGridView, true);
    }

    public SelectableGridViewItem(SheetCategory category, SelectableGridView<U> selectableGridView, boolean editable) {
        this.category = category.getForDefaultLocale();
        this.values = selectableGridView.getItems();
        this.selectedValues = selectableGridView.getSelectedItems();
        this.selectableGridView = selectableGridView;
        this.editable = editable;
    }

    @Override
    public Class<?> getType() {
        return SelectableGridViewItem.class;
    }

    @Override
    public String getCategory() {
        return this.category;
    }

    @Override
    public String getName() {
        return null;
    }

    @Override
    public String getDescription() {
        return null;
    }

    @Override
    public Object getValue() {
        return this.selectedValues;
    }

    @Override
    public void setValue(Object value) {
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

    public SelectableGridView<U> getSelectableGridView() {
        return this.selectableGridView;
    }

    public ObservableSet<U> getSelectedValues() {
        return this.selectedValues;
    }

    public int getNbItems() {
        return this.values.size();
    }

    public void select(U u) {
        this.selectableGridView.select(u);
    }

    public void unSelect(U u) {
        this.selectableGridView.unSelect(u);
    }

    public void setCellFactory(Function<U, String> textFunction, Function<U, File> imageFunction) {
        this.getSelectableGridView().setCellFactory(textFunction, imageFunction);
    }

    public Function<U, String> getCellFactory() {
        return this.textFunction;
    }
}
