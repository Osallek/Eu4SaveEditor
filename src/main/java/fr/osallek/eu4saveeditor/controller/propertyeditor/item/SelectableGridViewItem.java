package fr.osallek.eu4saveeditor.controller.propertyeditor.item;

import fr.osallek.eu4saveeditor.controller.control.SelectableGridView;
import java.io.File;
import java.util.Optional;
import java.util.function.Function;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.collections.ObservableSet;

public class SelectableGridViewItem<U> implements CustomItem<U> {

    private final String category;

    private final ObservableList<U> values;

    private final ObservableSet<U> selectedValues;

    private final SelectableGridView<U> selectableGridView;

    private final BooleanProperty editable;

    private Function<U, String> textFunction;

    public SelectableGridViewItem(String category, SelectableGridView<U> selectableGridView) {
        this(category, selectableGridView, new SimpleBooleanProperty(true));
    }

    public SelectableGridViewItem(String category, SelectableGridView<U> selectableGridView, BooleanProperty editable) {
        this.category = category;
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
    public String category() {
        return this.category;
    }

    @Override
    public String name() {
        return null;
    }

    @Override
    public String description() {
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
    public BooleanProperty isEditable() {
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

    public void setCellFactory(Function<U, String> textFunction, Function<U, File> imageFunction, File defaultFile) {
        this.getSelectableGridView().setCellFactory(textFunction, imageFunction, defaultFile);
    }

    public Function<U, String> getCellFactory() {
        return this.textFunction;
    }
}
