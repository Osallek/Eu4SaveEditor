package com.osallek.eu4saveeditor.controller.control;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.ObservableSet;
import org.controlsfx.control.GridView;

import java.io.File;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;

public class SelectableGridView<T> extends GridView<T> {

    private ObservableSet<T> selection = FXCollections.observableSet(new HashSet<>());

    private Set<SelectableGridCell<T>> cells = new HashSet<>();

    private boolean anyItemChanged;

    public SelectableGridView(ObservableList<T> items) {
        super();
        setCellHeight(48);
        setCellWidth(48);
        setHorizontalCellSpacing(3);
        setVerticalCellSpacing(5);
        itemsProperty().addListener((observable, oldValue, newValue) ->
                                            setMaxHeight(62 * Math.ceil((double) observable.getValue().size() / 6)));
        setItems(items);
    }

    public SelectableGridView(ObservableList<T> items, ObservableSet<T> selectedItems) {
        this(items);
        this.selection = selectedItems;
    }

    public void setCellFactory(Function<T, String> textFunction, Function<T, File> imageFunction) {
        super.setCellFactory(param -> new SelectableGridCell<>(textFunction, imageFunction));
    }

    public void select(T t) {
        this.selection.forEach(this::unSelect);
        this.selection.add(t);
        this.anyItemChanged = true;
    }

    public boolean isSelected(T t) {
        return this.selection.contains(t);
    }

    public void unSelect(T t) {
        this.selection.remove(t);
        this.anyItemChanged = true;
    }

    public ObservableSet<T> getSelectedItems() {
        return this.selection;
    }

    public boolean anyItemChanged() {
        return anyItemChanged;
    }

    Set<SelectableGridCell<T>> getCells() {
        return cells;
    }
}
