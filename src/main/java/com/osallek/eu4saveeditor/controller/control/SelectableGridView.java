package com.osallek.eu4saveeditor.controller.control;

import com.sun.javafx.collections.ObservableSetWrapper;
import impl.org.controlsfx.skin.GridViewSkin;
import javafx.collections.ObservableList;
import javafx.collections.ObservableSet;
import javafx.scene.control.Cell;
import org.controlsfx.control.GridView;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

public class SelectableGridView<T> extends GridView<T> {

    private ObservableSet<T> selection = new ObservableSetWrapper<>(new HashSet<>());

    private Set<SelectableGridCell<T>> cells = new HashSet<>();

    private boolean anyItemChanged;

    public SelectableGridView(ObservableList<T> items) {
        super();
        setCellHeight(50);
        setCellWidth(50);
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

    public void setCellFactory(Function<T, String> textFunction) {
        super.setCellFactory(param -> new SelectableGridCell<>(textFunction));
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
