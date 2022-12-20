package fr.osallek.eu4saveeditor.controller.control;

import java.io.File;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.ObservableSet;
import org.controlsfx.control.GridView;

public class SelectableGridView<T> extends GridView<T> {

    private ObservableSet<T> selection = FXCollections.observableSet(new HashSet<>());

    private final Set<SelectableGridCell<T>> cells = new HashSet<>();

    private boolean anyItemChanged;

    private int size = 48;

    private boolean multiSelect;

    public SelectableGridView(ObservableList<T> items, boolean multiSelect) {
        this(items, multiSelect, (Integer) null);
    }

    public SelectableGridView(ObservableList<T> items, boolean multiSelect, Integer size) {
        super();
        this.multiSelect = multiSelect;

        if (size != null) {
            this.size = size;
        }

        setCellHeight(this.size);
        setCellWidth(this.size);
        setHorizontalCellSpacing(3);
        setVerticalCellSpacing(5);
        itemsProperty().addListener((observable, oldValue, newValue) -> setMaxHeight(62 * Math.ceil((double) observable.getValue().size() / 6)));
        setItems(items);
    }

    public SelectableGridView(ObservableList<T> items, boolean multiSelect, ObservableSet<T> selectedItems) {
        this(items, multiSelect, selectedItems, null);
    }

    public SelectableGridView(ObservableList<T> items, boolean multiSelect, ObservableSet<T> selectedItems, Integer size) {
        this(items, multiSelect, size);
        this.selection = selectedItems;
    }

    public void setCellFactory(Function<T, String> textFunction, Function<T, File> imageFunction) {
        super.setCellFactory(param -> new SelectableGridCell<>(textFunction, imageFunction, this.size, !this.multiSelect));
    }

    public void select(T t) {
        if (!this.multiSelect) {
            this.selection.forEach(this::unSelect);
        }

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
