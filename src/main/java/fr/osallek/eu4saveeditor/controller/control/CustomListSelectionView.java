package fr.osallek.eu4saveeditor.controller.control;

import javafx.collections.ObservableList;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;
import org.controlsfx.control.ListSelectionView;

import java.util.Collection;
import java.util.function.Supplier;

public class CustomListSelectionView<T> extends ListSelectionView<T> {

    public CustomListSelectionView(ObservableList<T> choices, ObservableList<T> selected, Callback<ListView<T>, ListCell<T>> cellFactory, int width, int height) {
        setCellFactory(cellFactory);
        setPrefWidth(width);
        setPrefHeight(height);
        setSourceItems(choices);
        setTargetItems(selected);

        this.getActions().setAll(new MoveToTarget(), new MoveToTargetAll(), new MoveToSource(), new MoveToSourceAll());
    }

    public void onReset(Supplier<Collection<T>> sourceSupplier, Supplier<Collection<T>> targetSupplier) {
        getTargetItems().setAll(targetSupplier.get());
        getSourceItems().setAll(sourceSupplier.get());
    }
}
