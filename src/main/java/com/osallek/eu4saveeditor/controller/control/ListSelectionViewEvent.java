package com.osallek.eu4saveeditor.controller.control;

import com.osallek.eu4parser.model.game.Event;
import com.osallek.eu4saveeditor.controller.converter.EventStringCellFactory;
import javafx.collections.ObservableList;

import java.util.Collection;
import java.util.function.Supplier;

public class ListSelectionViewEvent extends CustomListSelectionView<Event> {

    public ListSelectionViewEvent(ObservableList<Event> choices, ObservableList<Event> selected) {
        setCellFactory(new EventStringCellFactory());
        setPrefWidth(1400);
        setPrefHeight(600);
        setSourceItems(choices);
        setTargetItems(selected);
    }

    @Override
    public void onReset(Supplier<Collection<Event>> sourceSupplier, Supplier<Collection<Event>> targetSupplier) {
        getTargetItems().setAll(targetSupplier.get());
        getSourceItems().setAll(sourceSupplier.get());
    }
}
