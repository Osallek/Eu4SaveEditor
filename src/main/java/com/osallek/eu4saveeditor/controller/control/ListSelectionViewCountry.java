package com.osallek.eu4saveeditor.controller.control;

import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4saveeditor.controller.converter.CountryStringCellFactory;
import javafx.collections.ObservableList;

import java.util.Collection;
import java.util.function.Supplier;

public class ListSelectionViewCountry extends CustomListSelectionView<Country> {

    public ListSelectionViewCountry(ObservableList<Country> choices, ObservableList<Country> selected) {
        setCellFactory(new CountryStringCellFactory());
        setPrefWidth(750);
        setPrefHeight(600);
        setSourceItems(choices);
        setTargetItems(selected);
    }

    @Override
    public void onReset(Supplier<Collection<Country>> sourceSupplier, Supplier<Collection<Country>> targetSupplier) {
        getTargetItems().setAll(targetSupplier.get());
        getSourceItems().setAll(sourceSupplier.get());
    }
}
