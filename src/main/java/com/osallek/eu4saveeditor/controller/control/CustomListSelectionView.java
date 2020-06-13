package com.osallek.eu4saveeditor.controller.control;

import org.controlsfx.control.ListSelectionView;

import java.util.Collection;
import java.util.function.Supplier;

public abstract class CustomListSelectionView<S> extends ListSelectionView<S> {

    public abstract void onReset(Supplier<Collection<S>> sourceSupplier, Supplier<Collection<S>> targetSupplier);
}
