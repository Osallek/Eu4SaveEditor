package com.osallek.eu4saveeditor.controller.propertyeditor.item;

import javafx.collections.ObservableList;
import org.controlsfx.control.PropertySheet;

public interface CustomItem<T> extends PropertySheet.Item {

    ObservableList<T> getChoices();

    default int forceValueColSpan() {
        return 1;
    }
}
