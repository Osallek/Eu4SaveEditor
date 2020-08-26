package com.osallek.eu4saveeditor.controller.propertyeditor.item;

import com.osallek.eu4saveeditor.controller.pane.CustomPropertySheet;
import javafx.collections.ObservableList;

public interface CustomItem<T> extends CustomPropertySheet.Item {

    ObservableList<T> getChoices();

    default int forceValueColSpan() {
        return 1;
    }
}
