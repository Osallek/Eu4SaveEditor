package com.osallek.eu4saveeditor.controller.converter;

import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;
import org.apache.commons.lang3.tuple.Pair;

public class PairCellFactory implements Callback<ListView<Pair<String, String>>, ListCell<Pair<String, String>>> {

    @Override
    public ListCell<Pair<String, String>> call(ListView<Pair<String, String>> param) {
        return new ListCell<>() {

            @Override
            protected void updateItem(Pair<String, String> value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : value.getValue());
            }
        };
    }
}
