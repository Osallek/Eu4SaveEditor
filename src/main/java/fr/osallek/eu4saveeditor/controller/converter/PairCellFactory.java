package fr.osallek.eu4saveeditor.controller.converter;

import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;
import org.apache.commons.lang3.tuple.Pair;

public class PairCellFactory implements Callback<ListView<Pair<Integer, String>>, ListCell<Pair<Integer, String>>> {

    @Override
    public ListCell<Pair<Integer, String>> call(ListView<Pair<Integer, String>> param) {
        return new ListCell<>() {

            @Override
            protected void updateItem(Pair<Integer, String> value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : value.getValue());
            }
        };
    }
}
