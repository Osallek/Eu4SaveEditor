package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.game.Decree;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class DecreeStringCellFactory implements Callback<ListView<Decree>, ListCell<Decree>> {

    @Override
    public ListCell<Decree> call(ListView<Decree> param) {
        return new ListCell<Decree>() {

            @Override
            protected void updateItem(Decree value, boolean empty) {
                super.updateItem(value, empty);
                if (empty) {
                    setText(null);
                } else {
                    setText(value == null ? null : value.getLocalizedName());
                }
            }
        };
    }
}
