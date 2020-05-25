package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.game.culture.Culture;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class CultureStringCellFactory implements Callback<ListView<Culture>, ListCell<Culture>> {

    @Override
    public ListCell<Culture> call(ListView<Culture> param) {
        return new ListCell<Culture>() {

            @Override
            protected void updateItem(Culture value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : value.getName());
            }
        };
    }
}
