package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.game.religion.Religion;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class ReligionStringCellFactory implements Callback<ListView<Religion>, ListCell<Religion>> {

    @Override
    public ListCell<Religion> call(ListView<Religion> param) {
        return new ListCell<Religion>() {

            @Override
            protected void updateItem(Religion value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : value.getName());
            }
        };
    }
}
