package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.game.ReligionGroup;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class ReligionGroupStringCellFactory implements Callback<ListView<ReligionGroup>, ListCell<ReligionGroup>> {

    @Override
    public ListCell<ReligionGroup> call(ListView<ReligionGroup> param) {
        return new ListCell<>() {

            @Override
            protected void updateItem(ReligionGroup value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : value.getLocalizedName());
            }
        };
    }
}
