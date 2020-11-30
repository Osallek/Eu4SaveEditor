package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.game.ReligiousReform;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class ReligiousReformStringCellFactory implements Callback<ListView<ReligiousReform>, ListCell<ReligiousReform>> {

    @Override
    public ListCell<ReligiousReform> call(ListView<ReligiousReform> param) {
        return new ListCell<>() {

            @Override
            protected void updateItem(ReligiousReform value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : value.getLocalizedName());
            }
        };
    }
}
