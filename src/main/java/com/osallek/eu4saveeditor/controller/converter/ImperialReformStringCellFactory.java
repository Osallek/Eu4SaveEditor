package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.game.ImperialReform;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class ImperialReformStringCellFactory implements Callback<ListView<ImperialReform>, ListCell<ImperialReform>> {

    @Override
    public ListCell<ImperialReform> call(ListView<ImperialReform> param) {
        return new ListCell<ImperialReform>() {

            @Override
            protected void updateItem(ImperialReform value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : value.getLocalizedName());
            }
        };
    }
}
