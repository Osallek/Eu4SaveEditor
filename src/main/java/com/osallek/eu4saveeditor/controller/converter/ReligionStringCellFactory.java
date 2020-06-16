package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.save.SaveReligion;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class ReligionStringCellFactory implements Callback<ListView<SaveReligion>, ListCell<SaveReligion>> {

    @Override
    public ListCell<SaveReligion> call(ListView<SaveReligion> param) {
        return new ListCell<SaveReligion>() {

            @Override
            protected void updateItem(SaveReligion value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : value.getLocalizedName());
            }
        };
    }
}
