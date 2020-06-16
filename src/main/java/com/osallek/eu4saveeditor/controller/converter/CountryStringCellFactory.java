package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4saveeditor.controller.EditorController;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class CountryStringCellFactory implements Callback<ListView<Country>, ListCell<Country>> {

    @Override
    public ListCell<Country> call(ListView<Country> param) {
        return new ListCell<Country>() {

            @Override
            protected void updateItem(Country value, boolean empty) {
                super.updateItem(value, empty);

                if (value != null) {
                    setText(EditorController.dummyCountry.equals(value) ? "" : value.getLocalizedName());
                } else {
                    setText(null);
                }
            }
        };
    }
}
