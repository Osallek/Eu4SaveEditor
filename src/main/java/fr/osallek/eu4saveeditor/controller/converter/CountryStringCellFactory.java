package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.save.country.SaveCountry;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class CountryStringCellFactory implements Callback<ListView<SaveCountry>, ListCell<SaveCountry>> {

    public static final CountryStringCellFactory INSTANCE = new CountryStringCellFactory();

    @Override
    public ListCell<SaveCountry> call(ListView<SaveCountry> param) {
        return new ListCell<>() {

            @Override
            protected void updateItem(SaveCountry value, boolean empty) {
                super.updateItem(value, empty);

                if (value != null) {
                    setText(CountryStringConverter.INSTANCE.toString(value));
                } else {
                    setText(null);
                }
            }
        };
    }
}
