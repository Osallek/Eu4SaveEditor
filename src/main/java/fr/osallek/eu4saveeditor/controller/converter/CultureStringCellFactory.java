package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.Culture;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class CultureStringCellFactory implements Callback<ListView<Culture>, ListCell<Culture>> {

    public static final CultureStringCellFactory INSTANCE = new CultureStringCellFactory();

    @Override
    public ListCell<Culture> call(ListView<Culture> param) {
        return new ListCell<>() {

            @Override
            protected void updateItem(Culture value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : CultureStringConverter.INSTANCE.toString(value));
            }
        };
    }
}
