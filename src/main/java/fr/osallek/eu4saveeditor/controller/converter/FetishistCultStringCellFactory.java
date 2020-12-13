package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.FetishistCult;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class FetishistCultStringCellFactory implements Callback<ListView<FetishistCult>, ListCell<FetishistCult>> {

    @Override
    public ListCell<FetishistCult> call(ListView<FetishistCult> param) {
        return new ListCell<>() {

            @Override
            protected void updateItem(FetishistCult value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : value.getLocalizedName());
            }
        };
    }
}
