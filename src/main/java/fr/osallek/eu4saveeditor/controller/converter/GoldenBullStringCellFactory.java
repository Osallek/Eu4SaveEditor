package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.GoldenBull;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class GoldenBullStringCellFactory implements Callback<ListView<GoldenBull>, ListCell<GoldenBull>> {

    @Override
    public ListCell<GoldenBull> call(ListView<GoldenBull> param) {
        return new ListCell<GoldenBull>() {

            @Override
            protected void updateItem(GoldenBull value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : value.getLocalizedName());
            }
        };
    }
}
