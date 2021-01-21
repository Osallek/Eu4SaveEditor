package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.TradeGood;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class TradeGoodStringCellFactory implements Callback<ListView<TradeGood>, ListCell<TradeGood>> {

    @Override
    public ListCell<TradeGood> call(ListView<TradeGood> param) {
        return new ListCell<TradeGood>() {

            @Override
            protected void updateItem(TradeGood value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : value.getLocalizedName());
            }
        };
    }
}
