package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.ParliamentBribe;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class ParliamentBribeStringCellFactory implements Callback<ListView<ParliamentBribe>, ListCell<ParliamentBribe>> {

    public static final ParliamentBribeStringCellFactory INSTANCE = new ParliamentBribeStringCellFactory();

    @Override
    public ListCell<ParliamentBribe> call(ListView<ParliamentBribe> param) {
        return new ListCell<ParliamentBribe>() {

            @Override
            protected void updateItem(ParliamentBribe value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : ParliamentBribeStringConverter.INSTANCE.toString(value));
            }
        };
    }
}
