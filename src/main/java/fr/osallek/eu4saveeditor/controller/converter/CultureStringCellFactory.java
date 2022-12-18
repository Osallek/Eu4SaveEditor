package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.Culture;
import fr.osallek.eu4parser.model.game.Game;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class CultureStringCellFactory implements Callback<ListView<Culture>, ListCell<Culture>> {

    private final Game game;

    public CultureStringCellFactory(Game game) {
        this.game = game;
    }

    @Override
    public ListCell<Culture> call(ListView<Culture> param) {
        return new ListCell<Culture>() {

            @Override
            protected void updateItem(Culture value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : Eu4SaveEditorUtils.localize(value.getName(), game));
            }
        };
    }
}
