package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.Game;
import fr.osallek.eu4parser.model.game.Religion;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class ReligionStringCellFactory implements Callback<ListView<Religion>, ListCell<Religion>> {

    private final Game game;

    public ReligionStringCellFactory(Game game) {
        this.game = game;
    }

    @Override
    public ListCell<Religion> call(ListView<Religion> param) {
        return new ListCell<>() {

            @Override
            protected void updateItem(Religion value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : Eu4SaveEditorUtils.localize(value.getName(), game));
            }
        };
    }
}
