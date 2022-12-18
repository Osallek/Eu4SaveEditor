package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.Game;
import fr.osallek.eu4parser.model.game.ReligiousReform;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class ReligiousReformStringCellFactory implements Callback<ListView<ReligiousReform>, ListCell<ReligiousReform>> {

    private final Game game;

    public ReligiousReformStringCellFactory(Game game) {
        this.game = game;
    }

    @Override
    public ListCell<ReligiousReform> call(ListView<ReligiousReform> param) {
        return new ListCell<>() {

            @Override
            protected void updateItem(ReligiousReform value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : Eu4SaveEditorUtils.localize(value.getName(), game));
            }
        };
    }
}
