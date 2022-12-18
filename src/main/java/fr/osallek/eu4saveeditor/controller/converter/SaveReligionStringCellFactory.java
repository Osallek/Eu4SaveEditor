package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.Game;
import fr.osallek.eu4parser.model.save.SaveReligion;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class SaveReligionStringCellFactory implements Callback<ListView<SaveReligion>, ListCell<SaveReligion>> {

    private final Game game;

    public SaveReligionStringCellFactory(Game game) {
        this.game = game;
    }

    @Override
    public ListCell<SaveReligion> call(ListView<SaveReligion> param) {
        return new ListCell<SaveReligion>() {

            @Override
            protected void updateItem(SaveReligion value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : Eu4SaveEditorUtils.localize(value.getName(), game));
            }
        };
    }
}
