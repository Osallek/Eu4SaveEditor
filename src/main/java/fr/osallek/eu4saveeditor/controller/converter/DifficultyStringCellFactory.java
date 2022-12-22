package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4parser.model.save.gameplayoptions.Difficulty;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class DifficultyStringCellFactory implements Callback<ListView<Difficulty>, ListCell<Difficulty>> {

    private final Save save;

    public DifficultyStringCellFactory(Save save) {
        this.save = save;
    }

    @Override
    public ListCell<Difficulty> call(ListView<Difficulty> param) {
        return new ListCell<Difficulty>() {

            @Override
            protected void updateItem(Difficulty value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : Eu4SaveEditorUtils.localize(value.name(), save.getGame()));
            }
        };
    }
}
