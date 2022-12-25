package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4parser.model.save.gameplayoptions.CustomNationDifficulty;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class CustomNationDifficultyStringCellFactory implements Callback<ListView<CustomNationDifficulty>, ListCell<CustomNationDifficulty>> {

    private final Save save;

    public CustomNationDifficultyStringCellFactory(Save save) {
        this.save = save;
    }

    @Override
    public ListCell<CustomNationDifficulty> call(ListView<CustomNationDifficulty> param) {
        return new ListCell<>() {

            @Override
            protected void updateItem(CustomNationDifficulty value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : Eu4SaveEditorUtils.localize(value.name(), save.getGame()));
            }
        };
    }
}
