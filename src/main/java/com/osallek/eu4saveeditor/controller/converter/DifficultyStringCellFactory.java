package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.gameplayoptions.Difficulty;
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
                setText(value == null ? null : save.getGame().getLocalisation(value.name()));
            }
        };
    }
}
