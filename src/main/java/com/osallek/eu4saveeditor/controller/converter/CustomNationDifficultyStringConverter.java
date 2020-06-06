package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.gameplayoptions.CustomNationDifficulty;
import javafx.util.StringConverter;

public class CustomNationDifficultyStringConverter extends StringConverter<CustomNationDifficulty> {

    private final Save save;

    public CustomNationDifficultyStringConverter(Save save) {
        this.save = save;
    }

    @Override
    public String toString(CustomNationDifficulty difficulty) {
        return this.save.getGame().getLocalisation(difficulty.name());
    }

    @Override
    public CustomNationDifficulty fromString(String difficulty) {
        return CustomNationDifficulty.valueOf(difficulty);
    }
}
