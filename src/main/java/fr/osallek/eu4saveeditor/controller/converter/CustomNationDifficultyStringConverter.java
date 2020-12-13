package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4parser.model.save.gameplayoptions.CustomNationDifficulty;
import javafx.util.StringConverter;

public class CustomNationDifficultyStringConverter extends StringConverter<CustomNationDifficulty> {

    private final Save save;

    public CustomNationDifficultyStringConverter(Save save) {
        this.save = save;
    }

    @Override
    public String toString(CustomNationDifficulty difficulty) {
        return difficulty == null ? "" : this.save.getGame().getLocalisation(difficulty.name());
    }

    @Override
    public CustomNationDifficulty fromString(String difficulty) {
        return difficulty == null ? null : CustomNationDifficulty.valueOf(difficulty);
    }
}
