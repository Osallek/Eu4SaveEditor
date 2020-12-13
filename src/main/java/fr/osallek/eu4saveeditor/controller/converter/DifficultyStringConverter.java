package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4parser.model.save.gameplayoptions.Difficulty;
import javafx.util.StringConverter;

public class DifficultyStringConverter extends StringConverter<Difficulty> {

    private final Save save;

    public DifficultyStringConverter(Save save) {
        this.save = save;
    }

    @Override
    public String toString(Difficulty difficulty) {
        return difficulty == null ? "" : this.save.getGame().getLocalisation(difficulty.name());
    }

    @Override
    public Difficulty fromString(String difficulty) {
        return difficulty == null ? null : Difficulty.valueOf(difficulty);
    }
}
