package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.Game;
import fr.osallek.eu4parser.model.save.SaveReligion;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import javafx.util.StringConverter;

public class ReligionStringConverter extends StringConverter<SaveReligion> {

    private final Game game;

    public ReligionStringConverter(Game game) {
        this.game = game;
    }

    @Override
    public String toString(SaveReligion religion) {
        return religion == null ? "" : Eu4SaveEditorUtils.localize(religion.getName(), this.game);
    }

    @Override
    public SaveReligion fromString(String religion) {
        return null;
    }
}
