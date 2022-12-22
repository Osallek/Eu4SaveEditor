package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.Game;
import fr.osallek.eu4parser.model.game.PersonalDeity;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import javafx.util.StringConverter;

public class PersonalDeityStringConverter extends StringConverter<PersonalDeity> {

    private final Game game;

    public PersonalDeityStringConverter(Game game) {
        this.game = game;
    }

    @Override
    public String toString(PersonalDeity personalDeity) {
        return personalDeity == null ? "" : Eu4SaveEditorUtils.localize(personalDeity.getName(), this.game);
    }

    @Override
    public PersonalDeity fromString(String s) {
        return null;
    }
}
