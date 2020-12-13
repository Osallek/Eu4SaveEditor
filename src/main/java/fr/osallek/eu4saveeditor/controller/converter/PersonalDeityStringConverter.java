package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.PersonalDeity;
import javafx.util.StringConverter;

public class PersonalDeityStringConverter extends StringConverter<PersonalDeity> {

    @Override
    public String toString(PersonalDeity personalDeity) {
        return personalDeity == null ? "" : personalDeity.getLocalizedName();
    }

    @Override
    public PersonalDeity fromString(String s) {
        return null;
    }
}
