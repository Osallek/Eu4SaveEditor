package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4saveeditor.controller.object.Personality;
import javafx.util.StringConverter;

public class PersonalityStringConverter extends StringConverter<Personality> {

    @Override
    public String toString(Personality rulerPersonality) {
        return rulerPersonality == null ? "" : rulerPersonality.getRulerPersonality().getLocalizedName();
    }

    @Override
    public Personality fromString(String s) {
        return null;
    }
}
