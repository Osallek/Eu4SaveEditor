package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.RulerPersonality;
import javafx.util.StringConverter;

public class RulerPersonalityStringConverter extends StringConverter<RulerPersonality> {

    @Override
    public String toString(RulerPersonality rulerPersonality) {
        return rulerPersonality == null ? "" : rulerPersonality.getLocalizedName();
    }

    @Override
    public RulerPersonality fromString(String s) {
        return null;
    }
}
