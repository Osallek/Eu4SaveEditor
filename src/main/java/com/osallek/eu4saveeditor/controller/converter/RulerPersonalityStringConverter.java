package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.game.RulerPersonality;
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
