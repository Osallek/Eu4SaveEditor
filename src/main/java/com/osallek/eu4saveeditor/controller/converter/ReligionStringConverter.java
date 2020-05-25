package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.game.religion.Religion;
import javafx.util.StringConverter;

public class ReligionStringConverter extends StringConverter<Religion> {

    @Override
    public String toString(Religion religion) {
        return religion.getName();
    }

    @Override
    public Religion fromString(String religion) {
        return null;
    }
}
