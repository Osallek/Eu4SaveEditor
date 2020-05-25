package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.game.culture.Culture;
import javafx.util.StringConverter;

public class CultureStringConverter extends StringConverter<Culture> {

    @Override
    public String toString(Culture culture) {
        return culture.getName();
    }

    @Override
    public Culture fromString(String culture) {
        return null;
    }
}
