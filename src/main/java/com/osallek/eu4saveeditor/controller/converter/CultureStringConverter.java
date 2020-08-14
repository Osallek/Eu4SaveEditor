package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.game.Culture;
import javafx.util.StringConverter;

public class CultureStringConverter extends StringConverter<Culture> {

    @Override
    public String toString(Culture culture) {
        return culture == null ? "" : culture.getLocalizedName();
    }

    @Override
    public Culture fromString(String culture) {
        return null;
    }
}
