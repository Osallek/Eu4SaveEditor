package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.game.ReligiousReform;
import javafx.util.StringConverter;

public class ReligiousReformStringConverter extends StringConverter<ReligiousReform> {

    @Override
    public String toString(ReligiousReform religiousReform) {
        return religiousReform == null ? "" : religiousReform.getLocalizedName();
    }

    @Override
    public ReligiousReform fromString(String s) {
        return null;
    }
}
