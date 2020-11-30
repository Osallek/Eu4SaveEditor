package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.game.FetishistCult;
import javafx.util.StringConverter;

public class FetishistCultStringConverter extends StringConverter<FetishistCult> {

    @Override
    public String toString(FetishistCult fetishistCult) {
        return fetishistCult == null ? "" : fetishistCult.getLocalizedName();
    }

    @Override
    public FetishistCult fromString(String s) {
        return null;
    }
}
