package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.game.Decree;
import javafx.util.StringConverter;

public class DecreeStringConverter extends StringConverter<Decree> {

    @Override
    public String toString(Decree decree) {
        return decree == null ? "" : decree.getLocalizedName();
    }

    @Override
    public Decree fromString(String tag) {
        return null;
    }
}
