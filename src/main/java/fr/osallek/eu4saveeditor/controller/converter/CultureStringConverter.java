package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.Culture;
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
