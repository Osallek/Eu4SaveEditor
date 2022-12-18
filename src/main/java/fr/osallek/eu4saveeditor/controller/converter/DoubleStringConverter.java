package fr.osallek.eu4saveeditor.controller.converter;

import java.util.Locale;
import javafx.util.StringConverter;

public class DoubleStringConverter extends StringConverter<Double> {

    @Override
    public String toString(Double aDouble) {
        return String.format(Locale.ENGLISH, "%.3f", aDouble);
    }

    @Override
    public Double fromString(String aDouble) {
        return Double.parseDouble(aDouble);
    }
}
