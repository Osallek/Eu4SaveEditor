package fr.osallek.eu4saveeditor.controller.converter;

import javafx.util.StringConverter;
import org.apache.commons.lang3.tuple.Pair;

public class PairConverter extends StringConverter<Pair<String, String>> {

    @Override
    public String toString(Pair<String, String> pair) {
        return pair == null ? "" : pair.getValue();
    }

    @Override
    public Pair<String, String> fromString(String s) {
        return null;
    }
}
