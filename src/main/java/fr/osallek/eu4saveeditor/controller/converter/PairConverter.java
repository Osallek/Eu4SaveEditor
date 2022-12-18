package fr.osallek.eu4saveeditor.controller.converter;

import javafx.util.StringConverter;
import org.apache.commons.lang3.tuple.Pair;

public class PairConverter extends StringConverter<Pair<Integer, String>> {

    @Override
    public String toString(Pair<Integer, String> pair) {
        return pair == null ? "" : pair.getValue();
    }

    @Override
    public Pair<Integer, String> fromString(String s) {
        return null;
    }
}
