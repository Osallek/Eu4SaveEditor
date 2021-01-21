package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.GoldenBull;
import javafx.util.StringConverter;

public class GoldenBullStringConverter extends StringConverter<GoldenBull> {

    @Override
    public String toString(GoldenBull goldenBull) {
        return goldenBull == null ? "" : goldenBull.getLocalizedName();
    }

    @Override
    public GoldenBull fromString(String goldenBull) {
        return null;
    }
}
