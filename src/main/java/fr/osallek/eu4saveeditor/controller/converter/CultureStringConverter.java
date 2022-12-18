package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.Culture;
import fr.osallek.eu4parser.model.game.Game;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import javafx.util.StringConverter;

public class CultureStringConverter extends StringConverter<Culture> {

    private final Game game;

    public CultureStringConverter(Game game) {
        this.game = game;
    }

    @Override
    public String toString(Culture culture) {
        return culture == null ? "" : Eu4SaveEditorUtils.localize(culture.getName(), this.game);
    }

    @Override
    public Culture fromString(String culture) {
        return null;
    }
}
