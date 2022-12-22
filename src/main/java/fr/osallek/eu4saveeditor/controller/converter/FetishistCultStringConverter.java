package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.FetishistCult;
import fr.osallek.eu4parser.model.game.Game;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import javafx.util.StringConverter;

public class FetishistCultStringConverter extends StringConverter<FetishistCult> {

    private final Game game;

    public FetishistCultStringConverter(Game game) {
        this.game = game;
    }

    @Override
    public String toString(FetishistCult fetishistCult) {
        return fetishistCult == null ? "" : Eu4SaveEditorUtils.localize(fetishistCult.getName(), this.game);
    }

    @Override
    public FetishistCult fromString(String s) {
        return null;
    }
}
