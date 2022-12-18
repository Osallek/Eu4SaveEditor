package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.Game;
import fr.osallek.eu4parser.model.game.TradeNode;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import javafx.util.StringConverter;

public class TradeNodeStringConverter extends StringConverter<TradeNode> {

    private final Game game;

    public TradeNodeStringConverter(Game game) {
        this.game = game;
    }

    @Override
    public String toString(TradeNode tradeNode) {
        return tradeNode == null ? "" : Eu4SaveEditorUtils.localize(tradeNode.getName(), this.game);
    }

    @Override
    public TradeNode fromString(String tradeNode) {
        return null;
    }
}
