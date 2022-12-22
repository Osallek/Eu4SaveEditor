package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.TradeNode;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import javafx.util.StringConverter;

public class TradeNodeStringConverter extends StringConverter<TradeNode> {

    public static final TradeNodeStringConverter INSTANCE = new TradeNodeStringConverter();

    @Override
    public String toString(TradeNode tradeNode) {
        return tradeNode == null ? "" : Eu4SaveEditorUtils.localize(tradeNode.getName(), tradeNode.getGame());
    }

    @Override
    public TradeNode fromString(String tradeNode) {
        return null;
    }
}
