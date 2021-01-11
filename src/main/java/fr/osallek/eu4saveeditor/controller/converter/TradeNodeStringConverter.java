package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.TradeNode;
import javafx.util.StringConverter;

public class TradeNodeStringConverter extends StringConverter<TradeNode> {

    @Override
    public String toString(TradeNode tradeNode) {
        return tradeNode == null ? "" : tradeNode.getLocalizedName();
    }

    @Override
    public TradeNode fromString(String tradeNode) {
        return null;
    }
}
