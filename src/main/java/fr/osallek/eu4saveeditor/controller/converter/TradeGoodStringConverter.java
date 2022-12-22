package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.TradeGood;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import javafx.util.StringConverter;

public class TradeGoodStringConverter extends StringConverter<TradeGood> {

    public static final TradeGoodStringConverter INSTANCE = new TradeGoodStringConverter();

    @Override
    public String toString(TradeGood tradeGood) {
        return tradeGood == null ? "" : Eu4SaveEditorUtils.localize(tradeGood.getName(), tradeGood.getGame());
    }

    @Override
    public TradeGood fromString(String tradeGood) {
        return null;
    }
}
