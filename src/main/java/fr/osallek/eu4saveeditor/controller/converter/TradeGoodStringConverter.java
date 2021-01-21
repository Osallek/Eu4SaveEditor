package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.TradeGood;
import javafx.util.StringConverter;

public class TradeGoodStringConverter extends StringConverter<TradeGood> {

    @Override
    public String toString(TradeGood tradeGood) {
        return tradeGood == null ? "" : tradeGood.getLocalizedName();
    }

    @Override
    public TradeGood fromString(String tradeGood) {
        return null;
    }
}
