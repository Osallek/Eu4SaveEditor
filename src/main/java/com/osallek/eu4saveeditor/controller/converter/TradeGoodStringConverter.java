package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.game.TradeGood;
import javafx.util.StringConverter;

public class TradeGoodStringConverter extends StringConverter<TradeGood> {

    @Override
    public String toString(TradeGood tradeGood) {
        return tradeGood.getLocalizedName();
    }

    @Override
    public TradeGood fromString(String tradeGood) {
        return null;
    }
}
