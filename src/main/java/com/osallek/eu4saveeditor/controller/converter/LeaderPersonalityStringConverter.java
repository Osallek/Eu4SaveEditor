package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.game.LeaderPersonality;
import javafx.util.StringConverter;

public class LeaderPersonalityStringConverter extends StringConverter<LeaderPersonality> {

    @Override
    public String toString(LeaderPersonality leaderPersonality) {
        return leaderPersonality == null ? "" : leaderPersonality.getLocalizedName();
    }

    @Override
    public LeaderPersonality fromString(String s) {
        return null;
    }
}
