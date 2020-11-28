package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.game.Policy;
import javafx.util.StringConverter;

public class PolicyStringConverter extends StringConverter<Policy> {

    @Override
    public String toString(Policy policy) {
        return policy == null ? "" : policy.getLocalizedName();
    }

    @Override
    public Policy fromString(String policy) {
        return null;
    }
}
