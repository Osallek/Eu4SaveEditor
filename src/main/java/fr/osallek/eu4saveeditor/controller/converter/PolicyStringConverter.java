package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.Game;
import fr.osallek.eu4parser.model.game.Policy;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import javafx.util.StringConverter;

public class PolicyStringConverter extends StringConverter<Policy> {

    private final Game game;

    public PolicyStringConverter(Game game) {
        this.game = game;
    }

    @Override
    public String toString(Policy policy) {
        return policy == null ? "" : Eu4SaveEditorUtils.localize(policy.getName(), this.game);
    }

    @Override
    public Policy fromString(String policy) {
        return null;
    }
}
