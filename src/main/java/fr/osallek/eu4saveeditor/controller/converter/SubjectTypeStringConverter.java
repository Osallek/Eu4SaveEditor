package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.Game;
import fr.osallek.eu4parser.model.game.SubjectType;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import javafx.util.StringConverter;

public class SubjectTypeStringConverter extends StringConverter<SubjectType> {

    private final Game game;

    public SubjectTypeStringConverter(Game game) {
        this.game = game;
    }

    @Override
    public String toString(SubjectType subjectType) {
        return subjectType == null ? "" : Eu4SaveEditorUtils.localize(subjectType.getName() + "_title", this.game);
    }

    @Override
    public SubjectType fromString(String tag) {
        return null;
    }
}
