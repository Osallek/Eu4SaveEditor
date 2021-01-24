package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.SubjectType;
import javafx.util.StringConverter;

public class SubjectTypeStringConverter extends StringConverter<SubjectType> {

    @Override
    public String toString(SubjectType subjectType) {
        return subjectType == null ? "" : subjectType.getLocalizedName();
    }

    @Override
    public SubjectType fromString(String tag) {
        return null;
    }
}
