package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.game.SubjectType;
import javafx.util.StringConverter;
import org.apache.commons.lang3.StringUtils;

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
