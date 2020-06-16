package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.save.SaveReligion;
import javafx.util.StringConverter;

public class ReligionStringConverter extends StringConverter<SaveReligion> {

    @Override
    public String toString(SaveReligion religion) {
        return religion.getLocalizedName();
    }

    @Override
    public SaveReligion fromString(String religion) {
        return null;
    }
}
