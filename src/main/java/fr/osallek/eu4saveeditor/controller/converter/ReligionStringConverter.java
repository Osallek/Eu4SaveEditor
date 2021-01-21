package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.save.SaveReligion;
import javafx.util.StringConverter;

public class ReligionStringConverter extends StringConverter<SaveReligion> {

    @Override
    public String toString(SaveReligion religion) {
        return religion == null ? "" : religion.getLocalizedName();
    }

    @Override
    public SaveReligion fromString(String religion) {
        return null;
    }
}
