package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.clausewitzparser.common.ClausewitzUtils;
import fr.osallek.eu4parser.model.save.province.SaveProvince;
import javafx.util.StringConverter;

public class ProvinceStringConverter extends StringConverter<SaveProvince> {

    public static final ProvinceStringConverter INSTANCE = new ProvinceStringConverter();

    @Override
    public String toString(SaveProvince province) {
        return province == null ? "" : ClausewitzUtils.removeQuotes(province.getName());
    }

    @Override
    public SaveProvince fromString(String province) {
        return null;
    }
}
