package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.clausewitzparser.common.ClausewitzUtils;
import fr.osallek.eu4parser.model.save.province.SaveProvince;
import javafx.util.StringConverter;

public class ProvinceIdStringConverter extends StringConverter<SaveProvince> {

    public static final ProvinceIdStringConverter INSTANCE = new ProvinceIdStringConverter();

    @Override
    public String toString(SaveProvince province) {
        return province == null ? "" : ClausewitzUtils.removeQuotes(province.getName()) + " (" + province.getId() + ")";
    }

    @Override
    public SaveProvince fromString(String province) {
        return null;
    }
}
