package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.save.country.SaveCountry;
import fr.osallek.eu4parser.model.save.province.SaveProvince;
import javafx.util.StringConverter;

public class ProvinceCountryStringConverter extends StringConverter<Object> {

    @Override
    public String toString(Object param) {
        if (param instanceof SaveProvince province) {
            return ProvinceIdStringConverter.INSTANCE.toString(province);
        } else if (param instanceof SaveCountry country) {
            return CountryStringConverter.INSTANCE.toString(country) + " (" + country.getTag() + ")";
        } else {
            return "";
        }
    }

    @Override
    public Object fromString(String string) {
        return null;
    }
}
