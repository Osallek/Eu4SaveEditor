package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.clausewitzparser.common.ClausewitzUtils;
import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4parser.model.save.country.SaveCountry;
import fr.osallek.eu4parser.model.save.province.SaveProvince;
import javafx.util.Callback;

public class ProvinceCountryCallBack implements Callback<Object, String> {

    @Override
    public String call(Object param) {
        if (param instanceof SaveProvince province) {
            return ClausewitzUtils.removeQuotes(province.getName()) + " (" + province.getId() + ")";
        } else if (param instanceof SaveCountry country) {
            return CountryStringConverter.INSTANCE.toString(country) + " (" + country.getTag() + ")";
        } else {
            return "";
        }
    }
}
