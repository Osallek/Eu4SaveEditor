package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.clausewitzparser.common.ClausewitzUtils;
import fr.osallek.eu4parser.model.save.country.Country;
import fr.osallek.eu4parser.model.save.province.SaveProvince;
import javafx.util.Callback;

public class ProvinceCountryCallBack implements Callback<Object, String> {

    @Override
    public String call(Object param) {
        if (SaveProvince.class.equals(param.getClass())) {
            return ClausewitzUtils.removeQuotes(((SaveProvince) param).getName()) + " (" + ((SaveProvince) param).getId() + ")";
        } else if (Country.class.equals(param.getClass())) {
            return ClausewitzUtils.removeQuotes(((Country) param).getLocalizedName()) + " (" + ((Country) param).getTag() + ")";
        } else {
            return "";
        }
    }
}
