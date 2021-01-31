package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.clausewitzparser.common.ClausewitzUtils;
import fr.osallek.eu4parser.model.save.province.SaveProvince;
import javafx.util.Callback;

public class ProvinceStringCallBack implements Callback<SaveProvince, String> {

    @Override
    public String call(SaveProvince param) {
        return ClausewitzUtils.removeQuotes(param.getName()) + " (" + param.getId() + ")";
    }
}
