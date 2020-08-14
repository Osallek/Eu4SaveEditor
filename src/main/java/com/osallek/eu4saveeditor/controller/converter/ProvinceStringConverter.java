package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.clausewitzparser.common.ClausewitzUtils;
import com.osallek.eu4parser.model.save.province.SaveProvince;
import javafx.util.StringConverter;

public class ProvinceStringConverter extends StringConverter<SaveProvince> {

    @Override
    public String toString(SaveProvince province) {
        return province == null ? "" : ClausewitzUtils.removeQuotes(province.getName());
    }

    @Override
    public SaveProvince fromString(String province) {
        return null;
    }
}
