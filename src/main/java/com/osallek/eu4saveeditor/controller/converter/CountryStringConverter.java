package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.save.country.Country;
import javafx.util.StringConverter;

public class CountryStringConverter extends StringConverter<Country> {

    @Override
    public String toString(Country country) {
        return country.getTag();
    }

    @Override
    public Country fromString(String tag) {
        return null;
    }
}
