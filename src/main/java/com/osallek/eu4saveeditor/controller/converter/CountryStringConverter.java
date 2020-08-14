package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4saveeditor.controller.EditorController;
import javafx.util.StringConverter;

public class CountryStringConverter extends StringConverter<Country> {

    @Override
    public String toString(Country country) {
        return (country == null || EditorController.dummyCountry.equals(country)) ? "" : country.getLocalizedName();
    }

    @Override
    public Country fromString(String tag) {
        return null;
    }
}
