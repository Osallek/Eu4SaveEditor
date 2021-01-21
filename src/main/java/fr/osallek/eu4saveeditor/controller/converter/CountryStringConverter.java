package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.save.country.Country;
import fr.osallek.eu4saveeditor.controller.EditorController;
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
