package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.clausewitzparser.common.ClausewitzUtils;
import fr.osallek.eu4parser.model.save.country.SaveCountry;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import fr.osallek.eu4saveeditor.controller.EditorController;
import javafx.util.StringConverter;
import org.apache.commons.lang3.ObjectUtils;

public class CountryStringConverter extends StringConverter<SaveCountry> {

    public static final CountryStringConverter INSTANCE = new CountryStringConverter();

    @Override
    public String toString(SaveCountry country) {
        return (country == null || EditorController.dummyCountry.equals(country)) ? "" :
               ObjectUtils.firstNonNull(ClausewitzUtils.removeQuotes(country.getLocalizedName()),
                                        Eu4SaveEditorUtils.localize(country.getTag(), country.getSave().getGame()));
    }

    @Override
    public SaveCountry fromString(String tag) {
        return null;
    }
}
