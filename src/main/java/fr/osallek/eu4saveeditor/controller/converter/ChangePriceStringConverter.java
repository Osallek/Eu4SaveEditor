package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.save.changeprices.ChangePrice;
import javafx.util.StringConverter;

public class ChangePriceStringConverter extends StringConverter<ChangePrice> {

    @Override
    public String toString(ChangePrice changePrice) {
        return changePrice == null ? "" : changePrice.getLocalizedName();
    }

    @Override
    public ChangePrice fromString(String changePrice) {
        return null;
    }
}
