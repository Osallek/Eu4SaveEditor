package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4saveeditor.controller.object.Decree;
import javafx.util.StringConverter;

public class DecreeStringConverter extends StringConverter<Decree> {

    @Override
    public String toString(Decree decree) {
        return decree == null ? "" : decree.toString();
    }

    @Override
    public Decree fromString(String tag) {
        return null;
    }
}
