package fr.osallek.eu4saveeditor.common;

import fr.osallek.eu4parser.model.save.country.Country;
import javafx.scene.paint.Color;

public class Eu4SaveEditorUtils {

    private Eu4SaveEditorUtils() {}

    public static Color countryToMapColor(Country country) {
        return Color.rgb(country.getColors().getMapColor().getRed(), country.getColors().getMapColor().getGreen(), country.getColors().getMapColor().getBlue());
    }

    public static String readArg(String[] args, String arg) {
        for (String s : args) {
            if (s.matches("^-?-?" + arg + "=.*$")) {
                return s.substring(s.indexOf(arg + "=") + arg.length() + 1).replace("\"", "");
            }
        }

        return null;
    }
}
