package fr.osallek.eu4saveeditor.common;

import fr.osallek.clausewitzparser.common.ClausewitzUtils;
import fr.osallek.eu4parser.Eu4Parser;
import fr.osallek.eu4parser.model.game.Game;
import fr.osallek.eu4parser.model.game.localisation.Eu4Language;
import fr.osallek.eu4parser.model.game.localisation.Localisation;
import fr.osallek.eu4parser.model.save.country.SaveCountry;
import java.awt.image.BufferedImage;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Stream;
import javafx.scene.image.ImageView;
import javafx.scene.image.PixelWriter;
import javafx.scene.image.WritableImage;
import javafx.scene.paint.Color;

public class Eu4SaveEditorUtils {

    private Eu4SaveEditorUtils() {}

    public static Color countryToMapColor(SaveCountry country) {
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

    public static String localize(String s, Game game) {
        return Optional.ofNullable(game.getLocalisation(ClausewitzUtils.removeQuotes(s), Eu4Language.getDefault()))
                       .or(() -> Optional.ofNullable(game.getLocalisation(ClausewitzUtils.removeQuotes(s), Eu4Language.ENGLISH)))
                       .map(Localisation::getValue)
                       .orElse(s);
    }

    public static ImageView bufferedToView(BufferedImage buffered) {
        WritableImage wr = null;

        if (buffered != null) {
            wr = new WritableImage(buffered.getWidth(), buffered.getHeight());
            PixelWriter pw = wr.getPixelWriter();

            for (int x = 0; x < buffered.getWidth(); x++) {
                for (int y = 0; y < buffered.getHeight(); y++) {
                    pw.setArgb(x, y, buffered.getRGB(x, y));
                }
            }
        }

        return new ImageView(wr);
    }

    public static List<Path> getSaves(File saveFolder) {
        try {
            if (saveFolder.exists() && saveFolder.isDirectory()) {
                try (Stream<Path> stream = Files.walk(saveFolder.toPath())) {
                    return stream.filter(path -> path.getFileName().toString().endsWith(".eu4"))
                                 .filter(Eu4Parser::isValid)
                                 .filter(Predicate.not(Eu4Parser::isIronman))
                                 .sorted(Comparator.comparing(t -> t.toFile().lastModified(), Comparator.reverseOrder()))
                                 .toList();
                }
            }
        } catch (Exception ignored) {
        }

        return new ArrayList<>();
    }
}
