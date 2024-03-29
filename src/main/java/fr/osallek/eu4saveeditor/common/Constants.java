package fr.osallek.eu4saveeditor.common;

import java.io.File;
import java.time.chrono.IsoChronology;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.FormatStyle;
import java.util.Locale;
import javax.swing.filechooser.FileSystemView;

public final class Constants {

    private Constants() {
    }

    //Args
    public static final String GAME_FOLDER_ARG = "game_folder";
    public static final String SAVE_FILE_ARG = "save_file";
    public static final String OVERRIDE_ARG = "override";

    //Templates
    public static final String TEMPLATES_PATH = "templates/";
    public static final String TEMPLATE_HOME = TEMPLATES_PATH + "home.fxml";
    public static final String TEMPLATE_EDITOR = TEMPLATES_PATH + "editor.fxml";

    //Images
    public static final String IMAGE_ICON = "/images/favicon.ico";

    public static final String DOCUMENTS_FOLDER = FileSystemView.getFileSystemView().getDefaultDirectory().getAbsolutePath();
    public static final File EDITOR_FOLDER = new File(DOCUMENTS_FOLDER + File.separator + "Osallek" + File.separator + "Eu4SaveEditor");
    public static final File EU4_DOCUMENTS_FOLDER = new File(DOCUMENTS_FOLDER + File.separator + "Paradox Interactive" + File.separator
                                                             + "Europa Universalis IV");
    public static final File DEFAULT_SAVES_FOLDER = new File(EU4_DOCUMENTS_FOLDER.getAbsolutePath() + File.separator + "save games");

    public static final Locale LOCALE = Locale.getDefault();

    //To get full years
    public static final DateTimeFormatter PRETTY_DATE_FORMAT = DateTimeFormatter.ofPattern(DateTimeFormatterBuilder.getLocalizedDateTimePattern(FormatStyle.SHORT, null, IsoChronology.INSTANCE, LOCALE)
                                                                                                                   .replace("yyyy", "yy")
                                                                                                                   .replace("yy", "yyyy")
                                                                                                                   .replace("uuuu", "uu")
                                                                                                                   .replace("uu", "uuuu"));
}
