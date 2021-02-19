package fr.osallek.eu4saveeditor.common;

import javax.swing.filechooser.FileSystemView;
import java.io.File;

public final class Constants {

    private Constants() {
    }

    //Args
    public static final String GAME_FOLDER_ARG= "game_folder";
    public static final String MODS_FOLDER_ARG= "mods_folder";
    public static final String SAVE_FILE_ARG= "save_file";

    //Templates
    public static final String TEMPLATES_PATH = "templates/";
    public static final String TEMPLATE_HOME = TEMPLATES_PATH + "home.fxml";
    public static final String TEMPLATE_EDITOR = TEMPLATES_PATH + "editor.fxml";

    //Images
    public static final String IMAGES_PATH = "images/";
    public static final String IMAGE_ICON = IMAGES_PATH + "EuIV_icon.png";

    public static final File DEFAULT_INSTALLATION_FOLDER = new File("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Europa Universalis IV");
    public static final String DOCUMENTS_FOLDER = FileSystemView.getFileSystemView().getDefaultDirectory().getAbsolutePath();
    public static final File EDITOR_FOLDER = new File(DOCUMENTS_FOLDER + File.separator + "Osallek" + File.separator + "Eu4SaveEditor");
    public static final File EU4_DOCUMENTS_FOLDER = new File(DOCUMENTS_FOLDER + File.separator + "Paradox Interactive"
                                                             + File.separator + "Europa Universalis IV");
    public static final File MODS_FOLDER = new File(EU4_DOCUMENTS_FOLDER.getAbsolutePath() + File.separator + "mod");
    public static final File SAVES_FOLDER = new File(EU4_DOCUMENTS_FOLDER.getAbsolutePath() + File.separator + "save games");
}
