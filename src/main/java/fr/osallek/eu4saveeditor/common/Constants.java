package fr.osallek.eu4saveeditor.common;

import java.io.File;

public final class Constants {

    private Constants() {
    }

    public static final String SUPPORTED_GAME_VERSION = "1.30.4";

    //Templates
    public static final String TEMPLATES_PATH = "/templates/";
    public static final String TEMPLATE_HOME = TEMPLATES_PATH + "home.fxml";
    public static final String TEMPLATE_EDITOR = TEMPLATES_PATH + "editor.fxml";

    //Images
    public static final String IMAGES_PATH = "/images/";
    public static final String IMAGE_ICON = IMAGES_PATH + "EuIV_icon.png";

    public static final File DEFAULT_INSTALLATION_FOLDER = new File("C:\\Program Files (x86)\\Steam\\steamapps\\common\\Europa Universalis IV");
    public static final File DOCUMENTS_FOLDER = new File(new javax.swing.JFileChooser().getFileSystemView().getDefaultDirectory().getAbsolutePath()
                                                         + File.separator + "Paradox Interactive" + File.separator + "Europa Universalis IV");
    public static final File MODS_FOLDER = new File(DOCUMENTS_FOLDER.getAbsolutePath() + File.separator + "mod");
    public static final File SAVES_FOLDER = new File(DOCUMENTS_FOLDER.getAbsolutePath() + File.separator + "save games");
}
