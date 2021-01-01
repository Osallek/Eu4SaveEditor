package fr.osallek.eu4saveeditor.i18n;

import fr.osallek.eu4parser.model.game.localisation.Eu4Language;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

public enum MenusI18n {
    SAVE_AS("Save as...", "Enregistrer sous...", "Speichern unter...", "Guardar como..."),
    SELECT_GAME_FOLDER("EuIV installation folder", "Dossier d'installation de EuIV", "EuIV-Installationsordner", "Carpeta de instalación de EuIV"),
    SELECT_GAME_FOLDER_DESC("EuIV installation folder: ", "Dossier d'installation de EuIV : ", "EuIV-Installationsordner: ", "Carpeta de instalación de EuIV: "),
    SELECT_MOD_FOLDER("EuIV mods folder", "Dossier des mods de EuIV", "EuIV-Modsordner", "Carpeta de mods de EuIV"),
    SELECT_MOD_FOLDER_DESC("EuIV mods folder: ", "Dossier des mods de EuIV : ", "EuIV-Modsordner: ", "Carpeta de mods de EuIV: "),
    SELECT_SAVE_FILE("Select a save", "Choisir une sauvegarde", "Wählen Sie eine Sicherung", "Elija una salvaguardia"),
    SELECT_SAVE_FILE_DESC("Select a save file: ", "Choisir une sauvegarde : ", "Wählen Sie eine Sicherung: ", "Elija una salvaguardia: "),
    EU4_EXT_DESC("EuIV save", "Sauvegarde EuIV", "EuIV Sicherung", "EuIV salvaguardia"),
    CHOOSE_FOLDER("Choose folder", "Choisir le dossier", "Wählen Sie den Ordner", "Elija la carpeta"),
    CHOOSE_FILE("Choose file", "Choisir le fichier", "Wählen Sie die Datei", "Elija el archivo"),
    START_EXTRACT("Start extract", "Démarrer l'extraction", "Extraktion beginnen", "Comienza la extracción"),
    EXTRACTING("Extracting...", "Extraction en cours...", "Extraktion im Gange...", "Extracción en curso...");

    final String english;

    final String french;

    final String german;

    final String spanish;

    private static final Map<String, MenusI18n> BY_ENGLISH = new HashMap<>();

    private static final Map<String, MenusI18n> BY_FRENCH = new HashMap<>();

    private static final Map<String, MenusI18n> BY_GERMAN = new HashMap<>();

    private static final Map<String, MenusI18n> BY_SPANISH = new HashMap<>();

    static {
        Arrays.stream(values()).forEach(sheetCategory -> BY_ENGLISH.put(sheetCategory.english, sheetCategory));
        Arrays.stream(values()).forEach(sheetCategory -> BY_FRENCH.put(sheetCategory.french, sheetCategory));
        Arrays.stream(values()).forEach(sheetCategory -> BY_GERMAN.put(sheetCategory.german, sheetCategory));
        Arrays.stream(values()).forEach(sheetCategory -> BY_SPANISH.put(sheetCategory.spanish, sheetCategory));
    }

    MenusI18n(String english, String french, String german, String spanish) {
        this.english = english;
        this.french = french;
        this.german = german;
        this.spanish = spanish;
    }

    public String getForDefaultLocale() {
        return getForLanguage(Eu4Language.getByLocale(Locale.getDefault()));
    }

    public String getForLanguage(Eu4Language eu4Language) {
        switch (eu4Language) {
            case FRENCH:
                return this.french;

            case GERMAN:
                return this.german;

            case SPANISH:
                return this.spanish;

            case ENGLISH:
            default:
                return this.english;
        }
    }
}
