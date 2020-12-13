package fr.osallek.eu4saveeditor.i18n;

import fr.osallek.eu4parser.model.game.localisation.Eu4Language;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

public enum SheetCategory {
    GENERAL("General", "Général", "Allgemein", "General"),
    PROVINCE_POLITICAL("Political", "Politique", "Politik", "Política"),
    ECONOMY("Economy", "Économie", "Wirtschaft", "Economía"),
    PROVINCE_INSTITUTIONS("Institutions", "Institutions", "Institutionen", "Instituciones"),
    PROVINCE_BUILDINGS("Buildings", "Bâtiments", "Gebäude", "Edificios"),
    PROVINCE_COLONY("Colonization", "Colonisation", "Kolonisierung", "Colonización"),
    SAVE_GAME_OPTIONS("Options", "Options", "Einstellungen", "Opciones"),
    SAVE_INSTITUTIONS("Institutions", "Institutions", "Institutionen", "Instituciones"),
    SAVE_GOODS("Trade Goods", "Biens commerciaux", "Handelsgüter", "Bienes Comerciales"),
    SAVE_HRE("Holy Roman Empire", "Saint Empire romain germanique", "Heilige Römische Reich", "Sacro Imperio Romano"),
    SAVE_CELESTIAL_EMPIRE("Empire of China", "Empire de Chine", "Das chinesische Reich", "Emperador de China"),
    COUNTRY_GOVERNMENT("Government", "Gouvernement", "Regierung", "Gobierno"),
    COUNTRY_MILITARY("Military", "Militaire", "Militär", "Militar"),
    COUNTRY_TECHNOLOGY("Techn", "Militaire", "Militär", "Militar");

    final String english;

    final String french;

    final String german;

    final String spanish;

    private static final Map<String, SheetCategory> BY_ENGLISH = new HashMap<>();

    private static final Map<String, SheetCategory> BY_FRENCH = new HashMap<>();

    private static final Map<String, SheetCategory> BY_GERMAN = new HashMap<>();

    private static final Map<String, SheetCategory> BY_SPANISH = new HashMap<>();

    static {
        Arrays.stream(values()).forEach(sheetCategory -> BY_ENGLISH.put(sheetCategory.english, sheetCategory));
        Arrays.stream(values()).forEach(sheetCategory -> BY_FRENCH.put(sheetCategory.french, sheetCategory));
        Arrays.stream(values()).forEach(sheetCategory -> BY_GERMAN.put(sheetCategory.german, sheetCategory));
        Arrays.stream(values()).forEach(sheetCategory -> BY_SPANISH.put(sheetCategory.spanish, sheetCategory));
    }

    SheetCategory(String english, String french, String german, String spanish) {
        this.english = english;
        this.french = french;
        this.german = german;
        this.spanish = spanish;
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

    public String getForDefaultLocale() {
        return getForLanguage(Eu4Language.getByLocale(Locale.getDefault()));
    }

    public static SheetCategory getByLocale(String value) {
        switch (Eu4Language.getByLocale(Locale.getDefault())) {
            case SPANISH:
                return BY_SPANISH.get(value);
            case GERMAN:
                return BY_GERMAN.get(value);
            case ENGLISH:
                return BY_ENGLISH.get(value);
            case FRENCH:
                return BY_FRENCH.get(value);
        }

        return SheetCategory.GENERAL;
    }
}
