package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.eu4parser.model.game.localisation.Eu4Language;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

public enum SheetCategory {
    PROVINCE_GENERAL("General", "Général", "Allgemein", "General"),
    PROVINCE_POLITICAL("Political", "Politique", "Politik", "Política"),
    PROVINCE_ECONOMY("Economy", "Économie", "Wirtschaft", "Economía"),
    PROVINCE_INSTITUTIONS("Institutions", "Institutions", "Institutionen", "Instituciones"),
    PROVINCE_BUILDINGS("Buildings", "Bâtiments", "Gebäude", "Edificios");

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

        return SheetCategory.PROVINCE_GENERAL;
    }
}
