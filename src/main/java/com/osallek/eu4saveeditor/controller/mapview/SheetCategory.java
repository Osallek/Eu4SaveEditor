package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.eu4parser.model.game.localisation.Eu4Language;

import java.util.Locale;

public enum SheetCategory {
    PROVINCE_GENERAL("General", "Général", "Allgemein", "General"),
    PROVINCE_POLITICAL("Political", "Politique", "Politik", "Política"),
    PROVINCE_ECONOMY("Economy", "Économie", "Wirtschaft", "Economía"),
    PROVINCE_INSTITUTIONS("Institutions", "Institutions", "Institutionen", "Instituciones");

    final String english;

    final String french;

    final String german;

    final String spanish;

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
}
