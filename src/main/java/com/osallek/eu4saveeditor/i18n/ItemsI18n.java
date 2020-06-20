package com.osallek.eu4saveeditor.i18n;

import com.osallek.eu4parser.model.game.localisation.Eu4Language;

import java.util.Locale;

public enum ItemsI18n {
    FIRED_EVENTS("Fired events", "Événements passés", "Vergangene Ereignisse", "Eventos pasados"),
    PENDING_EVENTS("Pending events", "Événements en attente", "Ausstehende Ereignisse", "Eventos pendientes");

    final String english;

    final String french;

    final String german;

    final String spanish;

    ItemsI18n(String english, String french, String german, String spanish) {
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
