package com.osallek.eu4saveeditor.common;

import com.osallek.eu4parser.model.game.localisation.Eu4Language;

import java.util.Arrays;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

public enum ReadSaveStep {
    META_DATA("players_countries", 0, "Reading meta datas...", "Lecture des méta-données...", "Lesen von Metadaten...", "Leyendo los metadatos..."),
    PROVINCES("provinces", 1, "Reading provinces...", "Lecture des provinces...", "Leseprovinzen...", "Provincias de lectura..."),
    COUNTRIES("countries", 2, "Reading countries...", "Lecture des pays...", "Leseländer...", "Países de lectura..."),
    WARS("active_advisors", 3, "Reading wars...", "Lectures des guerres...", "Kriege lesen...", "Leer las guerras..."),
    GAME("campaign_stats", 4, "Reading game files...", "Lecture des fichiers du jeu...", "Lesen von Spieldateien...", "Leer los archivos de los juegos...");

    public final String itemName;

    public final int step;

    public final String english;

    public final String french;

    public final String german;

    public final String spanish;

    public static final Map<String, ReadSaveStep> BY_ITEM_NAME;

    public static final int NB_STEPS;

    static {
        NB_STEPS = ReadSaveStep.values().length;
        BY_ITEM_NAME = Arrays.stream(ReadSaveStep.values()).collect(Collectors.toMap(ReadSaveStep::getItemName, Function.identity()));
    }

    public String getText(Eu4Language eu4Language) {
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

    ReadSaveStep(String itemName, int step, String english, String french, String german, String spanish) {
        this.itemName = itemName;
        this.step = step;
        this.english = english;
        this.french = french;
        this.german = german;
        this.spanish = spanish;
    }

    public String getItemName() {
        return itemName;
    }

    public int getStep() {
        return step;
    }

    public String getEnglish() {
        return english;
    }

    public String getFrench() {
        return french;
    }

    public String getGerman() {
        return german;
    }

    public String getSpanish() {
        return spanish;
    }
}
