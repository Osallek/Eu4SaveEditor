package com.osallek.eu4saveeditor.common;

import com.osallek.eu4parser.model.game.localisation.Eu4Language;

import java.util.Arrays;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

public enum WriteSaveStep {
    META_DATA("players_countries", 0, "Writing meta data...", "Écriture des méta-données...", "Schreiben von Metadaten...", "Escribir metadatos..."),
    PROVINCES("provinces", 1, "Writing provinces...", "Écriture des provinces...", "Provinzen schreiben...", "Escribir provincias..."),
    COUNTRIES("countries", 2, "Writing countries...", "Écriture des pays...", "Länder schreiben...", "Los países que escriben..."),
    WARS("active_advisors", 3, "Writing wars...", "Écriture des guerres...", "Kriege schreiben...", "Escribir guerras...");

    public final String itemName;

    public final int step;

    public final String english;

    public final String french;

    public final String german;

    public final String spanish;

    public static final Map<String, WriteSaveStep> BY_ITEM_NAME;

    public static final int NB_STEPS;

    static {
        NB_STEPS = WriteSaveStep.values().length;
        BY_ITEM_NAME = Arrays.stream(WriteSaveStep.values()).collect(Collectors.toMap(WriteSaveStep::getItemName, Function.identity()));
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

    WriteSaveStep(String itemName, int step, String english, String french, String german, String spanish) {
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
