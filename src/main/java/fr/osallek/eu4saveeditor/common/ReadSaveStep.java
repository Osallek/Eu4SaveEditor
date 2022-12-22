package fr.osallek.eu4saveeditor.common;

import java.util.Arrays;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

public enum ReadSaveStep {
    META_DATA("players_countries", 0),
    PROVINCES("provinces", 1),
    COUNTRIES("countries", 2),
    WARS("active_advisors", 3),
    GAME("idea_dates", 4);

    public final String itemName;

    public final int step;

    private static final Map<String, ReadSaveStep> BY_ITEM_NAME;

    public static final int NB_STEPS;

    static {
        NB_STEPS = ReadSaveStep.values().length;
        BY_ITEM_NAME = Arrays.stream(ReadSaveStep.values()).collect(Collectors.toMap(ReadSaveStep::getItemName, Function.identity()));
    }

    ReadSaveStep(String itemName, int step) {
        this.itemName = itemName;
        this.step = step;
    }

    public String getItemName() {
        return itemName;
    }

    public int getStep() {
        return step;
    }

    public static Optional<ReadSaveStep> getStep(String itemName) {
        return Optional.ofNullable(BY_ITEM_NAME.get(itemName));
    }
}
