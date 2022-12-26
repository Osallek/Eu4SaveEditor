package fr.osallek.eu4saveeditor.common;

import java.util.Arrays;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

public enum ReadSaveStep {
    GAME(null, 0),
    META_DATA("players_countries", 50),
    PROVINCES("provinces", 55),
    COUNTRIES("countries", 65),
    WARS("active_advisors", 95),
    FINISHED("idea_dates", 100);

    public final String itemName;

    public final int progress;

    private static final Map<String, ReadSaveStep> BY_ITEM_NAME;

    public static final int NB_STEPS;

    static {
        NB_STEPS = ReadSaveStep.values().length;
        BY_ITEM_NAME = Arrays.stream(ReadSaveStep.values())
                             .filter(step -> step.itemName != null)
                             .collect(Collectors.toMap(ReadSaveStep::getItemName, Function.identity()));
    }

    ReadSaveStep(String itemName, int progress) {
        this.itemName = itemName;
        this.progress = progress;
    }

    public ReadSaveStep next() {
        if (ordinal() >= ReadSaveStep.values().length - 1) {
            return ReadSaveStep.values()[0];
        } else {
            return ReadSaveStep.values()[ordinal() + 1];
        }
    }

    public String getItemName() {
        return itemName;
    }

    public int getProgress() {
        return progress;
    }

    public static Optional<ReadSaveStep> getStep(String itemName) {
        return Optional.ofNullable(BY_ITEM_NAME.get(itemName));
    }
}
