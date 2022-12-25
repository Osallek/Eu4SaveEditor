package fr.osallek.eu4saveeditor.controller.propertyeditor.item;

import java.util.Optional;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.scene.layout.HBox;

public class HBoxItem<U> implements CustomItem<U> {

    private final String category;

    private final String name;

    private final HBox hBox;

    private final Integer colSpan;

    public HBoxItem(String category, HBox hBox) {
        this(category, null, hBox);
    }

    public HBoxItem(String category, String name, HBox hBox) {
        this(category, name, hBox, null);
    }

    public HBoxItem(String category, String name, HBox hBox, Integer colSpan) {
        this.category = category;
        this.name = name;
        this.hBox = hBox;
        this.colSpan = colSpan;
    }

    @Override
    public Class<?> getType() {
        return HBoxItem.class;
    }

    @Override
    public String category() {
        return this.category;
    }

    @Override
    public String name() {
        return this.name;
    }

    @Override
    public String description() {
        return null;
    }

    @Override
    public Object getValue() {
        return null;
    }

    @Override
    public void setValue(Object value) {
    }

    @Override
    public ObservableList<U> getChoices() {
        return null;
    }

    @Override
    public Optional<ObservableValue<? extends Object>> getObservableValue() {
        return Optional.empty();
    }

    @Override
    public int forceValueColSpan() {
        return this.colSpan == null ? 1 : this.colSpan;
    }

    public HBox gethBox() {
        return hBox;
    }
}
