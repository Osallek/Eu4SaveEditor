package com.osallek.eu4saveeditor.controller.propertyeditor.item;

import com.osallek.eu4saveeditor.controller.mapview.SheetCategory;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.scene.layout.HBox;

import java.util.Optional;

public class HBoxItem<U> implements CustomItem<U> {

    private final String category;

    private final HBox hBox;

    public HBoxItem(SheetCategory category, HBox hBox) {
        this.category = category.getForDefaultLocale();
        this.hBox = hBox;
    }

    @Override
    public Class<?> getType() {
        return HBoxItem.class;
    }

    @Override
    public String getCategory() {
        return this.category;
    }

    @Override
    public String getName() {
        return null;
    }

    @Override
    public String getDescription() {
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

    public HBox gethBox() {
        return hBox;
    }
}
