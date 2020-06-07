package com.osallek.eu4saveeditor.controller.propertyeditor.item;

import com.osallek.eu4saveeditor.controller.mapview.SheetCategory;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.text.TextAlignment;

import java.util.Optional;

public class ButtonItem implements CustomItem<String> {

    private final String category;

    private final String name;

    private final String description;

    private final Button button;

    public ButtonItem(SheetCategory category, String name, String label) {
        this(category, name, null, label);
    }

    public ButtonItem(SheetCategory category, String name, String description, String label) {
        this.category = category.getForDefaultLocale();
        this.name = name;
        this.description = description;
        this.button = new Button(label);
        this.button.setAlignment(Pos.CENTER);
    }

    @Override
    public Class<?> getType() {
        return ButtonItem.class;
    }

    @Override
    public String getCategory() {
        return this.category;
    }

    @Override
    public String getName() {
        return this.name;
    }

    @Override
    public String getDescription() {
        return this.description;
    }

    @Override
    public Object getValue() {
        return this.button.getText();
    }

    @Override
    public void setValue(Object value) {
        this.button.setText((String) value);
    }

    @Override
    public ObservableList<String> getChoices() {
        return null;
    }

    @Override
    public Optional<ObservableValue<? extends Object>> getObservableValue() {
        return Optional.empty();
    }

    @Override
    public boolean isEditable() {
        return true;
    }

    public Button getButton() {
        return button;
    }

    public void setOnAction(EventHandler<ActionEvent> onAction) {
        this.button.setOnAction(onAction);
    }
}
