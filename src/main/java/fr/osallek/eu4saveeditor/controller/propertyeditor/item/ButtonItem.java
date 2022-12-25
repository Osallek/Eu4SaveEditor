package fr.osallek.eu4saveeditor.controller.propertyeditor.item;

import java.util.Optional;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Pos;
import javafx.scene.control.Button;

public class ButtonItem implements CustomItem<String> {

    private final String category;

    private final String name;

    private final String description;

    private final Button button;

    private final Integer colSpan;

    public ButtonItem(String category, String name, String label) {
        this(category, name, null, label, null);
    }

    public ButtonItem(String category, String name, String label, Integer colSpan) {
        this(category, name, null, label, colSpan);
    }

    public ButtonItem(String category, String name, String description, String label) {
        this(category, name, description, label, null);
    }

    public ButtonItem(String category, String name, String description, String label, Integer colSpan) {
        this.category = category;
        this.name = name;
        this.description = description;
        this.button = new Button(label);
        this.colSpan = colSpan;
        this.button.setAlignment(Pos.CENTER);
        this.button.setMaxWidth(Double.MAX_VALUE);
    }

    @Override
    public Class<?> getType() {
        return ButtonItem.class;
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
    public BooleanProperty isEditable() {
        return new SimpleBooleanProperty(true);
    }

    @Override
    public int forceValueColSpan() {
        return this.colSpan == null ? 1 : this.colSpan;
    }

    public Button getButton() {
        return button;
    }

    public void setOnAction(EventHandler<ActionEvent> onAction) {
        this.button.setOnAction(onAction);
    }
}
