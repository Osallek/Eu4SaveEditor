package fr.osallek.eu4saveeditor.controller.propertyeditor.item;

import java.util.Optional;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.scene.text.Text;

public record TextItem(String category, String name, String description, Text text) implements CustomItem<String> {

    @Override
    public Class<?> getType() {
        return TextItem.class;
    }

    @Override
    public Object getValue() {
        return this.text.getText();
    }

    @Override
    public void setValue(Object value) {
        this.text.setText((String) value);
    }

    @Override
    public ObservableList<String> getChoices() {
        return null;
    }

    @Override
    public Optional<ObservableValue<? extends Object>> getObservableValue() {
        return Optional.empty();
    }
}
