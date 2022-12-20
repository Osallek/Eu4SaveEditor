package fr.osallek.eu4saveeditor.controller.propertyeditor.item;

import java.util.Optional;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.scene.text.Text;

public class TextItem implements CustomItem<String> {

    private final String category;

    private final String name;

    private final String description;

    private final Text text;

    public TextItem(String category, String name, String description, Text text) {
        this.category = category;
        this.name = name;
        this.description = description;
        this.text = text;
    }

    @Override
    public Class<?> getType() {
        return TextItem.class;
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

    public Text getText() {
        return text;
    }
}
