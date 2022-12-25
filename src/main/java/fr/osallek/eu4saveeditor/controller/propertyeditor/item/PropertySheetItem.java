package fr.osallek.eu4saveeditor.controller.propertyeditor.item;

import fr.osallek.eu4saveeditor.controller.pane.CustomPropertySheet;
import java.util.Optional;
import javafx.beans.value.ObservableValue;

public record PropertySheetItem(String category, CustomPropertySheet propertySheet) implements CustomPropertySheet.Item {

    @Override
    public Class<?> getType() {
        return PropertySheetItem.class;
    }

    @Override
    public String name() {
        return null;
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
    public Optional<ObservableValue<? extends Object>> getObservableValue() {
        return Optional.empty();
    }
}
