package com.osallek.eu4saveeditor.controller.propertyeditor.item;

import com.osallek.eu4saveeditor.i18n.SheetCategory;
import javafx.beans.value.ObservableValue;
import org.controlsfx.control.PropertySheet;

import java.util.Optional;

public class PropertySheetItem implements PropertySheet.Item {

    private final String category;

    private final PropertySheet propertySheet;

    public PropertySheetItem(SheetCategory category, PropertySheet propertySheet) {
        this(category.getForDefaultLocale(), propertySheet);
    }

    public PropertySheetItem(String category, PropertySheet propertySheet) {
        this.category = category;
        this.propertySheet = propertySheet;
    }

    @Override
    public Class<?> getType() {
        return PropertySheetItem.class;
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
    public Optional<ObservableValue<? extends Object>> getObservableValue() {
        return Optional.empty();
    }

    public PropertySheet getPropertySheet() {
        return propertySheet;
    }
}
