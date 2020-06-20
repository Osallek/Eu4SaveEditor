package com.osallek.eu4saveeditor.controller.propertyeditor.item;

import com.osallek.eu4saveeditor.controller.control.CustomClearableTextField;
import com.osallek.eu4saveeditor.i18n.SheetCategory;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import org.controlsfx.control.textfield.CustomTextField;

import java.util.Optional;
import java.util.function.Supplier;

public class ClearableTextItem implements CustomItem<Void> {

    private final String category;

    private final String name;

    private final CustomTextField textField;

    private String value;

    private Supplier<String> supplier;

    private boolean editable;

    public ClearableTextItem(SheetCategory category, String name) {
        this(category, name, null, null, true);
    }

    public ClearableTextItem(SheetCategory category, String name, String value, Supplier<String> clearSupplier) {
        this(category, name, value, clearSupplier, true);
    }

    public ClearableTextItem(SheetCategory category, String name, String value, Supplier<String> clearSupplier, boolean editable) {
        this.category = category.getForDefaultLocale();
        this.name = name;
        this.textField = CustomClearableTextField.createClearableTextField(clearSupplier);
        this.value = value;
        this.supplier = clearSupplier;
        this.editable = editable;
    }

    @Override
    public Class<?> getType() {
        return ClearableTextItem.class;
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
        return this.name;
    }

    @Override
    public Object getValue() {
        return this.value;
    }

    @Override
    public void setValue(Object value) {
        this.value = ((String) value);
    }

    @Override
    public ObservableList<Void> getChoices() {
        return null;
    }

    @Override
    public Optional<ObservableValue<? extends Object>> getObservableValue() {
        return Optional.empty();
    }

    @Override
    public boolean isEditable() {
        return this.editable;
    }

    public void setEditable(boolean editable) {
        this.editable = editable;
    }

    public void setSupplier(Supplier<String> supplier) {
        this.supplier = supplier;
        this.textField.getRight().setOnMouseReleased(e -> this.textField.setText(this.supplier.get()));
    }

    public Supplier<String> getSupplier() {
        return supplier;
    }

    public String getText() {
        return this.value;
    }

    public CustomTextField getTextField() {
        return textField;
    }
}
