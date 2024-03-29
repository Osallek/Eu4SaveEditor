package fr.osallek.eu4saveeditor.controller.propertyeditor.item;

import fr.osallek.eu4saveeditor.controller.control.CustomClearableTextField;
import java.util.Optional;
import java.util.function.Supplier;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import org.controlsfx.control.textfield.CustomTextField;

public class ClearableTextItem implements CustomItem<Void> {

    private final String category;

    private final String name;

    private final CustomTextField textField;

    private String value;

    private Supplier<String> supplier;

    private final BooleanProperty editable;

    public ClearableTextItem(String category, String name) {
        this(category, name, null, null);
    }

    public ClearableTextItem(String category, String name, String value, Supplier<String> clearSupplier) {
        this(category, name, value, clearSupplier, new SimpleBooleanProperty(true));
    }

    public ClearableTextItem(String category, String name, String value, Supplier<String> clearSupplier, BooleanProperty editable) {
        this.category = category;
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
    public String category() {
        return this.category;
    }

    @Override
    public String name() {
        return this.name;
    }

    @Override
    public String description() {
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
    public BooleanProperty isEditable() {
        return this.editable;
    }

    public void setEditable(boolean editable) {
        this.editable.set(editable);
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
