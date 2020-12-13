package fr.osallek.eu4saveeditor.controller.propertyeditor.item;

import fr.osallek.eu4saveeditor.controller.control.ClearableSpinner;
import fr.osallek.eu4saveeditor.i18n.SheetCategory;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import java.util.Optional;
import java.util.function.Supplier;

public class ClearableSpinnerItem<T> implements CustomItem<Integer> {

    private final String category;

    private final String name;

    private final ClearableSpinner<T> spinner;

    private final BooleanProperty editable;

    public ClearableSpinnerItem(SheetCategory category, String name, ClearableSpinner<T> spinner) {
        this(category.getForDefaultLocale(), name, spinner);
    }


    public ClearableSpinnerItem(String category, String name, ClearableSpinner<T> spinner) {
        this(category, name, spinner, new SimpleBooleanProperty(true));
    }

    public ClearableSpinnerItem(SheetCategory category, String name, ClearableSpinner<T> spinner, BooleanProperty editable) {
        this(category.getForDefaultLocale(), name, spinner, editable);
    }

    public ClearableSpinnerItem(String category, String name, ClearableSpinner<T> spinner, BooleanProperty editable) {
        this.category = category;
        this.name = name;
        this.spinner = spinner;
        this.editable = editable;
    }

    @Override
    public Class<?> getType() {
        return ClearableSpinnerItem.class;
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
        return this.spinner.getValue();
    }

    @Override
    public void setValue(Object value) {
        this.spinner.setValue((T) value);
    }

    @Override
    public ObservableList<Integer> getChoices() {
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

    public ClearableSpinner<T> getSpinner() {
        return this.spinner;
    }

    public T getTrueValue() {
        return this.spinner.getValue();
    }

    public ObjectProperty<T> valueProperty() {
        return this.spinner.getSpinner().getValueFactory().valueProperty();
    }

    public void setSupplier(Supplier<T> clearSupplier) {
        if (clearSupplier != null) {
            this.spinner.setSupplier(clearSupplier);
        }
    }

    public void setMax(T max) {
        this.spinner.setMax(max);
    }

    public void setMin(T max) {
        this.spinner.setMax(max);
    }
}
