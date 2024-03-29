package fr.osallek.eu4saveeditor.controller.propertyeditor.item;

import fr.osallek.eu4saveeditor.controller.control.ClearableSliderInt;
import java.util.Optional;
import java.util.function.IntSupplier;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.util.converter.IntegerStringConverter;

public class ClearableSliderIntItem implements CustomItem<Integer> {

    private final String category;

    private final String name;

    private final ClearableSliderInt slider;

    private final BooleanProperty editable;

    public ClearableSliderIntItem(String category, String name, int min, int max) {
        this(category, name, min, max, null, null);
    }

    public ClearableSliderIntItem(String category, String name, int min, int max, Integer value, IntSupplier supplier) {
        this(category, name, new ClearableSliderInt(min, max, value, supplier, new IntegerStringConverter()), new SimpleBooleanProperty(true));
    }

    public ClearableSliderIntItem(String category, String name, ClearableSliderInt slider, BooleanProperty editable) {
        this.category = category;
        this.name = name;
        this.slider = slider;
        this.editable = editable;
    }

    @Override
    public Class<?> getType() {
        return ClearableSliderIntItem.class;
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
        return this.slider.getValue();
    }

    @Override
    public void setValue(Object value) {
        this.slider.setValue(value == null ? 0 : (int) value);
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

    public ClearableSliderInt getSlider() {
        return this.slider;
    }

    public int getIntValue() {
        return this.slider.getValue();
    }

    public void setSupplier(IntSupplier clearSupplier) {
        if (clearSupplier != null) {
            this.slider.setSupplier(clearSupplier);
        }
    }
}
