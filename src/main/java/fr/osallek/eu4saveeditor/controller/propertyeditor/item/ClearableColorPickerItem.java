package fr.osallek.eu4saveeditor.controller.propertyeditor.item;

import fr.osallek.eu4saveeditor.controller.control.ClearableColorPicker;
import java.util.Optional;
import java.util.function.Supplier;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.paint.Color;
import javafx.util.Callback;
import javafx.util.StringConverter;

public class ClearableColorPickerItem implements CustomItem<Color> {

    private final String category;

    private final String name;

    private final String description;

    private final ClearableColorPicker colorPicker;

    private final BooleanProperty editable;

    private EventHandler<ActionEvent> onAction;

    private StringConverter<Color> converter;

    private Callback<ListView<Color>, ListCell<Color>> cellFactory;

    public ClearableColorPickerItem(String category, String name, ClearableColorPicker colorPicker) {
        this(category, name, null, colorPicker, new SimpleBooleanProperty(true));
    }

    public ClearableColorPickerItem(String category, String name, String description, ClearableColorPicker colorPicker) {
        this(category, name, description, colorPicker, new SimpleBooleanProperty(true));
    }

    public ClearableColorPickerItem(String category, String name, String description, ClearableColorPicker colorPicker, BooleanProperty editable) {
        this.category = category;
        this.name = name;
        this.description = description;
        this.colorPicker = colorPicker;
        this.editable = editable;
    }

    @Override
    public Class<?> getType() {
        return ClearableColorPickerItem.class;
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
        return this.colorPicker.getSelectedValue();
    }

    @Override
    public void setValue(Object value) {
        this.colorPicker.select((Color) value);
    }

    @Override
    public ObservableList<Color> getChoices() {
        return null;
    }

    @Override
    public Optional<ObservableValue<? extends Object>> getObservableValue() {
        return Optional.of(this.colorPicker.getColorPicker().valueProperty());
    }

    @Override
    public BooleanProperty isEditable() {
        return this.editable;
    }

    public void setEditable(boolean editable) {
        this.editable.set(editable);
    }

    public ClearableColorPicker getColorPicker() {
        return this.colorPicker;
    }

    public Color getSelectedValue() {
        return this.colorPicker.getSelectedValue();
    }

    public void setSupplier(Supplier<Color> clearSupplier) {
        if (clearSupplier != null) {
            this.colorPicker.setSupplier(clearSupplier);
        }
    }

    public EventHandler<ActionEvent> getOnAction() {
        return this.onAction;
    }

    public void setOnAction(EventHandler<ActionEvent> onAction) {
        this.onAction = onAction;
    }

    public ObjectProperty<Color> valueProperty() {
        return this.colorPicker.valueProperty();
    }

    public void setConverter(StringConverter<Color> converter) {
        this.converter = converter;
    }

    public StringConverter<Color> getConverter() {
        return this.converter;
    }

    public void setCellFactory(Callback<ListView<Color>, ListCell<Color>> cellFactory) {
        this.cellFactory = cellFactory;
    }

    public Callback<ListView<Color>, ListCell<Color>> getCellFactory() {
        return this.cellFactory;
    }
}
