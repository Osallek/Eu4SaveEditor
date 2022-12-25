package fr.osallek.eu4saveeditor.controller.propertyeditor.item;

import fr.osallek.eu4saveeditor.controller.control.ClearableComboBox;
import java.util.Optional;
import java.util.function.Predicate;
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
import javafx.util.Callback;
import javafx.util.StringConverter;

public class ClearableComboBoxItem<U> implements CustomItem<U> {

    private final String category;

    private final String name;

    private final String description;

    private ObservableList<U> values;

    private U value;

    private final ClearableComboBox<U> comboBox;

    private final BooleanProperty editable;

    private EventHandler<ActionEvent> onAction;

    private StringConverter<U> converter;

    private Callback<ListView<U>, ListCell<U>> cellFactory;

    private Predicate<U> filter;

    private final BooleanProperty visible;

    public ClearableComboBoxItem(String category, String name, ObservableList<U> values, ClearableComboBox<U> comboBox) {
        this(category, name, values, null, null, comboBox);
    }

    public ClearableComboBoxItem(String category, String name, ObservableList<U> values, U value, ClearableComboBox<U> comboBox) {
        this(category, name, values, value, null, comboBox);
    }

    public ClearableComboBoxItem(String category, String name, ObservableList<U> values, U value, String description, ClearableComboBox<U> comboBox) {
        this(category, name, values, value, description, comboBox, new SimpleBooleanProperty(true), new SimpleBooleanProperty(true));
    }

    public ClearableComboBoxItem(String category, String name, ObservableList<U> values, U value, String description, ClearableComboBox<U> comboBox,
                                 BooleanProperty editable, BooleanProperty visible) {
        this.category = category;
        this.name = name;
        this.description = description;
        this.values = values;
        this.value = value;
        this.comboBox = comboBox;
        this.editable = editable;
        this.visible = visible;
    }

    @Override
    public Class<?> getType() {
        return ClearableComboBoxItem.class;
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
        return this.value;
    }

    @Override
    public void setValue(Object value) {
        this.value = ((U) value);
    }

    @Override
    public ObservableList<U> getChoices() {
        if (this.filter == null) {
            return this.values;
        } else {
            return this.values.filtered(this.filter);
        }
    }

    @Override
    public Optional<ObservableValue<? extends Object>> getObservableValue() {
        return Optional.empty();
    }

    @Override
    public BooleanProperty isEditable() {
        return this.editable;
    }

    @Override
    public BooleanProperty isVisible() {
        return this.visible;
    }

    public void setValues(ObservableList<U> values) {
        this.values = values;
    }

    public void setEditable(boolean editable) {
        this.editable.set(editable);
    }

    public ClearableComboBox<U> getComboBox() {
        return this.comboBox;
    }

    public U getSelectedValue() {
        return this.value;
    }

    public void select(U u) {
        this.comboBox.getComboBox().getSelectionModel().select(u);
    }

    public void setSupplier(Supplier<U> clearSupplier) {
        if (clearSupplier != null) {
            this.comboBox.setSupplier(clearSupplier);
        }
    }

    public EventHandler<ActionEvent> getOnAction() {
        return this.onAction;
    }

    public void setOnAction(EventHandler<ActionEvent> onAction) {
        this.onAction = onAction;
    }

    public ObjectProperty<U> valueProperty() {
        return this.comboBox.getComboBox().valueProperty();
    }

    public void setConverter(StringConverter<U> converter) {
        this.converter = converter;
    }

    public StringConverter<U> getConverter() {
        return this.converter;
    }

    public void setCellFactory(Callback<ListView<U>, ListCell<U>> cellFactory) {
        this.cellFactory = cellFactory;
    }

    public Callback<ListView<U>, ListCell<U>> getCellFactory() {
        return this.cellFactory;
    }

    public BooleanProperty editableProperty() {
        return this.editable;
    }

    public Predicate<U> getFilter() {
        return filter;
    }

    public void setFilter(Predicate<U> filter) {
        this.filter = filter;
    }
}
