package fr.osallek.eu4saveeditor.controller.propertyeditor.item;

import fr.osallek.eu4saveeditor.controller.control.ClearableCheckComboBox;
import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;
import javafx.util.StringConverter;

public class ClearableCheckComboBoxItem<U> implements CustomItem<U> {

    private final String category;

    private final String name;

    private final ObservableList<U> values;

    private ObservableList<U> selectedValues;

    private final ClearableCheckComboBox<U> checkComboBox;

    private final BooleanProperty editable;

    private EventHandler<ActionEvent> onAction;

    private StringConverter<U> converter;

    private Callback<ListView<U>, ListCell<U>> cellFactory;

    public ClearableCheckComboBoxItem(String category, String name, ObservableList<U> values, ClearableCheckComboBox<U> checkComboBox) {
        this(category, name, values, null, checkComboBox, new SimpleBooleanProperty(true));
    }

    public ClearableCheckComboBoxItem(String category, String name, ObservableList<U> values, ObservableList<U> selectedValues,
                                      ClearableCheckComboBox<U> checkComboBox, BooleanProperty editable) {
        this.category = category;
        this.name = name;
        this.values = values;
        this.selectedValues = selectedValues;
        this.checkComboBox = checkComboBox;
        this.editable = editable;
    }

    @Override
    public Class<?> getType() {
        return ClearableCheckComboBoxItem.class;
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
        return this.selectedValues;
    }

    @Override
    public void setValue(Object value) {
        this.selectedValues = ((ObservableList<U>) value);
    }

    @Override
    public ObservableList<U> getChoices() {
        return this.values;
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

    public ClearableCheckComboBox<U> getCheckComboBox() {
        return this.checkComboBox;
    }

    public ObservableList<U> getSelectedValues() {
        return this.selectedValues;
    }

    public void check(U u) {
        this.checkComboBox.check(u);
    }

    public void clearCheck(U u) {
        this.checkComboBox.clearCheck(u);
    }

    public void setSupplier(Supplier<List<U>> clearSupplier) {
        if (clearSupplier != null) {
            this.checkComboBox.setSupplier(clearSupplier);
        }
    }

    public EventHandler<ActionEvent> getOnAction() {
        return this.onAction;
    }

    public void setOnAction(EventHandler<ActionEvent> onAction) {
        this.onAction = onAction;
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
}
