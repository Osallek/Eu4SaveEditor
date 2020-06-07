package com.osallek.eu4saveeditor.controller.propertyeditor;

import com.osallek.eu4saveeditor.controller.control.ClearableCheckComboBox;
import com.osallek.eu4saveeditor.controller.control.ClearableComboBox;
import com.osallek.eu4saveeditor.controller.control.ClearableSlider;
import com.osallek.eu4saveeditor.controller.control.ClearableSpinner;
import com.osallek.eu4saveeditor.controller.control.SelectableGridView;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ButtonItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.CheckComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableCheckComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSliderItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSpinnerItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableTextItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.HBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.SelectableGridViewItem;
import javafx.beans.property.ListProperty;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SetProperty;
import javafx.beans.property.SimpleListProperty;
import javafx.beans.property.SimpleSetProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.collections.ObservableSet;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import org.controlsfx.control.CheckComboBox;
import org.controlsfx.property.editor.AbstractPropertyEditor;
import org.controlsfx.property.editor.PropertyEditor;

public class CustomEditors {

    private CustomEditors() {}

    public static PropertyEditor<String> createClearableLabeledTextEditor(ClearableTextItem property) {

        return new AbstractPropertyEditor<String, TextField>(property, property.getTextField()) {

            @Override
            protected StringProperty getObservableValue() {
                return getEditor().textProperty();
            }

            @Override
            public void setValue(String value) {
                getEditor().setText(value);
            }
        };
    }

    public static <T> PropertyEditor<T> createClearableSpinnerEditor(ClearableSpinnerItem<T> property) {

        return new AbstractPropertyEditor<T, ClearableSpinner<T>>(property, property.getSpinner()) {

            @Override
            protected ObservableValue<T> getObservableValue() {
                return getEditor().getSpinner().valueProperty();
            }

            @Override
            public void setValue(T value) {
                getEditor().setValue(value);
            }
        };
    }

    public static PropertyEditor<Double> createClearableSliderEditor(ClearableSliderItem property) {

        return new AbstractPropertyEditor<Double, ClearableSlider>(property, property.getSlider()) {

            @Override
            protected ObservableValue<Double> getObservableValue() {
                return getEditor().getObservableValue();
            }

            @Override
            public void setValue(Double value) {
                getEditor().setValue(value);
            }
        };
    }

    public static <T> PropertyEditor<T> createCustomChoiceEditor(ComboBoxItem<T> comboBoxItem) {

        return new AbstractPropertyEditor<T, ComboBox<T>>(comboBoxItem, comboBoxItem.getComboBox()) {
            {
                getEditor().setItems(comboBoxItem.getChoices());

                if (comboBoxItem.getOnAction() != null) {
                    getEditor().setOnAction(comboBoxItem.getOnAction());
                }

                if (comboBoxItem.getConverter() != null) {
                    getEditor().setConverter(comboBoxItem.getConverter());
                }

                if (comboBoxItem.getCellFactory() != null) {
                    getEditor().setCellFactory(comboBoxItem.getCellFactory());
                }
            }

            @Override
            protected ObservableValue<T> getObservableValue() {
                return getEditor().getSelectionModel().selectedItemProperty();
            }

            @Override
            public void setValue(T value) {
                getEditor().getSelectionModel().select(value);
            }
        };
    }

    public static <T> PropertyEditor<T> createClearableComboBoxEditor(ClearableComboBoxItem<T> clearableComboBoxItem) {

        return new AbstractPropertyEditor<T, ClearableComboBox<T>>(clearableComboBoxItem, clearableComboBoxItem.getComboBox()) {
            {
                getEditor().setItems(clearableComboBoxItem.getChoices());

                if (clearableComboBoxItem.getOnAction() != null) {
                    getEditor().setOnAction(clearableComboBoxItem.getOnAction());
                }

                if (clearableComboBoxItem.getConverter() != null) {
                    getEditor().setConverter(clearableComboBoxItem.getConverter());
                }

                if (clearableComboBoxItem.getCellFactory() != null) {
                    getEditor().setCellFactory(clearableComboBoxItem.getCellFactory());
                }
            }

            @Override
            protected ObservableValue<T> getObservableValue() {
                return getEditor().getSelectionModel().selectedItemProperty();
            }

            @Override
            public void setValue(T value) {
                getEditor().getSelectionModel().select(value);
            }
        };
    }

    public static <T> PropertyEditor<ObservableList<T>> createCheckComboBoxEditor(CheckComboBoxItem<T> comboBoxItem) {

        return new AbstractPropertyEditor<ObservableList<T>, CheckComboBox<T>>(comboBoxItem, comboBoxItem.getCheckComboBox()) {

            private ListProperty<T> list;

            {
                getEditor().getItems().setAll(comboBoxItem.getChoices());

                if (comboBoxItem.getConverter() != null) {
                    getEditor().setConverter(comboBoxItem.getConverter());
                }
            }

            @Override
            protected ListProperty<T> getObservableValue() {
                if (list == null) {
                    list = new SimpleListProperty<>(getEditor().getCheckModel().getCheckedItems());
                }
                return list;
            }

            @Override
            public void setValue(ObservableList<T> checked) {
                checked.forEach(getEditor().getCheckModel()::check);
            }
        };
    }

    public static <T> PropertyEditor<ObservableList<T>> createClearableCheckComboBoxEditor(ClearableCheckComboBoxItem<T> comboBoxItem) {

        return new AbstractPropertyEditor<ObservableList<T>, ClearableCheckComboBox<T>>(comboBoxItem, comboBoxItem.getCheckComboBox()) {

            private ListProperty<T> list;

            {
                if (getEditor().getItems() != null && comboBoxItem.getChoices() != null) {
                    getEditor().getItems().setAll(comboBoxItem.getChoices());
                }

                if (comboBoxItem.getConverter() != null) {
                    getEditor().setConverter(comboBoxItem.getConverter());
                }
            }

            @Override
            protected ListProperty<T> getObservableValue() {
                if (list == null) {
                    list = new SimpleListProperty<>(getEditor().getSelectedValues());
                }

                return list;
            }

            @Override
            public void setValue(ObservableList<T> checked) {
                getEditor().clearChecks();
                checked.forEach(getEditor()::check);
            }
        };
    }

    public static <T> PropertyEditor<ObservableSet<T>> createSelectableGridViewEditor(SelectableGridViewItem<T> selectableGridViewItem) {

        return new AbstractPropertyEditor<ObservableSet<T>, SelectableGridView<T>>(selectableGridViewItem, selectableGridViewItem
                .getSelectableGridView()) {

            private SetProperty<T> list = new SimpleSetProperty<>();

            @Override
            protected SetProperty<T> getObservableValue() {
                if (list == null) {
                    list = new SimpleSetProperty<>(getEditor().getSelectedItems());
                }

                return list;
            }

            @Override
            public void setValue(ObservableSet<T> value) {
                if (value != null) {
                    value.forEach(getEditor()::select);
                }
            }
        };
    }

    public static <T> PropertyEditor<T> createHBox(HBoxItem<T> hBoxItem) {

        return new AbstractPropertyEditor<T, HBox>(hBoxItem, hBoxItem.gethBox()) {

            @Override
            protected ObservableValue<T> getObservableValue() {
                return new ReadOnlyObjectWrapper<>(hBoxItem.gethBox(), "value");
            }

            @Override
            public void setValue(T value) {
            }
        };
    }

    public static PropertyEditor<String> createButton(ButtonItem buttonItem) {

        return new AbstractPropertyEditor<String, Button>(buttonItem, buttonItem.getButton()) {

            @Override
            protected ObservableValue<String> getObservableValue() {
                return new ReadOnlyObjectWrapper<>(buttonItem.getButton(), "value");
            }

            @Override
            public void setValue(String value) {
            }
        };
    }
}
