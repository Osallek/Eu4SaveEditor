package com.osallek.eu4saveeditor.controller.propertyeditor;

import com.osallek.eu4saveeditor.controller.item.CustomClearableTextField;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.CheckComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableTextItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ComboBoxItem;
import javafx.beans.property.ListProperty;
import javafx.beans.property.SimpleListProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.scene.control.ComboBox;
import javafx.scene.control.TextField;
import org.controlsfx.control.CheckComboBox;
import org.controlsfx.property.editor.AbstractPropertyEditor;
import org.controlsfx.property.editor.PropertyEditor;

public class CustomEditors {

    private CustomEditors() {}

    public static PropertyEditor<String> createClearableLabeledTextEditor(ClearableTextItem property) {

        return new AbstractPropertyEditor<String, TextField>(property, CustomClearableTextField.createClearableTextField(property.getSupplier())) {

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
}
