package fr.osallek.eu4saveeditor.controller.control;

import javafx.collections.ObservableList;
import javafx.scene.control.ComboBox;

public class RequiredComboBox<T> extends ComboBox<T> {

    public RequiredComboBox() {
        super();
    }

    public RequiredComboBox(ObservableList<T> items) {
        super(items);
        valueProperty().addListener((observable, oldValue, newValue) -> {
            if (oldValue != null && newValue == null) {
                setValue(oldValue);
            }
        });
    }

}
