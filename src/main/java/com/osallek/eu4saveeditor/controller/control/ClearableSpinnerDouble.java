package com.osallek.eu4saveeditor.controller.control;

import javafx.scene.control.Spinner;
import javafx.scene.control.SpinnerValueFactory;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.util.StringConverter;

import java.util.function.Supplier;

public class ClearableSpinnerDouble extends ClearableSpinner<Double> {

    public ClearableSpinnerDouble(double min, double max, double step) {
        this(min, max , 0, step, null);
    }

    public ClearableSpinnerDouble(double min, double max, double value, double step, Supplier<Double> clearSupplier) {
        this.spinner = new Spinner<>(min, max, value, step);
        this.spinner.setEditable(true);
        this.spinner.focusedProperty().addListener((observable, oldValue, newValue) -> {
            //Force commit
            if (Boolean.FALSE.equals(newValue)) {
                if (!this.spinner.isEditable()) {
                    return;
                }

                String text = this.spinner.getEditor().getText();
                SpinnerValueFactory<Double> valueFactory = this.spinner.getValueFactory();
                if (valueFactory != null) {
                    StringConverter<Double> converter = valueFactory.getConverter();
                    if (converter != null) {
                        Double v = converter.fromString(text);
                        valueFactory.setValue(v);
                    }
                }
            }
        });

        HBox.setHgrow(this.spinner, Priority.ALWAYS);
        fill(clearSupplier);
    }

    @Override
    public void setValue(Double value) {
        super.setValue(value == null ? 0 : value);
    }
}
