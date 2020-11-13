package com.osallek.eu4saveeditor.controller.control;

import javafx.scene.Node;
import javafx.scene.control.Spinner;
import javafx.scene.control.SpinnerValueFactory;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;

import java.util.function.Supplier;

public class ClearableSpinnerInt extends ClearableSpinner<Integer> {

    public ClearableSpinnerInt(int min, int max, int step) {
        this(min, max , 0, step, null);
    }

    public ClearableSpinnerInt(int min, int max, Integer value, int step, Supplier<Integer> clearSupplier) {
        this(min, max, value, step, clearSupplier, null);
    }

    public ClearableSpinnerInt(int min, int max, Integer value, int step, Supplier<Integer> clearSupplier, Node centerNode) {
        this.spinner = new Spinner<>(min, max, value == null ? 0 : value, step);
        this.spinner.setEditable(true);
        this.spinner.focusedProperty().addListener((observable, oldValue, newValue) -> {
            if (Boolean.FALSE.equals(newValue)) {
                spinner.increment(0); // won't change value, but will commit editor
            }
        });

        HBox.setHgrow(this.spinner, Priority.ALWAYS);
        fill(clearSupplier, centerNode);
    }

    @Override
    public void setValue(Integer value) {
        super.setValue(value == null ? 0 : value);
    }

    @Override
    public void setMax(Integer max) {
        ((SpinnerValueFactory.IntegerSpinnerValueFactory) this.spinner.getValueFactory()).setMax(max);
    }

    @Override
    public void setMin(Integer max) {
        ((SpinnerValueFactory.IntegerSpinnerValueFactory) this.spinner.getValueFactory()).setMax(max);
    }
}
