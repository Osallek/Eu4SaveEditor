package fr.osallek.eu4saveeditor.controller.control;

import javafx.beans.NamedArg;
import javafx.beans.property.DoubleProperty;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.scene.control.SpinnerValueFactory;
import javafx.util.converter.NumberStringConverter;

import java.math.BigDecimal;

public class NumberSpinnerValueFactory extends SpinnerValueFactory<Number> {

    /**
     * Constructs a new DoubleSpinnerValueFactory that sets the initial value to be equal to the min value, and a default {@code amountToStepBy} of one.
     *
     * @param min The minimum allowed double value for the Spinner.
     * @param max The maximum allowed double value for the Spinner.
     */
    public NumberSpinnerValueFactory(@NamedArg("min") double min,
                                     @NamedArg("max") double max) {
        this(min, max, min);
    }

    /**
     * Constructs a new DoubleSpinnerValueFactory with a default {@code amountToStepBy} of one.
     *
     * @param min          The minimum allowed double value for the Spinner.
     * @param max          The maximum allowed double value for the Spinner.
     * @param initialValue The value of the Spinner when first instantiated, must be within the bounds of the min and max arguments, or else the min value will
     *                     be used.
     */
    public NumberSpinnerValueFactory(@NamedArg("min") double min,
                                     @NamedArg("max") double max,
                                     @NamedArg("initialValue") double initialValue) {
        this(min, max, initialValue, 1);
    }

    /**
     * Constructs a new DoubleSpinnerValueFactory.
     *
     * @param min            The minimum allowed double value for the Spinner.
     * @param max            The maximum allowed double value for the Spinner.
     * @param initialValue   The value of the Spinner when first instantiated, must be within the bounds of the min and max arguments, or else the min value
     *                       will be used.
     * @param amountToStepBy The amount to increment or decrement by, per step.
     */
    public NumberSpinnerValueFactory(@NamedArg("min") double min,
                                     @NamedArg("max") double max,
                                     @NamedArg("initialValue") double initialValue,
                                     @NamedArg("amountToStepBy") double amountToStepBy) {
        setMin(min);
        setMax(max);
        setAmountToStepBy(amountToStepBy);
        setConverter(new NumberStringConverter("###.###"));

        valueProperty().addListener((o, oldValue, newValue) -> {
            if (newValue == null) {
                return;
            }

            // when the value is set, we need to react to ensure it is a
            // valid value (and if not, blow up appropriately)
            if (newValue.doubleValue() < getMin()) {
                setValue(getMin());
            } else if (newValue.doubleValue() > getMax()) {
                setValue(getMax());
            }
        });
        setValue(initialValue >= min && initialValue <= max ? initialValue : min);
    }


    /***********************************************************************
     *                                                                     *
     * Properties                                                          *
     *                                                                     *
     **********************************************************************/

    // --- min
    private final DoubleProperty min = new SimpleDoubleProperty(this, "min") {
        @Override
        protected void invalidated() {
            Double currentValue = this.getValue();
            if (currentValue == null) {
                return;
            }

            final double newMin = get();
            if (newMin > getMax()) {
                setMin(getMax());
                return;
            }

            if (currentValue < newMin) {
                this.setValue(newMin);
            }
        }
    };

    public final void setMin(double value) {
        min.set(value);
    }

    public final double getMin() {
        return min.get();
    }

    /**
     * Sets the minimum allowable value for this value factory
     *
     * @return the minimum allowable value for this value factory
     */
    public final DoubleProperty minProperty() {
        return min;
    }

    // --- max
    private final DoubleProperty max = new SimpleDoubleProperty(this, "max") {
        @Override
        protected void invalidated() {
            Double currentValue = this.getValue();
            if (currentValue == null) {
                return;
            }

            final double newMax = get();
            if (newMax < getMin()) {
                setMax(getMin());
                return;
            }

            if (currentValue > newMax) {
                this.setValue(newMax);
            }
        }
    };

    public final void setMax(double value) {
        max.set(value);
    }

    public final double getMax() {
        return max.get();
    }

    /**
     * Sets the maximum allowable value for this value factory
     *
     * @return the maximum allowable value for this value factory
     */
    public final DoubleProperty maxProperty() {
        return max;
    }

    // --- amountToStepBy
    private final DoubleProperty amountToStepBy = new SimpleDoubleProperty(this, "amountToStepBy");

    public final void setAmountToStepBy(double value) {
        amountToStepBy.set(value);
    }

    public final double getAmountToStepBy() {
        return amountToStepBy.get();
    }

    /**
     * Sets the amount to increment or decrement by, per step.
     *
     * @return the amount to increment or decrement by, per step
     */
    public final DoubleProperty amountToStepByProperty() {
        return amountToStepBy;
    }


    /**
     * {@inheritDoc}
     */
    @Override
    public void decrement(int steps) {
        final BigDecimal currentValue = BigDecimal.valueOf(getValue().doubleValue());
        final BigDecimal minBigDecimal = BigDecimal.valueOf(getMin());
        final BigDecimal maxBigDecimal = BigDecimal.valueOf(getMax());
        final BigDecimal amountToStepByBigDecimal = BigDecimal.valueOf(getAmountToStepBy());
        BigDecimal newValue = currentValue.subtract(amountToStepByBigDecimal.multiply(BigDecimal.valueOf(steps)));
        setValue(newValue.compareTo(minBigDecimal) >= 0 ? newValue.doubleValue() :
                 (isWrapAround() ? wrapValue(newValue, minBigDecimal, maxBigDecimal).doubleValue() : getMin()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void increment(int steps) {
        final BigDecimal currentValue = BigDecimal.valueOf(getValue().doubleValue());
        final BigDecimal minBigDecimal = BigDecimal.valueOf(getMin());
        final BigDecimal maxBigDecimal = BigDecimal.valueOf(getMax());
        final BigDecimal amountToStepByBigDecimal = BigDecimal.valueOf(getAmountToStepBy());
        BigDecimal newValue = currentValue.add(amountToStepByBigDecimal.multiply(BigDecimal.valueOf(steps)));
        setValue(newValue.compareTo(maxBigDecimal) <= 0 ? newValue.doubleValue() :
                 (isWrapAround() ? wrapValue(newValue, minBigDecimal, maxBigDecimal).doubleValue() : getMax()));
    }

    public static BigDecimal wrapValue(BigDecimal value, BigDecimal min, BigDecimal max) {
        if (max.doubleValue() == 0) {
            throw new RuntimeException();
        }

        // note that this wrap method differs from the others where we take the
        // difference - in this approach we wrap to the min or max - it feels better
        // to go from 1 to 0, rather than 1 to 0.05 (where max is 1 and step is 0.05).
        if (value.compareTo(min) < 0) {
            return max;
        } else if (value.compareTo(max) > 0) {
            return min;
        }
        return value;
    }
}
