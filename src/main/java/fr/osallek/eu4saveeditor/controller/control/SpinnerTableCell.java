package fr.osallek.eu4saveeditor.controller.control;

import java.util.function.Function;
import javafx.scene.control.Spinner;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.util.Callback;
import javafx.util.StringConverter;
import org.apache.commons.lang3.BooleanUtils;

public class SpinnerTableCell<S, T> extends TableCell<S, T> {

    public static <S> Callback<TableColumn<S, Integer>, TableCell<S, Integer>> forTableColumn(int min, int max, int value,
                                                                                              StringConverter<Integer> converter) {
        return forTableColumn(min, max, value, converter, null);
    }

    public static <S> Callback<TableColumn<S, Integer>, TableCell<S, Integer>> forTableColumn(int min, int max, int value,
                                                                                              StringConverter<Integer> converter,
                                                                                              Function<S, Boolean> disableSupplier) {
        return param -> new SpinnerTableCell<>(min, max, value, converter, disableSupplier);
    }

    public static <S> Callback<TableColumn<S, Double>, TableCell<S, Double>> forTableColumn(double min, double max, double value,
                                                                                            StringConverter<Double> converter) {
        return forTableColumn(min, max, value, converter, null);
    }

    public static <S> Callback<TableColumn<S, Double>, TableCell<S, Double>> forTableColumn(double min, double max, double value,
                                                                                            StringConverter<Double> converter,
                                                                                            Function<S, Boolean> disableSupplier) {
        return param -> new SpinnerTableCell<>(min, max, value, converter, disableSupplier);
    }

    private final Spinner<T> spinner;

    private final StringConverter<T> converter;

    private final Function<S, Boolean> disableSupplier;

    public SpinnerTableCell(int min, int max, int value, StringConverter<T> converter, Function<S, Boolean> disableSupplier) {
        this(new Spinner<>(min, max, value), converter, disableSupplier);
    }

    public SpinnerTableCell(double min, double max, double value, StringConverter<T> converter, Function<S, Boolean> disableSupplier) {
        this(new Spinner<>(min, max, value), converter, disableSupplier);
    }

    public SpinnerTableCell(Spinner<T> spinner, StringConverter<T> converter, Function<S, Boolean> disableSupplier) {
        this.spinner = spinner;
        this.disableSupplier = disableSupplier;
        this.spinner.setMaxWidth(Double.MAX_VALUE);
        this.spinner.setEditable(true);
        this.converter = converter;

        this.spinner.focusedProperty().addListener((obs, wasFocused, isNowFocused) -> {
            if (BooleanUtils.isTrue(wasFocused) && BooleanUtils.isFalse(isNowFocused)) {
                commitEdit(this.spinner.getValue());
                setText(this.converter.toString(getItem()));
                setGraphic(null);
            }
        });
    }

    @Override
    public void startEdit() {
        if (!isEditable() || (getTableView() != null && !getTableView().isEditable()) || (getTableColumn() != null && !getTableColumn().isEditable())) {
            return;
        }

        super.startEdit();
        this.spinner.getValueFactory().setValue(getItem());
        setText(null);
        setGraphic(this.spinner);
    }

    @Override
    public void cancelEdit() {
        super.cancelEdit();

        setText(this.converter.toString(getItem()));
        setGraphic(null);
    }

    @Override
    public void updateItem(T item, boolean empty) {
        super.updateItem(item, empty);

        if (isEmpty() || item == null) {
            setText(null);
            setGraphic(null);
        } else {
            this.spinner.getValueFactory().setValue(item);

            if (this.disableSupplier != null) {
                this.spinner.disableProperty().setValue(this.disableSupplier.apply(getTableRow().getItem()));
                this.setEditable(!this.disableSupplier.apply(getTableRow().getItem()));
            }

            if (isEditing()) {
                setText(null);
                setGraphic(this.spinner);
            } else {
                setText(this.converter.toString(this.spinner.getValue()));
                setGraphic(null);
            }
        }
    }
}
