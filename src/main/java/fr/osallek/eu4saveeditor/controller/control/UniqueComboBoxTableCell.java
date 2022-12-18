package fr.osallek.eu4saveeditor.controller.control;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.ListChangeListener;
import javafx.collections.MapChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;
import javafx.collections.transformation.SortedList;
import javafx.scene.control.Cell;
import javafx.scene.control.ComboBox;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.util.Callback;
import javafx.util.StringConverter;

public class UniqueComboBoxTableCell<S, T> extends TableCell<S, T> {

    public static <S, T> Callback<TableColumn<S, T>, TableCell<S, T>> forTableColumn(StringConverter<T> converter,
                                                                                     ObservableMap<S, ObservableList<T>> source,
                                                                                     Comparator<T> comparator,
                                                                                     ObservableList<S> items,
                                                                                     Supplier<ObservableList<T>> supplier) {
        return forTableColumn(converter, source, comparator, ComboBox::new, items, supplier);
    }

    public static <S, T> Callback<TableColumn<S, T>, TableCell<S, T>> forTableColumn(StringConverter<T> converter,
                                                                                     ObservableMap<S, ObservableList<T>> source,
                                                                                     Comparator<T> comparator,
                                                                                     Function<ObservableList<T>, ComboBox<T>> boxSupplier,
                                                                                     ObservableList<S> items,
                                                                                     Supplier<ObservableList<T>> supplier) {
        return list -> new UniqueComboBoxTableCell<>(converter, source, comparator, boxSupplier, items, supplier);
    }

    private final ObservableMap<S, ObservableList<T>> source;

    private final Map<S, SortedList<T>> sorted = new HashMap<>();

    private final Comparator<T> comparator;

    private final Function<ObservableList<T>, ComboBox<T>> boxSupplier;

    private final Supplier<ObservableList<T>> supplier;

    private ComboBox<T> comboBox;

    public UniqueComboBoxTableCell(StringConverter<T> converter, ObservableMap<S, ObservableList<T>> source, Comparator<T> comparator,
                                   Function<ObservableList<T>, ComboBox<T>> boxSupplier, ObservableList<S> items, Supplier<ObservableList<T>> supplier) {
        this.getStyleClass().add("combo-box-table-cell");
        this.source = source;
        this.comparator = comparator;
        this.boxSupplier = boxSupplier;
        this.supplier = supplier;
        setConverter(converter);

        this.source.forEach((s, ts) -> this.sorted.put(s, ts.sorted(this.comparator)));

        this.source.addListener((MapChangeListener<? super S, ? super ObservableList<T>>) change -> {
            if (change.wasAdded()) {
                this.sorted.put(change.getKey(), change.getValueAdded().sorted(this.comparator));
            } else if (change.wasRemoved()) {
                this.sorted.remove(change.getKey());
            }
        });

        items.addListener((ListChangeListener<? super S>) change -> {
            while (change.next()) {
                if (change.wasAdded()) {
                    change.getAddedSubList().forEach(s -> this.source.put(s, this.supplier.get()));
                } else if (change.wasRemoved()) {
                    change.getRemoved().forEach(this.source::remove);
                }
            }
        });
    }

    private final ObjectProperty<StringConverter<T>> converter = new SimpleObjectProperty<>(this, "converter");

    public final ObjectProperty<StringConverter<T>> converterProperty() {
        return converter;
    }

    public final void setConverter(StringConverter<T> value) {
        converterProperty().set(value);
    }

    public final StringConverter<T> getConverter() {
        return converterProperty().get();
    }

    private final BooleanProperty comboBoxEditable = new SimpleBooleanProperty(this, "comboBoxEditable");

    public final BooleanProperty comboBoxEditableProperty() {
        return comboBoxEditable;
    }

    public final void setComboBoxEditable(boolean value) {
        comboBoxEditableProperty().set(value);
    }

    public final boolean isComboBoxEditable() {
        return comboBoxEditableProperty().get();
    }

    public Map<S, ObservableList<T>> getSource() {
        return source;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void startEdit() {
        if (!isEditable() || !getTableView().isEditable() || !getTableColumn().isEditable()) {
            return;
        }

        if (this.comboBox == null) {
            this.comboBox = createComboBox(this, this.boxSupplier, this.sorted.get(getTableRow().getItem()), converterProperty());
            this.comboBox.editableProperty().bind(comboBoxEditableProperty());
        }

        this.comboBox.getSelectionModel().select(getItem());

        super.startEdit();
        setText(null);
        setGraphic(this.comboBox);
    }

    @Override
    public void commitEdit(T newValue) {
        this.source.forEach((s, ts) -> {
            if (!ts.contains(getItem())) {
                ts.add(getItem());
            }

            if (!s.equals(getTableView().getItems().get(getTableRow().getIndex()))) {
                ts.remove(newValue);
            }
        });

        super.commitEdit(newValue);
    }

    @Override
    public void cancelEdit() {
        super.cancelEdit();

        setText(getConverter().toString(getItem()));
        setGraphic(null);
    }

    @Override
    public void updateItem(T item, boolean empty) {
        super.updateItem(item, empty);

        if (this.isEmpty()) {
            this.setText(null);
            this.setGraphic(null);
        } else {
            if (this.isEditing()) {
                this.comboBox.getSelectionModel().select(this.getItem());
                this.setText(null);
                this.setGraphic(this.comboBox);
            } else {
                this.setText(getItemText(this, getConverter()));
                this.setGraphic(null);
            }
        }
    }

    private static <T> String getItemText(Cell<T> cell, StringConverter<T> converter) {
        return converter == null ? cell.getItem() == null ? "" : cell.getItem().toString() :
               converter.toString(cell.getItem());
    }

    static <T> ComboBox<T> createComboBox(final Cell<T> cell, final Function<ObservableList<T>, ComboBox<T>> boxSupplier, final ObservableList<T> items,
                                          final ObjectProperty<StringConverter<T>> converter) {
        ComboBox<T> comboBox = boxSupplier.apply(items);
        comboBox.converterProperty().bind(converter);
        comboBox.setMaxWidth(Double.MAX_VALUE);

        comboBox.addEventFilter(KeyEvent.KEY_RELEASED, e -> {
            if (e.getCode() == KeyCode.ENTER) {
                tryComboBoxCommit(comboBox, cell);
            } else if (e.getCode() == KeyCode.ESCAPE) {
                cell.cancelEdit();
            }
        });

        comboBox.valueProperty().addListener((observable, oldValue, newValue) -> {
            if (cell.isEditing()) {
                tryComboBoxCommit(comboBox, cell);
            }
        });

        return comboBox;
    }

    private static <T> void tryComboBoxCommit(ComboBox<T> comboBox, Cell<T> cell) {
        StringConverter<T> sc = comboBox.getConverter();
        if (comboBox.isEditable() && sc != null) {
            T value = sc.fromString(comboBox.getEditor().getText());
            cell.commitEdit(value);
        } else {
            cell.commitEdit(comboBox.getValue());
        }
    }
}
