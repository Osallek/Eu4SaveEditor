package com.osallek.eu4saveeditor.controller.control;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.transformation.SortedList;
import javafx.scene.Node;
import javafx.scene.control.Cell;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ListView;
import javafx.scene.control.Skin;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.skin.ComboBoxListViewSkin;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.input.MouseEvent;
import javafx.util.Callback;
import javafx.util.StringConverter;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;

public class UniqueComboBoxTableCell<S, T> extends TableCell<S, T> {

    public static <S, T> Callback<TableColumn<S, T>, TableCell<S, T>> forTableColumn(StringConverter<T> converter,
                                                                                     Map<S, ObservableList<T>> source,
                                                                                     Comparator<T> comparator,
                                                                                     Function<S, T> mapper,
                                                                                     Supplier<ObservableList<T>> supplier) {
        return list -> new UniqueComboBoxTableCell<>(converter, source, comparator, mapper, supplier);
    }

    private final Map<S, ObservableList<T>> source;

    private final Map<S, SortedList<T>> sorted = new HashMap<>();

    private final Comparator<T> comparator;

    private final Function<S, T> mapper;

    private final Supplier<ObservableList<T>> supplier;

    private ComboBox<T> comboBox;

    public UniqueComboBoxTableCell(StringConverter<T> converter, Map<S, ObservableList<T>> source, Comparator<T> comparator, Function<S, T> mapper,
                                   Supplier<ObservableList<T>> supplier) {
        this.getStyleClass().add("combo-box-table-cell");
        this.source = source;
        this.comparator = comparator;
        this.mapper = mapper;
        this.supplier = supplier;
        setConverter(converter);

        this.source.forEach((s, ts) -> this.sorted.put(s, ts.sorted(this.comparator)));
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
            this.comboBox = createComboBox(this, this.sorted.get(getTableRow().getItem()), converterProperty());
            this.comboBox.editableProperty().bind(comboBoxEditableProperty());

            getTableView().getItems().addListener((ListChangeListener<? super S>) c -> {
                while (c.next()) {
                    c.getRemoved().forEach(s -> {
                        this.source.remove(s);
                        this.sorted.remove(s);
                        this.source.values().forEach(list -> list.add(this.mapper.apply(s)));
                    });
                    c.getAddedSubList().forEach(s -> {
                        this.source.values().forEach(countries -> countries.remove(this.mapper.apply(s)));
                        this.source.put(s, this.supplier.get());
                        this.sorted.put(s, this.source.get(s).sorted(this.comparator));
                    });
                }
            });
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

    static <T> ComboBox<T> createComboBox(final Cell<T> cell, final ObservableList<T> items, final ObjectProperty<StringConverter<T>> converter) {
        ComboBox<T> comboBox = new ComboBox<>(items);
        comboBox.converterProperty().bind(converter);
        comboBox.setMaxWidth(Double.MAX_VALUE);

        comboBox.addEventFilter(KeyEvent.KEY_RELEASED, e -> {
            if (e.getCode() == KeyCode.ENTER) {
                tryComboBoxCommit(comboBox, cell);
            } else if (e.getCode() == KeyCode.ESCAPE) {
                cell.cancelEdit();
            }
        });

        comboBox.getEditor().focusedProperty().addListener(o -> {
            if (!comboBox.isFocused()) {
                tryComboBoxCommit(comboBox, cell);
            }
        });

        boolean success = listenToComboBoxSkin(comboBox, cell);
        if (!success) {
            comboBox.skinProperty().addListener(new InvalidationListener() {
                @Override
                public void invalidated(Observable observable) {
                    boolean successInListener = listenToComboBoxSkin(comboBox, cell);
                    if (successInListener) {
                        comboBox.skinProperty().removeListener(this);
                    }
                }
            });
        }

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

    private static <T> boolean listenToComboBoxSkin(final ComboBox<T> comboBox, final Cell<T> cell) {
        Skin<?> skin = comboBox.getSkin();
        if (skin != null && skin instanceof ComboBoxListViewSkin) {
            ComboBoxListViewSkin cbSkin = (ComboBoxListViewSkin) skin;
            Node popupContent = cbSkin.getPopupContent();
            if (popupContent != null && popupContent instanceof ListView) {
                popupContent.addEventHandler(MouseEvent.MOUSE_RELEASED, e -> cell.commitEdit(comboBox.getValue()));
                return true;
            }
        }
        return false;
    }
}
