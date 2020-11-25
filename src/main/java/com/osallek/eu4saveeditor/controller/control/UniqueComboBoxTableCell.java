/*
 * Copyright (c) 2012, 2017, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package com.osallek.eu4saveeditor.controller.control;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
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

import java.util.Map;

public class UniqueComboBoxTableCell<S, T> extends TableCell<S, T> {

    public static <S, T> Callback<TableColumn<S, T>, TableCell<S, T>> forTableColumn(final StringConverter<T> converter, final Map<S, SortedList<T>> items) {
        return list -> new UniqueComboBoxTableCell<>(converter, items);
    }

    private final Map<S, SortedList<T>> items;

    private ComboBox<T> comboBox;

    public UniqueComboBoxTableCell(StringConverter<T> converter, Map<S, SortedList<T>> items) {
        this.getStyleClass().add("combo-box-table-cell");
        this.items = items;
        setConverter(converter);
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

    public Map<S, SortedList<T>> getItems() {
        return items;
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
            this.comboBox = createComboBox(this, this.items.get(getTableRow().getItem()), converterProperty());
            this.comboBox.editableProperty().bind(comboBoxEditableProperty());
        }

        this.comboBox.getSelectionModel().select(getItem());

        super.startEdit();
        setText(null);
        setGraphic(comboBox);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void cancelEdit() {
        super.cancelEdit();

        setText(getConverter().toString(getItem()));
        setGraphic(null);
    }

    /**
     * {@inheritDoc}
     */
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
