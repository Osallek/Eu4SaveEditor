package com.osallek.eu4saveeditor.controller.propertyeditor.item;

import javafx.collections.ObservableList;
import org.controlsfx.control.PropertySheet;

import java.util.Collection;

public interface CustomItem<T> extends PropertySheet.Item {

    ObservableList<T> getChoices();
}
