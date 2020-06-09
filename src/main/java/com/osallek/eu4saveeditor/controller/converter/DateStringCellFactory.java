package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.clausewitzparser.common.ClausewitzUtils;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

import java.util.Date;

public class DateStringCellFactory implements Callback<ListView<Date>, ListCell<Date>> {

    @Override
    public ListCell<Date> call(ListView<Date> param) {
        return new ListCell<Date>() {

            @Override
            protected void updateItem(Date value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : ClausewitzUtils.DATE_FORMAT.format(value));
            }
        };
    }
}
