package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.clausewitzparser.common.ClausewitzUtils;
import com.osallek.eu4parser.model.save.province.SaveProvince;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class ProvinceStringCellFactory implements Callback<ListView<SaveProvince>, ListCell<SaveProvince>> {

    @Override
    public ListCell<SaveProvince> call(ListView<SaveProvince> param) {
        return new ListCell<SaveProvince>() {

            @Override
            protected void updateItem(SaveProvince value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : ClausewitzUtils.removeQuotes(value.getName()));
            }
        };
    }
}
