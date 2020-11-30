package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.game.PersonalDeity;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class PersonalDeityStringCellFactory implements Callback<ListView<PersonalDeity>, ListCell<PersonalDeity>> {

    @Override
    public ListCell<PersonalDeity> call(ListView<PersonalDeity> param) {
        return new ListCell<>() {

            @Override
            protected void updateItem(PersonalDeity value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : value.getLocalizedName());
            }
        };
    }
}
