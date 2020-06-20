package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.game.Event;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class EventStringCellFactory implements Callback<ListView<Event>, ListCell<Event>> {

    @Override
    public ListCell<Event> call(ListView<Event> param) {
        return new ListCell<Event>() {

            @Override
            protected void updateItem(Event value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : value.getLocalizedName());
            }
        };
    }
}
