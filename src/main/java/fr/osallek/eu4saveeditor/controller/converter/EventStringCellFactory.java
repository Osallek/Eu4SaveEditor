package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.Event;
import fr.osallek.eu4parser.model.game.Game;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class EventStringCellFactory implements Callback<ListView<Event>, ListCell<Event>> {

    private final Game game;

    public EventStringCellFactory(Game game) {
        this.game = game;
    }

    @Override
    public ListCell<Event> call(ListView<Event> param) {
        return new ListCell<>() {

            @Override
            protected void updateItem(Event value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : Eu4SaveEditorUtils.localize(value.getTitle(), game));
            }
        };
    }
}
