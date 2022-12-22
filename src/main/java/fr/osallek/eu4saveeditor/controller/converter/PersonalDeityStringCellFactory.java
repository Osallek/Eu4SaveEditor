package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.Game;
import fr.osallek.eu4parser.model.game.PersonalDeity;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class PersonalDeityStringCellFactory implements Callback<ListView<PersonalDeity>, ListCell<PersonalDeity>> {

    private final Game game;

    public PersonalDeityStringCellFactory(Game game) {this.game = game;}

    @Override
    public ListCell<PersonalDeity> call(ListView<PersonalDeity> param) {
        return new ListCell<>() {

            @Override
            protected void updateItem(PersonalDeity value, boolean empty) {
                super.updateItem(value, empty);
                setText(value == null ? null : Eu4SaveEditorUtils.localize(value.getName(), game));
            }
        };
    }
}
