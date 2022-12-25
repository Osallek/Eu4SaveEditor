package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.localisation.Eu4Language;
import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4parser.model.save.empire.HreReligionStatus;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class HreReligionStatusStringCellFactory implements Callback<ListView<HreReligionStatus>, ListCell<HreReligionStatus>> {

    private final Save save;

    public HreReligionStatusStringCellFactory(Save save) {
        this.save = save;
    }

    @Override
    public ListCell<HreReligionStatus> call(ListView<HreReligionStatus> param) {
        return new ListCell<>() {

            @Override
            protected void updateItem(HreReligionStatus value, boolean empty) {
                super.updateItem(value, empty);

                if (!empty) {
                    String text = switch (value) {
                        case PROTESTANT -> save.getGame().getLocalisationClean("protestant", Eu4Language.getDefault());
                        case CATHOLIC -> save.getGame().getLocalisationClean("catholic", Eu4Language.getDefault());
                        case PEACE -> save.getGame().getLocalisationClean("HRE_RELIGIOUS_PEACE", Eu4Language.getDefault());
                    };

                    setText(text.substring(0, 1).toUpperCase() + text.substring(1));
                }
            }
        };
    }
}
