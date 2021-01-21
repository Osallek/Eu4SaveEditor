package fr.osallek.eu4saveeditor.controller.converter;

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
        return new ListCell<HreReligionStatus>() {

            @Override
            protected void updateItem(HreReligionStatus value, boolean empty) {
                super.updateItem(value, empty);

                if (!empty) {
                    String text;
                    switch (value) {
                        case PROTESTANT:
                            text = save.getGame().getLocalisation("protestant");
                            break;

                        case CATHOLIC:
                            text = save.getGame().getLocalisation("catholic");
                            break;

                        case PEACE:
                        default:
                            text = save.getGame().getLocalisationClean("HRE_RELIGIOUS_PEACE");
                    }

                    setText(text.substring(0, 1).toUpperCase() + text.substring(1));
                }
            }
        };
    }
}
