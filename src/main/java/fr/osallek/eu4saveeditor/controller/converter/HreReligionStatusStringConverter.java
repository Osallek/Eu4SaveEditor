package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.localisation.Eu4Language;
import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4parser.model.save.empire.HreReligionStatus;
import javafx.util.StringConverter;

public class HreReligionStatusStringConverter extends StringConverter<HreReligionStatus> {

    private final Save save;

    public HreReligionStatusStringConverter(Save save) {
        this.save = save;
    }

    @Override
    public String toString(HreReligionStatus hreReligionStatus) {
        String text;

        if (hreReligionStatus == null) {
            text = this.save.getGame().getLocalisationClean("HRE_RELIGIOUS_PEACE", Eu4Language.getDefault());
        } else {
            text = switch (hreReligionStatus) {
                case PROTESTANT -> this.save.getGame().getLocalisationClean("protestant", Eu4Language.getDefault());
                case CATHOLIC -> this.save.getGame().getLocalisationClean("catholic", Eu4Language.getDefault());
                case PEACE -> this.save.getGame().getLocalisationClean("HRE_RELIGIOUS_PEACE", Eu4Language.getDefault());
            };
        }

        return text.substring(0, 1).toUpperCase() + text.substring(1);
    }

    @Override
    public HreReligionStatus fromString(String name) {
        return name == null ? null : HreReligionStatus.valueOf(name);
    }
}
