package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.empire.HreReligionStatus;
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
            text = this.save.getGame().getLocalisationClean("HRE_RELIGIOUS_PEACE");
        } else {
            switch (hreReligionStatus) {
                case PROTESTANT:
                    text = this.save.getGame().getLocalisation("protestant");
                    break;

                case CATHOLIC:
                    text = this.save.getGame().getLocalisation("catholic");
                    break;

                case PEACE:
                default:
                    text = this.save.getGame().getLocalisationClean("HRE_RELIGIOUS_PEACE");
            }
        }

        return text.substring(0, 1).toUpperCase() + text.substring(1);
    }

    @Override
    public HreReligionStatus fromString(String name) {
        return name == null ? null : HreReligionStatus.valueOf(name);
    }
}
