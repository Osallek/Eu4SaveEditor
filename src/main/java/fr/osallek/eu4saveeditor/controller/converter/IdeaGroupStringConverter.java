package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.IdeaGroup;
import javafx.util.StringConverter;

public class IdeaGroupStringConverter extends StringConverter<IdeaGroup> {

    @Override
    public String toString(IdeaGroup ideaGroup) {
        return ideaGroup == null ? "" : ideaGroup.getLocalizedName();
    }

    @Override
    public IdeaGroup fromString(String tag) {
        return null;
    }
}
