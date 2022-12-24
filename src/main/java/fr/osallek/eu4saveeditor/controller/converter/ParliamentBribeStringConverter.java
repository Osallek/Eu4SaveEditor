package fr.osallek.eu4saveeditor.controller.converter;

import fr.osallek.eu4parser.model.game.ParliamentBribe;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import javafx.util.StringConverter;
import org.apache.commons.lang3.StringUtils;

public class ParliamentBribeStringConverter extends StringConverter<ParliamentBribe> {

    public static final ParliamentBribeStringConverter INSTANCE = new ParliamentBribeStringConverter();

    @Override
    public String toString(ParliamentBribe parliamentBribe) {
        return parliamentBribe == null ? "" : StringUtils.capitalize(Eu4SaveEditorUtils.localize(parliamentBribe.getName(), parliamentBribe.getGame()));
    }

    @Override
    public ParliamentBribe fromString(String parliamentBribe) {
        return null;
    }
}
