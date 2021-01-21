package fr.osallek.eu4saveeditor.common;

import fr.osallek.clausewitzparser.model.ClausewitzItem;
import fr.osallek.eu4parser.Eu4Parser;
import fr.osallek.eu4parser.model.game.localisation.Eu4Language;
import fr.osallek.eu4parser.model.save.Save;
import javafx.concurrent.Task;

import java.io.File;
import java.util.Map;

public class WriteSaveTask extends Task<Void> {

    private final Save save;

    private final File file;

    private final Eu4Language eu4Language;

    public WriteSaveTask(Save save, File file, Eu4Language eu4Language) {
        this.save = save;
        this.file = file;
        this.eu4Language = eu4Language;
    }

    @Override
    protected Void call() throws Exception {
        this.updateProgress(WriteSaveStep.values()[0].step, WriteSaveStep.NB_STEPS);
        this.updateTitle(WriteSaveStep.values()[0].getText(this.eu4Language));
        Eu4Parser.writeSave(this.save, this.file.toString(), Map.of(item -> ClausewitzItem.DEFAULT_NAME.equals(item.getParent().getName()), this::getProgress));
        return null;
    }

    private void getProgress(String itemName) {
        WriteSaveStep step;

        if ((step = WriteSaveStep.BY_ITEM_NAME.get(itemName)) != null) {
            this.updateProgress(step.step, WriteSaveStep.NB_STEPS);
            this.updateTitle(step.getText(this.eu4Language));
        }
    }
}
