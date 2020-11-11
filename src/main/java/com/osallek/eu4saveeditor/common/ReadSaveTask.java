package com.osallek.eu4saveeditor.common;

import com.osallek.clausewitzparser.model.ClausewitzItem;
import com.osallek.eu4parser.Eu4Parser;
import com.osallek.eu4parser.model.game.localisation.Eu4Language;
import com.osallek.eu4parser.model.save.Save;
import javafx.concurrent.Task;

import java.util.Map;

public class ReadSaveTask extends Task<Save> {

    private final FileProperty gameDirectory;

    private final FileProperty modDirectory;

    private final FileProperty saveFile;

    private final Eu4Language eu4Language;

    public ReadSaveTask(FileProperty gameDirectory, FileProperty modDirectory, FileProperty saveFile, Eu4Language eu4Language) {
        this.gameDirectory = gameDirectory;
        this.modDirectory = modDirectory;
        this.saveFile = saveFile;
        this.eu4Language = eu4Language;
    }

    @Override
    protected Save call() throws Exception {
        return Eu4Parser.loadSave(this.gameDirectory.getValue().getAbsolutePath(),
                                  this.modDirectory.getValue().getAbsolutePath(),
                                  this.saveFile.getValue().getAbsolutePath(),
                                  Map.of(item -> ClausewitzItem.DEFAULT_NAME.equals(item.getParent().getName()), this::getProgress));
    }

    private void getProgress(String itemName) {
        ReadSaveStep step;

        if ((step = ReadSaveStep.BY_ITEM_NAME.get(itemName)) != null) {
            this.updateProgress(step.step, ReadSaveStep.NB_STEPS);
            this.updateTitle(step.getText(this.eu4Language));
        }
    }
}
