package fr.osallek.eu4saveeditor.common;

import fr.osallek.clausewitzparser.model.ClausewitzItem;
import fr.osallek.eu4parser.Eu4Parser;
import fr.osallek.eu4parser.model.save.Save;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import javafx.concurrent.Task;
import org.springframework.context.MessageSource;

public class ReadSaveTask extends Task<Save> {

    private final FileProperty gameDirectory;

    private final Path saveFile;

    private final MessageSource messageSource;

    public ReadSaveTask(FileProperty gameDirectory, Path saveFile, MessageSource messageSource) {
        this.gameDirectory = gameDirectory;
        this.saveFile = saveFile;
        this.messageSource = messageSource;
        getProgress(ReadSaveStep.values()[0].itemName); //Init
    }

    @Override
    protected Save call() throws Exception {
        return Eu4Parser.loadSave(this.gameDirectory.getValue().getAbsoluteFile().toPath(),
                                  this.saveFile,
                                  Eu4Parser.loadSettings(this.gameDirectory.getValue().getAbsoluteFile().toPath()), new HashMap<>(),
                                  Map.of(item -> ClausewitzItem.DEFAULT_NAME.equals(item.getParent().getName()), this::getProgress));
    }

    private void getProgress(String itemName) {
        ReadSaveStep.getStep(itemName).ifPresent(step -> {
            updateProgress(step.step, ReadSaveStep.NB_STEPS);
            updateTitle(this.messageSource.getMessage("ose.progress." + step.name(), null, Constants.LOCALE));
        });
    }
}
