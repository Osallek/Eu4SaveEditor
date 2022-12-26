package fr.osallek.eu4saveeditor.common;

import fr.osallek.clausewitzparser.model.ClausewitzItem;
import fr.osallek.eu4parser.Eu4Parser;
import fr.osallek.eu4parser.model.game.Game;
import fr.osallek.eu4parser.model.save.Save;
import javafx.concurrent.Task;
import org.springframework.context.MessageSource;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

public class ReadSaveTask extends Task<Save> {

    private final FileProperty gameDirectory;

    private final Path saveFile;

    private final MessageSource messageSource;

    public ReadSaveTask(FileProperty gameDirectory, Path saveFile, MessageSource messageSource) {
        this.gameDirectory = gameDirectory;
        this.saveFile = saveFile;
        this.messageSource = messageSource;

        //Init
        updateProgress(0, 100);
        updateTitle(this.messageSource.getMessage("ose.progress." + ReadSaveStep.values()[0].name(), null, Constants.LOCALE));
    }

    @Override
    protected Save call() throws Exception {
        AtomicInteger count = new AtomicInteger(0);
        Game game = Eu4Parser.parseGame(this.gameDirectory.getValue().getAbsoluteFile().toPath(), Eu4Parser.getMods(this.saveFile, new HashMap<>()),
                                        Eu4Parser.loadSettings(this.gameDirectory.getValue().getAbsoluteFile().toPath()),
                                        () -> {
                                            count.incrementAndGet();
                                            int progress = ReadSaveStep.GAME.progress;
                                            progress += (ReadSaveStep.GAME.next().progress - ReadSaveStep.GAME.progress) * count.get()
                                                        / (Game.NB_PARTS + 1);

                                            updateProgress(progress, 100);
                                        });

        return Eu4Parser.loadSave(this.saveFile, game, new HashMap<>(),
                                  Map.of(item -> ClausewitzItem.DEFAULT_NAME.equals(item.getParent().getName()), this::getProgress));
    }

    private void getProgress(String itemName) {
        ReadSaveStep.getStep(itemName).ifPresent(step -> {
            updateProgress(step.progress, 100);
            updateTitle(this.messageSource.getMessage("ose.progress." + step.name(), null, Constants.LOCALE));
        });
    }
}
