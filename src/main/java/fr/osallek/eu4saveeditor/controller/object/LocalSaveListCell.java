package fr.osallek.eu4saveeditor.controller.object;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import javafx.scene.control.ListCell;

public class LocalSaveListCell extends ListCell<Path> {

    private final Path saveFolder;

    public LocalSaveListCell(Path saveFolder) {
        this.saveFolder = saveFolder;
    }

    @Override
    protected void updateItem(Path path, boolean empty) {
        super.updateItem(path, empty);
        if (path == null || empty) {
            setGraphic(null);
        } else {
            Path relativize = this.saveFolder == null ? path : this.saveFolder.relativize(path);

            List<String> strings = new ArrayList<>();

            for (int i = 0; i < relativize.getNameCount(); i++) {
                strings.add(relativize.getName(i).toString());
            }

            setText(String.join(" > ", strings));
        }
    }

}
