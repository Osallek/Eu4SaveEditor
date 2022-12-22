package fr.osallek.eu4saveeditor.controller.converter;

import java.io.File;
import javafx.util.StringConverter;

public class FileStringConverter extends StringConverter<File> {

    @Override
    public String toString(File file) {
        if(file != null && file.exists() && file.canRead()) {
            return file.getPath();
        }

        return "";
    }

    @Override
    public File fromString(String path) {
        return path == null ? null : new File(path);
    }
}
