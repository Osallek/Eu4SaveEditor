package fr.osallek.eu4saveeditor.common;

import java.io.File;
import javafx.beans.property.ObjectPropertyBase;

public class FileProperty extends ObjectPropertyBase<File> {

    private final Object bean;

    private final String name;

    public FileProperty(Object bean, String name) {
        this.bean = bean;
        this.name = name;
    }


    @Override
    public Object getBean() {
        return this.bean;
    }

    @Override
    public String getName() {
        return this.name;
    }
}
