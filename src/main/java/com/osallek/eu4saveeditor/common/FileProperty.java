package com.osallek.eu4saveeditor.common;

import javafx.beans.property.ObjectPropertyBase;

import java.io.File;

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
