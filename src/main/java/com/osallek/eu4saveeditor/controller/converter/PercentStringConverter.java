package com.osallek.eu4saveeditor.controller.converter;

import javafx.util.StringConverter;

public class PercentStringConverter extends StringConverter<Integer> {

    @Override
    public String toString(Integer integer) {
        return integer == null ? "" : integer + "%";
    }

    @Override
    public Integer fromString(String integer) {
        return integer == null ? null : Integer.parseInt(integer.replace("%", ""));
    }
}
