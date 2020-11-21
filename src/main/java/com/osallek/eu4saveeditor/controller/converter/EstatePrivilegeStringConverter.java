package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.game.EstatePrivilege;
import javafx.util.StringConverter;

public class EstatePrivilegeStringConverter extends StringConverter<EstatePrivilege> {

    @Override
    public String toString(EstatePrivilege privilege) {
        return privilege == null ? "" : privilege.getLocalizedName();
    }

    @Override
    public EstatePrivilege fromString(String s) {
        return null;
    }
}
