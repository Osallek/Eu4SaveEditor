package com.osallek.eu4saveeditor.controller.validator;

import javafx.scene.control.Control;
import org.controlsfx.control.decoration.Decoration;
import org.controlsfx.validation.decoration.GraphicValidationDecoration;

import java.util.ArrayList;
import java.util.Collection;

public class CustomGraphicValidationDecoration extends GraphicValidationDecoration {

    @Override
    protected Collection<Decoration> createRequiredDecorations(Control target) {
        return new ArrayList<>();
    }
}
