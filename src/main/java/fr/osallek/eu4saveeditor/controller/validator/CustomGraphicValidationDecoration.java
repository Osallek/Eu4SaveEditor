package fr.osallek.eu4saveeditor.controller.validator;

import java.util.ArrayList;
import java.util.Collection;
import javafx.scene.control.Control;
import org.controlsfx.control.decoration.Decoration;
import org.controlsfx.validation.decoration.GraphicValidationDecoration;

public class CustomGraphicValidationDecoration extends GraphicValidationDecoration {

    @Override
    protected Collection<Decoration> createRequiredDecorations(Control target) {
        return new ArrayList<>();
    }
}
