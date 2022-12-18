package fr.osallek.eu4saveeditor.controller.control;

import java.util.function.Supplier;
import javafx.beans.property.ObjectProperty;
import javafx.scene.Node;
import javafx.scene.control.TextField;
import javafx.scene.layout.Region;
import javafx.scene.layout.StackPane;
import org.controlsfx.control.textfield.CustomTextField;

/**
 * Copy from {@link org.controlsfx.control.textfield.TextFields}.createClearableTextField()
 */
public class CustomClearableTextField {

    private CustomClearableTextField() {}

    public static CustomTextField createClearableTextField(Supplier<String> clearSupplier) {
        CustomTextField inputField = new CustomTextField();
        setupClearButtonField(inputField, inputField.rightProperty(), clearSupplier);
        return inputField;
    }

    private static void setupClearButtonField(TextField inputField, ObjectProperty<Node> rightProperty, Supplier<String> clearSupplier) {
        inputField.getStyleClass().add("clearable-field");

        Region clearButton = new Region();
        clearButton.getStyleClass().addAll("graphic");
        StackPane clearButtonPane = new StackPane(clearButton);
        clearButtonPane.getStyleClass().addAll("clear-button");
        clearButtonPane.managedProperty().bind(inputField.editableProperty());
        clearButtonPane.visibleProperty().bind(inputField.editableProperty());

        if (clearSupplier != null) {
            clearButtonPane.setOnMouseReleased(e -> inputField.setText(clearSupplier.get()));
        }

        rightProperty.set(clearButtonPane);
    }
}
