package com.osallek.eu4saveeditor.controller.item;

import javafx.animation.FadeTransition;
import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.property.ObjectProperty;
import javafx.scene.Cursor;
import javafx.scene.Node;
import javafx.scene.control.TextField;
import javafx.scene.layout.Region;
import javafx.scene.layout.StackPane;
import javafx.util.Duration;
import org.controlsfx.control.textfield.CustomTextField;

import java.util.function.Supplier;

/**
 * Copy from {@link org.controlsfx.control.textfield.TextFields}.createClearableTextField()
 */
public class CustomClearableTextField {

    private CustomClearableTextField() {}

    private static final Duration FADE_DURATION = Duration.millis(350);

    public static TextField createClearableTextField(Supplier<String> clearSupplier) {
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
        clearButtonPane.setOpacity(0.0);
        clearButtonPane.setCursor(Cursor.DEFAULT);
        clearButtonPane.setOnMouseReleased(e -> inputField.setText(clearSupplier.get()));
        clearButtonPane.managedProperty().bind(inputField.editableProperty());
        clearButtonPane.visibleProperty().bind(inputField.editableProperty());

        rightProperty.set(clearButtonPane);

        final FadeTransition fader = new FadeTransition(FADE_DURATION, clearButtonPane);
        fader.setCycleCount(1);

        inputField.textProperty().addListener(new InvalidationListener() {
            @Override
            public void invalidated(Observable arg0) {
                String text = inputField.getText();
                boolean isTextEmpty = text == null || text.isEmpty();
                boolean isButtonVisible = fader.getNode().getOpacity() > 0;

                if (isTextEmpty && isButtonVisible) {
                    setButtonVisible(false);
                } else if (!isTextEmpty && !isButtonVisible) {
                    setButtonVisible(true);
                }
            }

            private void setButtonVisible(boolean visible) {
                fader.setFromValue(visible ? 0.0 : 1.0);
                fader.setToValue(visible ? 1.0 : 0.0);
                fader.play();
            }
        });
    }
}
