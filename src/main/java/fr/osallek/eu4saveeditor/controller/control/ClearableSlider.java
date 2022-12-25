package fr.osallek.eu4saveeditor.controller.control;

import java.util.function.DoubleSupplier;
import javafx.beans.property.DoubleProperty;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Slider;
import javafx.scene.control.Spinner;
import javafx.scene.control.SpinnerValueFactory;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.text.TextAlignment;
import javafx.util.StringConverter;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;

public class ClearableSlider extends HBox {

    private final Slider slider;

    private final Spinner<Number> spinner;

    private final Button button;

    public ClearableSlider(double min, double max, Double value, DoubleSupplier clearSupplier, StringConverter<Number> stringConverter) {
        this.spinner = new Spinner<>(new NumberSpinnerValueFactory(min, max, value == null ? 0 : value));
        this.spinner.getValueFactory().setConverter(stringConverter);
        this.spinner.setEditable(true);
        this.spinner.focusedProperty().addListener((observable, oldValue, newValue) -> {
            //Force commit
            if (Boolean.FALSE.equals(newValue)) {
                if (!this.spinner.isEditable()) {
                    return;
                }

                String text = this.spinner.getEditor().getText();
                SpinnerValueFactory<Number> valueFactory = this.spinner.getValueFactory();
                if (valueFactory != null) {
                    StringConverter<Number> converter = valueFactory.getConverter();
                    if (converter != null) {
                        Number v = converter.fromString(text);
                        valueFactory.setValue(v);
                    }
                }
            }
        });
        HBox.setHgrow(this.spinner, Priority.ALWAYS);

        this.slider = new Slider(min, max, value == null ? 0 : value);

        this.slider.valueProperty().addListener((observable, oldValue, newValue) -> {
            if (!oldValue.equals(newValue)) {
                this.spinner.getValueFactory().setValue(newValue);
            }
        });
        this.spinner.valueProperty().addListener((observable, oldValue, newValue) -> {
            if (!oldValue.equals(newValue)) {
                this.slider.setValue(newValue.doubleValue());
            }
        });

        HBox.setHgrow(this.slider, Priority.ALWAYS);


        this.button = new Button(String.valueOf(FontAwesome.Glyph.REMOVE.getChar()));
        this.button.setStyle("-fx-font-family: FontAwesome");
        this.button.setTextAlignment(TextAlignment.CENTER);

        if (clearSupplier != null) {
            this.button.setOnMouseReleased(e -> this.setValue(clearSupplier.getAsDouble()));
        }

        this.setAlignment(Pos.CENTER_LEFT);
        this.setSpacing(5);
        getChildren().add(this.slider);
        getChildren().add(this.spinner);
        getChildren().add(this.button);
    }

    public Slider getSlider() {
        return slider;
    }

    public double getValue() {
        return this.slider.getValue();
    }

    public void setValue(double value) {
        this.slider.setValue(value);
    }

    public DoubleProperty getDoubleProperty() {
        return this.slider.valueProperty();
    }

    public void setSupplier(DoubleSupplier clearSupplier) {
        if (clearSupplier != null) {
            this.button.setOnMouseReleased(e -> this.setValue(clearSupplier.getAsDouble()));
        }
    }
}
