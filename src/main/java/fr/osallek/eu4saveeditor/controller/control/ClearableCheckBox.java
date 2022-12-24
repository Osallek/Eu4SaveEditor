package fr.osallek.eu4saveeditor.controller.control;

import java.util.function.Supplier;
import javafx.beans.property.BooleanProperty;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;

public class ClearableCheckBox extends HBox {

    private final CheckBox checkBox;

    private final Button button;

    public ClearableCheckBox() {
        this(null);
    }

    public ClearableCheckBox(Supplier<Boolean> clearSupplier) {
        this.checkBox = new CheckBox();
        this.checkBox.setMaxWidth(Double.MAX_VALUE);
        HBox.setHgrow(this.checkBox, Priority.ALWAYS);

        this.button = new Button("", new Glyph("FontAwesome", FontAwesome.Glyph.CLOSE));
        this.button.setAlignment(Pos.TOP_CENTER);

        if (clearSupplier != null) {
            this.button.setOnMouseReleased(e -> this.checkBox.setSelected(clearSupplier.get()));
        }

        getChildren().add(this.checkBox);
        getChildren().add(this.button);
    }

    public CheckBox getCheckBox() {
        return checkBox;
    }

    public boolean getValue() {
        return this.checkBox.isSelected();
    }

    public BooleanProperty selectedProperty() {
        return this.checkBox.selectedProperty();
    }

    public void setValue(boolean b) {
        this.checkBox.setSelected(b);
    }

    public void setSupplier(Supplier<Boolean> clearSupplier) {
        if (clearSupplier != null) {
            this.button.setOnMouseReleased(e -> setValue(clearSupplier.get()));
        }
    }

    public EventHandler<ActionEvent> getOnAction() {
        return this.checkBox.getOnAction();
    }

    public void setOnAction(EventHandler<ActionEvent> onAction) {
        this.checkBox.setOnAction(onAction);
    }
}
