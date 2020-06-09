package com.osallek.eu4saveeditor.controller.pane;

import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4saveeditor.common.Constants;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Dialog;
import javafx.scene.control.DialogPane;
import javafx.scene.image.Image;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import org.controlsfx.control.tableview2.TableView2;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;

import java.util.List;
import java.util.function.Supplier;

public class TableView2Dialog<S> extends Dialog<List<S>> {

    private final TableView2<S> tableView2;

    public TableView2Dialog(Save save, TableView2<S> tableView2, String title, Supplier<S> supplier, Supplier<List<S>> clearSupplier) {
        this.tableView2 = tableView2;
        Button addButton = new Button("", new Glyph("FontAwesome", FontAwesome.Glyph.PLUS));
        Button resetButton = new Button(save.getGame().getLocalisation("PW_RESET"));
        final DialogPane dialogPane = getDialogPane();

        VBox vBox = new VBox(3);
        vBox.getChildren().add(this.tableView2);
        VBox.setVgrow(this.tableView2, Priority.ALWAYS);

        HBox hBox = new HBox(3);
        Pane spacer = new Pane();
        hBox.getChildren().add(spacer);
        HBox.setHgrow(spacer, Priority.ALWAYS);
        hBox.getChildren().add(resetButton);
        hBox.getChildren().add(addButton);
        vBox.getChildren().add(hBox);

        resetButton.setOnAction(event -> this.tableView2.getItems().setAll(clearSupplier.get()));
        addButton.setOnAction(event -> this.tableView2.getItems().add(supplier.get()));

        setTitle(title);
        setResizable(true);
        dialogPane.setMaxWidth(Double.MAX_VALUE);
        dialogPane.setContent(vBox);
        dialogPane.getButtonTypes().addAll(ButtonType.OK, ButtonType.CANCEL);
        ((Stage) dialogPane.getScene().getWindow()).getIcons().addAll(new Image(Constants.IMAGE_ICON));
        setResultConverter(button -> {
            if (button.getButtonData().isDefaultButton() && !button.getButtonData().isCancelButton()) {
                return this.tableView2.getItems();
            }

            return null;
        });
    }
}
