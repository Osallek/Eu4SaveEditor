package com.osallek.eu4saveeditor.controller.pane;

import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4saveeditor.common.Constants;
import com.osallek.eu4saveeditor.controller.control.CustomListSelectionView;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Dialog;
import javafx.scene.image.Image;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import org.controlsfx.control.ListSelectionView;

import java.util.Collection;
import java.util.List;
import java.util.function.Supplier;

public class ListSelectionViewDialog<S> extends Dialog<List<S>> {

    private final CustomListSelectionView<S> listSelectionView;

    public ListSelectionViewDialog(Save save, CustomListSelectionView<S> listSelectionView, String title, Supplier<Collection<S>> sourceSupplier,
                                   Supplier<Collection<S>> targetSupplier) {
        this.listSelectionView = listSelectionView;
        Button resetButton = new Button(save.getGame().getLocalisation("PW_RESET"));
        resetButton.setOnAction(event -> this.listSelectionView.onReset(sourceSupplier, targetSupplier));

        VBox vBox = new VBox(3);
        vBox.getChildren().add(this.listSelectionView);
        vBox.getChildren().add(resetButton);
        vBox.setAlignment(Pos.CENTER_RIGHT);
        VBox.setVgrow(this.listSelectionView, Priority.ALWAYS);

        setTitle(title);
        setResizable(true);
        getDialogPane().setMaxWidth(Double.MAX_VALUE);
        getDialogPane().setContent(vBox);
        getDialogPane().getButtonTypes().addAll(ButtonType.OK, ButtonType.CANCEL);
        ((Stage) getDialogPane().getScene().getWindow()).getIcons().addAll(new Image(Constants.IMAGE_ICON));
        setResultConverter(button -> {
            if (button.getButtonData().isDefaultButton() && !button.getButtonData().isCancelButton()) {
                return this.listSelectionView.getTargetItems();
            }

            return null;
        });
    }

    public ListSelectionView<S> getListSelectionView() {
        return listSelectionView;
    }
}
