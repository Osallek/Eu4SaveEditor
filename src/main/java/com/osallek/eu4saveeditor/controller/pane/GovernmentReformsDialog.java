package com.osallek.eu4saveeditor.controller.pane;

import com.osallek.eu4parser.model.game.GovernmentReform;
import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4saveeditor.common.Constants;
import com.osallek.eu4saveeditor.controller.control.SelectableGridView;
import javafx.collections.FXCollections;
import javafx.geometry.Pos;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Dialog;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TitledPane;
import javafx.scene.image.Image;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

import java.util.ArrayList;
import java.util.List;

public class GovernmentReformsDialog extends Dialog<List<GovernmentReform>> {

    private final List<GovernmentReform> governmentReforms;

    private final List<SelectableGridView<GovernmentReform>> selectableGridViews = new ArrayList<>();

    public GovernmentReformsDialog(Country country, List<GovernmentReform> governmentReforms) {
        VBox vBox = new VBox(3);
        vBox.setAlignment(Pos.CENTER_RIGHT);
        vBox.setMaxWidth(Double.MAX_VALUE);
        vBox.setPrefWidth(760);

        this.governmentReforms = new ArrayList<>(governmentReforms);
        country.getGovernment()
               .getAvailableReforms()
               .forEach((s, reforms) -> {
                   TitledPane titledPane = new TitledPane();
                   titledPane.setText(country.getSave().getGame().getLocalisationClean(s));
                   SelectableGridView<GovernmentReform> gridView = new SelectableGridView<>(FXCollections.observableArrayList(reforms));
                   gridView.setCellFactory(GovernmentReform::getLocalizedName, GovernmentReform::getImageFile);
                   gridView.getItems().stream().filter(this.governmentReforms::contains).findFirst().ifPresent(gridView::select);
                   this.governmentReforms.removeAll(reforms);
                   this.selectableGridViews.add(gridView);
                   titledPane.setContent(gridView);
                   vBox.getChildren().add(titledPane);
               });


        ScrollPane scrollPane = new ScrollPane();
        scrollPane.setMaxWidth(Double.MAX_VALUE);
        scrollPane.setContent(vBox);

        setTitle(country.getSave().getGame().getLocalisationClean("governmental_reforms"));
        setResizable(true);
        getDialogPane().setMaxWidth(Double.MAX_VALUE);
        getDialogPane().setPrefWidth(800);
        getDialogPane().setPrefHeight(vBox.getChildren().size() * 100d);
        getDialogPane().setContent(scrollPane);
        getDialogPane().getButtonTypes().addAll(ButtonType.OK, ButtonType.CANCEL);
        ((Stage) getDialogPane().getScene().getWindow()).getIcons().addAll(new Image(Constants.IMAGE_ICON));
        setResultConverter(button -> {
            if (button.getButtonData().isDefaultButton() && !button.getButtonData().isCancelButton()) {
                this.selectableGridViews.forEach(view -> this.governmentReforms.addAll(view.getSelectedItems()));
                return this.governmentReforms;
            }

            return null;
        });
    }
}
