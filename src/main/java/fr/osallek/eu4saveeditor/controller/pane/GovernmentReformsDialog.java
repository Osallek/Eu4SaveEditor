package fr.osallek.eu4saveeditor.controller.pane;

import fr.osallek.eu4parser.model.game.GovernmentReform;
import fr.osallek.eu4parser.model.save.country.Country;
import fr.osallek.eu4saveeditor.Eu4SaveEditor;
import fr.osallek.eu4saveeditor.common.Constants;
import fr.osallek.eu4saveeditor.controller.control.SelectableGridView;
import javafx.collections.FXCollections;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Dialog;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.TitledPane;
import javafx.scene.image.Image;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

import java.util.ArrayList;
import java.util.List;

public class GovernmentReformsDialog extends Dialog<List<GovernmentReform>> {

    private final List<GovernmentReform> backUp;

    private final List<GovernmentReform> governmentReforms;

    private final List<SelectableGridView<GovernmentReform>> selectableGridViews = new ArrayList<>();

    public GovernmentReformsDialog(Country country, List<GovernmentReform> governmentReforms) {
        VBox vBox = new VBox(3);
        vBox.setAlignment(Pos.TOP_RIGHT);
        vBox.setMaxWidth(Double.MAX_VALUE);
        vBox.setPrefWidth(760);

        this.backUp = new ArrayList<>(governmentReforms);
        this.governmentReforms = new ArrayList<>(this.backUp);

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
                   VBox.setVgrow(gridView, Priority.ALWAYS);
                   VBox.setVgrow(titledPane, Priority.ALWAYS);
               });

        Pane emptyPane = new Pane();
        vBox.getChildren().add(emptyPane);
        VBox.setVgrow(emptyPane, Priority.ALWAYS);

        Button resetButton = new Button(country.getSave().getGame().getLocalisation("PW_RESET"));
        resetButton.setPrefHeight(20);
        resetButton.setOnAction(event -> {
            this.governmentReforms.clear();
            this.governmentReforms.addAll(this.backUp);
            this.selectableGridViews.forEach(view -> {
                List<GovernmentReform> reforms = new ArrayList<>(view.getItems());
                view.getItems().clear();
                view.getSelectedItems().clear();
                view.getItems().addAll(reforms);
                view.getItems().stream().filter(this.governmentReforms::contains).findFirst().ifPresent(view::select);
            });
        });
        vBox.getChildren().add(resetButton);

        ScrollPane scrollPane = new ScrollPane();
        scrollPane.setMaxWidth(Double.MAX_VALUE);
        scrollPane.setFitToHeight(true);
        scrollPane.setContent(vBox);

        setTitle(country.getSave().getGame().getLocalisationClean("governmental_reforms"));
        setResizable(true);
        getDialogPane().setMaxWidth(Double.MAX_VALUE);
        getDialogPane().setPrefWidth(800);
        getDialogPane().setContent(scrollPane);
        getDialogPane().getButtonTypes().addAll(ButtonType.OK, ButtonType.CANCEL);
        ((Stage) getDialogPane().getScene().getWindow()).getIcons().addAll(new Image(Eu4SaveEditor.class.getResourceAsStream(Constants.IMAGE_ICON)));
        setResultConverter(button -> {
            if (button.getButtonData().isDefaultButton() && !button.getButtonData().isCancelButton()) {
                this.selectableGridViews.forEach(view -> this.governmentReforms.addAll(view.getSelectedItems()));
                return this.governmentReforms;
            }

            return null;
        });
    }
}
