package com.osallek.eu4saveeditor.controller.control;

import javafx.geometry.Pos;
import javafx.scene.control.Tooltip;
import javafx.scene.effect.ColorAdjust;
import javafx.scene.effect.Effect;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseButton;
import javafx.stage.PopupWindow;
import org.controlsfx.control.GridCell;
import org.controlsfx.control.InfoOverlay;

import java.io.File;
import java.util.function.Function;

public class SelectableGridCell<T> extends GridCell<T> {

    private final ColorAdjust notSelectedEffect;

    private final ImageView imageView;

    private Tooltip tooltip;

    private Function<T, String> textFunction;

    public SelectableGridCell(Function<T, String> textFunction) {
        this.notSelectedEffect = new ColorAdjust();
        this.notSelectedEffect.setSaturation(-1);

        this.imageView = new ImageView();
        this.imageView.setPickOnBounds(true);

        if (textFunction != null) {
            this.textFunction = textFunction;
            this.tooltip = new Tooltip();
            this.tooltip.setAnchorLocation(PopupWindow.AnchorLocation.WINDOW_TOP_RIGHT);
            Tooltip.install(this, this.tooltip);
        }

        setAlignment(Pos.CENTER);
        setOnMouseClicked(event -> {
            if (MouseButton.PRIMARY.equals(event.getButton())) {
                SelectableGridCell<T> source = ((SelectableGridCell<T>) event.getSource());

                if (source.isSelected()) {
                    unSelect();
                } else {
                    source.getSelectableGridView().getCells().forEach(SelectableGridCell::unSelect);
                    source.updateSelected(true);
                    source.getSelectableGridView().select(source.getItem());
                    this.imageView.setEffect(null);
                }
            }
        });
    }

    public SelectableGridView<T> getSelectableGridView() {
        return ((SelectableGridView<T>) gridViewProperty().get());
    }

    public void unSelect() {
        updateSelected(false);
        getSelectableGridView().unSelect(getItem());
        this.imageView.setEffect(this.notSelectedEffect);
    }

    @Override
    protected void updateItem(T item, boolean empty) {
        super.updateItem(item, empty);
        if (!empty) {
            this.imageView.setImage(new Image(new File("C:\\Users\\gaeta\\Downloads\\latin_temple.jpg")
                                                      .toURI().toString()));
            setGraphic(this.imageView);
            setMaxWidth(50);
            setMaxHeight(50);
            this.imageView.setFitHeight(50);
            this.imageView.setFitWidth(50);
            updateSelected(getSelectableGridView().isSelected(item));
            getSelectableGridView().getCells().add(this);

            if (this.tooltip != null) {
                this.tooltip.setText(textFunction.apply(item));
            }

            if (!selectedProperty().get()) {
                this.imageView.setEffect(this.notSelectedEffect);
            } else {
                this.imageView.setEffect(null);
            }
        }
    }

}
