package com.osallek.eu4saveeditor.controller.control;

import com.osallek.eu4saveeditor.imagereader.ImageReader;
import javafx.embed.swing.SwingFXUtils;
import javafx.geometry.Pos;
import javafx.scene.control.Tooltip;
import javafx.scene.effect.ColorAdjust;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseButton;
import javafx.stage.PopupWindow;
import org.controlsfx.control.GridCell;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.function.Function;

public class SelectableGridCell<T> extends GridCell<T> {

    private static final Logger LOGGER = LoggerFactory.getLogger(SelectableGridCell.class);

    private final ColorAdjust notSelectedEffect;

    private ImageView imageView;

    private Tooltip tooltip;

    private Function<T, String> textFunction;

    private Function<T, File> imageFunction;

    private int size;

    public SelectableGridCell(Function<T, String> textFunction, Function<T, File> imageFunction, int size) {
        this.size = size;
        this.notSelectedEffect = new ColorAdjust();
        this.notSelectedEffect.setSaturation(-1);

        if (imageFunction != null) {
            this.imageFunction = imageFunction;
            this.imageView = new ImageView();
            this.imageView.setPickOnBounds(true);
        }

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

                    if (this.imageView != null) {
                        this.imageView.setEffect(null);
                    }
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

            updateSelected(getSelectableGridView().isSelected(item));
            getSelectableGridView().getCells().add(this);

            if (this.tooltip != null) {
                this.tooltip.setText(this.textFunction.apply(item));
            }

            if (this.imageFunction != null) {
                try {
                    BufferedImage image = ImageReader.convertFileToImage(this.imageFunction.apply(item));

                    if (image != null) {
                        this.imageView.setImage(SwingFXUtils.toFXImage(image, null));

                        setGraphic(this.imageView);
                        setMaxWidth(this.size);
                        setMaxHeight(this.size);
                        this.imageView.setFitHeight(this.size);
                        this.imageView.setFitWidth(this.size);

                        if (!selectedProperty().get()) {
                            this.imageView.setEffect(this.notSelectedEffect);
                        } else {
                            this.imageView.setEffect(null);
                        }
                    }
                } catch (IOException e) {
                    LOGGER.error(e.getMessage(), e);
                }
            }
        }
    }

}
