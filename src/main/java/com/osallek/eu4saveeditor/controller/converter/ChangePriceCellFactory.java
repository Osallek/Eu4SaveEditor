package com.osallek.eu4saveeditor.controller.converter;

import com.osallek.eu4parser.model.save.changeprices.ChangePrice;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.layout.HBox;
import javafx.util.Callback;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;

public class ChangePriceCellFactory implements Callback<TableColumn<ChangePrice, Void>, TableCell<ChangePrice, Void>> {

    @Override
    public TableCell<ChangePrice, Void> call(TableColumn<ChangePrice, Void> param) {
        return new TableCell<ChangePrice, Void>() {
            private final HBox hBox = new HBox();
            private final Button button = new Button("", new Glyph("FontAwesome", FontAwesome.Glyph.CLOSE));

            {
                this.hBox.setAlignment(Pos.CENTER);
                this.hBox.getChildren().add(this.button);
                this.button.setOnAction(event -> getTableView().getItems().remove(getIndex()));
            }

            @Override
            public void updateItem(Void item, boolean empty) {
                super.updateItem(item, empty);
                if (empty) {
                    setGraphic(null);
                } else {
                    setGraphic(this.hBox);
                }
            }
        };
    }
}
