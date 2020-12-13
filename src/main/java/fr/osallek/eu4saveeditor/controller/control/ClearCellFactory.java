package fr.osallek.eu4saveeditor.controller.control;

import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.layout.HBox;
import javafx.util.Callback;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;

import java.util.function.Function;

public class ClearCellFactory<T> extends TableCell<T, Void> {

    public static <T> Callback<TableColumn<T, Void>, TableCell<T, Void>> forTableColumn() {
        return list -> new ClearCellFactory<>(null);
    }

    public static <T> Callback<TableColumn<T, Void>, TableCell<T, Void>> forTableColumn(Function<T, Boolean> disableSupplier) {
        return list -> new ClearCellFactory<>(disableSupplier);
    }

    private final Function<T, Boolean> disableSupplier;

    private final HBox hBox = new HBox();

    private final Button button = new Button("", new Glyph("FontAwesome", FontAwesome.Glyph.CLOSE));

    public ClearCellFactory(Function<T, Boolean> disableSupplier) {
        this.hBox.setAlignment(Pos.CENTER);
        this.hBox.getChildren().add(this.button);
        this.button.setOnAction(event -> getTableView().getItems().remove(getIndex()));
        this.disableSupplier = disableSupplier;
    }

    @Override
    public void updateItem(Void item, boolean empty) {
        super.updateItem(item, empty);
        if (empty) {
            setGraphic(null);
        } else {
            setGraphic(this.hBox);

            if (this.disableSupplier != null) {
                this.button.disableProperty().setValue(this.disableSupplier.apply(getTableRow().getItem()));
            }
        }
    }
}
