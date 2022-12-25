package fr.osallek.eu4saveeditor.controller.control;

import java.util.function.Function;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.layout.HBox;
import javafx.scene.text.TextAlignment;
import javafx.util.Callback;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;

public class ClearCellFactory<T> extends TableCell<T, Void> {

    public static <T> Callback<TableColumn<T, Void>, TableCell<T, Void>> forTableColumn() {
        return list -> new ClearCellFactory<>(null);
    }

    public static <T> Callback<TableColumn<T, Void>, TableCell<T, Void>> forTableColumn(Function<T, Boolean> disableSupplier) {
        return list -> new ClearCellFactory<>(disableSupplier);
    }

    private final Function<T, Boolean> disableSupplier;

    private final HBox hBox = new HBox();

    private final Button button;

    public ClearCellFactory(Function<T, Boolean> disableSupplier) {
        this.button = new Button(String.valueOf(FontAwesome.Glyph.REMOVE.getChar()));
        this.button.setStyle("-fx-font-family: FontAwesome");
        this.button.setTextAlignment(TextAlignment.CENTER);
        this.button.setOnAction(event -> getTableView().getItems().remove(getIndex()));
        this.hBox.setAlignment(Pos.CENTER);
        this.hBox.getChildren().add(this.button);
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
