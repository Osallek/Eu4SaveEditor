package fr.osallek.eu4saveeditor.controller.pane;

import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4saveeditor.Eu4SaveEditorApplication;
import fr.osallek.eu4saveeditor.common.Constants;
import fr.osallek.eu4saveeditor.common.Copy;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import javafx.beans.property.BooleanProperty;
import javafx.collections.ObservableList;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Dialog;
import javafx.scene.control.DialogPane;
import javafx.scene.control.TableView;
import javafx.scene.image.Image;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;

public class TableViewDialog<S extends Copy<S>> extends Dialog<List<S>> {

    private final TableView<S> tableView2;

    private final Button addButton;

    private BooleanProperty disableAddProperty;

    public TableViewDialog(Save save, TableView<S> tableView2, String title, Function<ObservableList<S>, S> supplier, Supplier<List<S>> clearSupplier) {
        this(save, tableView2, title, supplier, clearSupplier, null);
    }

    public TableViewDialog(Save save, TableView<S> tableView2, String title, Function<ObservableList<S>, S> supplier, Supplier<List<S>> clearSupplier, BooleanProperty disableAdd) {
        this.tableView2 = tableView2;
        this.addButton = new Button("", new Glyph("FontAwesome", FontAwesome.Glyph.PLUS));
        Button resetButton = new Button(Eu4SaveEditorUtils.localize("PW_RESET", save.getGame()));
        final DialogPane dialogPane = getDialogPane();

        VBox vBox = new VBox(3);
        vBox.getChildren().add(this.tableView2);
        VBox.setVgrow(this.tableView2, Priority.ALWAYS);

        HBox hBox = new HBox(3);
        Pane spacer = new Pane();
        hBox.getChildren().add(spacer);
        HBox.setHgrow(spacer, Priority.ALWAYS);
        hBox.getChildren().add(resetButton);
        hBox.getChildren().add(this.addButton);
        vBox.getChildren().add(hBox);

        resetButton.setOnAction(event -> this.tableView2.getItems().setAll(clearSupplier.get().stream().map(Copy::copy).collect(Collectors.toList())));
        this.addButton.setOnAction(event -> this.tableView2.getItems().add(supplier.apply(getItems())));

        setDisableAddProperty(disableAdd);

        setTitle(title);
        setResizable(true);
        dialogPane.setMaxWidth(Double.MAX_VALUE);
        dialogPane.setContent(vBox);
        dialogPane.getButtonTypes().addAll(ButtonType.OK, ButtonType.CANCEL);
        ((Stage) dialogPane.getScene().getWindow()).getIcons().addAll(new Image(Eu4SaveEditorApplication.class.getResourceAsStream(Constants.IMAGE_ICON)));
        setResultConverter(button -> {
            if (button.getButtonData().isDefaultButton() && !button.getButtonData().isCancelButton()) {
                return this.tableView2.getItems();
            }

            return null;
        });
    }

    public ObservableList<S> getItems() {
        return this.tableView2.getItems();
    }

    public TableView<S> getTableView2() {
        return tableView2;
    }

    public BooleanProperty disableAddPropertyProperty() {
        return disableAddProperty;
    }

    public void setDisableAddProperty(BooleanProperty disableAddProperty) {
        this.disableAddProperty = disableAddProperty;

        this.addButton.disableProperty().unbind();
        if (this.disableAddProperty != null) {
            this.addButton.disableProperty().bind(this.disableAddProperty);
        }
    }
}
