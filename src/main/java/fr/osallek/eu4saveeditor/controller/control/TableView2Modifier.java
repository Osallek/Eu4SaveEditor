package fr.osallek.eu4saveeditor.controller.control;

import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4saveeditor.controller.object.Modifier;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;

import java.time.LocalDate;
import java.util.stream.Collectors;

public class TableView2Modifier extends TableView<Modifier> {

    public TableView2Modifier(Save save, ObservableList<Modifier> modifiers) {
        TableColumn<Modifier, String> name = new TableColumn<>(save.getGame().getLocalisation("NAME"));
        name.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getModifier().getLocalizedName()));
        name.setEditable(false);
        name.setPrefWidth(450);
        name.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<Modifier, LocalDate> birthDate = new TableColumn<>(save.getGame().getLocalisationCleanNoPunctuation("EXPIRES_ON"));
        birthDate.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getDate()));
        birthDate.setCellFactory(DatePickerCell.forTableColumn(save.getDate(), null));
        birthDate.setOnEditCommit(event -> event.getRowValue().setDate(event.getNewValue()));
        birthDate.setPrefWidth(150);
        birthDate.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<Modifier, Void> remove = new TableColumn<>();
        remove.setPrefWidth(48);
        remove.setEditable(false);
        remove.setCellFactory(ClearCellFactory.forTableColumn());

        setFixedCellSize(40);
        setPrefWidth(650);
        setEditable(true);

        getColumns().clear();
        getColumns().add(name);
        getColumns().add(birthDate);
        getColumns().add(remove);
        setItems(modifiers.stream().map(Modifier::new).collect(Collectors.toCollection(FXCollections::observableArrayList)));
    }
}
