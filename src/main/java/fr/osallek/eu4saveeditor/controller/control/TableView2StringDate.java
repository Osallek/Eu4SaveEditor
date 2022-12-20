package fr.osallek.eu4saveeditor.controller.control;

import fr.osallek.eu4parser.model.game.localisation.Eu4Language;
import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import fr.osallek.eu4saveeditor.controller.object.StringDate;
import java.time.LocalDate;
import java.util.stream.Collectors;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;

public class TableView2StringDate extends TableView<StringDate> {

    public TableView2StringDate(Save save, ObservableList<StringDate> stringDates, boolean dateEditable, LocalDate startDate, LocalDate endDate) {
        TableColumn<StringDate, String> name = new TableColumn<>(save.getGame().getLocalisationClean("NAME", Eu4Language.getDefault()));
        name.setCellValueFactory(p -> p.getValue() == null ? null :
                                      new ReadOnlyObjectWrapper<>(Eu4SaveEditorUtils.localize(p.getValue().getName(), save.getGame())));
        name.setEditable(false);
        name.setPrefWidth(450);
        name.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<StringDate, LocalDate> date = new TableColumn<>(save.getGame()
                                                                        .getLocalisationCleanNoPunctuation("FE_STARTING_DATE", Eu4Language.getDefault()));
        date.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getDate()));
        date.setCellFactory(DatePickerCell.forTableColumn(startDate, endDate));
        date.setOnEditCommit(event -> event.getRowValue().setDate(event.getNewValue()));
        date.setPrefWidth(150);
        date.setStyle("-fx-alignment: CENTER-LEFT");
        date.setEditable(dateEditable);

        TableColumn<StringDate, Void> remove = new TableColumn<>();
        remove.setPrefWidth(48);
        remove.setEditable(false);
        remove.setCellFactory(ClearCellFactory.forTableColumn());

        setFixedCellSize(40);
        setPrefWidth(663);
        setEditable(true);

        getColumns().clear();
        getColumns().add(name);
        getColumns().add(date);
        getColumns().add(remove);
        setItems(stringDates.stream().map(StringDate::new).collect(Collectors.toCollection(FXCollections::observableArrayList)));
    }
}
