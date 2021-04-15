package fr.osallek.eu4saveeditor.controller.control;

import fr.osallek.clausewitzparser.common.ClausewitzUtils;
import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4saveeditor.controller.converter.PercentStringConverter;
import fr.osallek.eu4saveeditor.controller.object.PriceChange;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.collections.FXCollections;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.util.converter.LocalDateStringConverter;

import java.time.LocalDate;
import java.util.Comparator;
import java.util.List;

public class TableView2PriceChange extends TableView<PriceChange> {

    public TableView2PriceChange(List<PriceChange> priceChanges, Save save) {
        TableColumn<PriceChange, String> name = new TableColumn<>(save.getGame().getLocalisation("LEDGER_NAME"));
        name.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getName()));
        name.setCellFactory(TextFieldTableCell.forTableColumn());
        name.setEditable(false);
        name.setPrefWidth(500);
        name.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<PriceChange, Integer> value = new TableColumn<>(save.getGame().getLocalisation("LEDGER_VALUE"));
        value.setCellValueFactory(p -> p.getValue() == null ? null :
                                       new SimpleIntegerProperty(p.getValue().getValue()).asObject());
        value.setCellFactory(TextFieldTableCell.forTableColumn(new PercentStringConverter()));
        value.setOnEditCommit(event -> event.getRowValue().setValue(event.getNewValue()));
        value.setPrefWidth(100);
        value.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<PriceChange, LocalDate> expiryDate = new TableColumn<>(save.getGame().getLocalisationCleanNoPunctuation("EXPIRES_ON"));
        expiryDate.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getExpiryDate()));
        expiryDate.setCellFactory(TextFieldTableCell.forTableColumn(new LocalDateStringConverter(ClausewitzUtils.DATE_FORMAT, ClausewitzUtils.DATE_FORMAT)));
        expiryDate.setOnEditCommit(event -> event.getRowValue().setExpiryDate(event.getNewValue()));
        expiryDate.setPrefWidth(100);
        expiryDate.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<PriceChange, Void> remove = new TableColumn<>();
        remove.setPrefWidth(48);
        remove.setEditable(false);
        remove.setCellFactory(ClearCellFactory.forTableColumn());

        setFixedCellSize(40);
        setPrefWidth(750);
        setEditable(true);

        getColumns().clear();
        getColumns().add(name);
        getColumns().add(value);
        getColumns().add(expiryDate);
        getColumns().add(remove);
        priceChanges.sort(Comparator.comparing(PriceChange::getExpiryDate));
        setItems(FXCollections.observableArrayList(priceChanges));
    }
}
