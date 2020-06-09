package com.osallek.eu4saveeditor.controller.control;

import com.osallek.clausewitzparser.common.ClausewitzUtils;
import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.changeprices.ChangePrice;
import com.osallek.eu4saveeditor.controller.converter.ChangePriceCellFactory;
import com.osallek.eu4saveeditor.controller.converter.PercentStringConverter;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.util.converter.DateStringConverter;
import org.controlsfx.control.tableview2.TableColumn2;
import org.controlsfx.control.tableview2.TableView2;
import org.controlsfx.control.tableview2.cell.TextField2TableCell;

import java.util.Comparator;
import java.util.Date;
import java.util.List;

public class TableView2ChangePrice extends TableView2<ChangePrice> {

    public TableView2ChangePrice(List<ChangePrice> changePrices, Save save) {
        TableColumn2<ChangePrice, String> name = new TableColumn2<>(save.getGame().getLocalisation("LEDGER_NAME"));
        name.setCellValueFactory(p -> p.getValue() == null ? null :
                                      new SimpleStringProperty(p.getValue().getLocalizedName()));
        name.setCellFactory(TextField2TableCell.forTableColumn());
        name.setEditable(false);
        name.setPrefWidth(500);

        TableColumn2<ChangePrice, Integer> value = new TableColumn2<>(save.getGame().getLocalisation("LEDGER_VALUE"));
        value.setCellValueFactory(p -> p.getValue() == null ? null :
                                       new SimpleIntegerProperty(p.getValue().getValue()).asObject());
        value.setCellFactory(TextField2TableCell.forTableColumn(new PercentStringConverter()));
        value.setOnEditCommit(event -> event.getRowValue().setValue(event.getNewValue()));
        value.setPrefWidth(100);

        TableColumn2<ChangePrice, Date> expiryDate = new TableColumn2<>(save.getGame()
                                                                            .getLocalisationCleanNoPunctuation("EXPIRES_ON"));
        expiryDate.setCellValueFactory(p -> p.getValue() == null ? null :
                                            new ReadOnlyObjectWrapper<>(p.getValue().getExpiryDate()));
        expiryDate.setCellFactory(TextField2TableCell.forTableColumn(new DateStringConverter(ClausewitzUtils.DATE_FORMAT)));
        expiryDate.setOnEditCommit(event -> event.getRowValue().setExpiryDate(event.getNewValue()));
        expiryDate.setPrefWidth(100);

        TableColumn2<ChangePrice, Void> remove = new TableColumn2<>();
        remove.setPrefWidth(48);
        remove.setEditable(false);
        remove.setCellFactory(new ChangePriceCellFactory());

        setFixedCellSize(40);
        setPrefWidth(750);
        setEditable(true);

        getColumns().clear();
        getColumns().add(name);
        getColumns().add(value);
        getColumns().add(expiryDate);
        getColumns().add(remove);
        changePrices.sort(Comparator.comparing(ChangePrice::getExpiryDate));
        setItems(FXCollections.observableArrayList(changePrices));
    }
}
