package com.osallek.eu4saveeditor.controller.control;

import com.osallek.clausewitzparser.common.ClausewitzUtils;
import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.changeprices.ChangePrice;
import com.osallek.eu4saveeditor.controller.converter.ClearCellFactory;
import com.osallek.eu4saveeditor.controller.converter.PercentStringConverter;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.util.converter.LocalDateStringConverter;

import java.time.LocalDate;
import java.util.Comparator;
import java.util.List;

public class TableView2ChangePrice extends TableView<ChangePrice> {

    public TableView2ChangePrice(List<ChangePrice> changePrices, Save save) {
        TableColumn<ChangePrice, String> name = new TableColumn<>(save.getGame().getLocalisation("LEDGER_NAME"));
        name.setCellValueFactory(p -> p.getValue() == null ? null :
                                      new SimpleStringProperty(p.getValue().getLocalizedName()));
        name.setCellFactory(TextFieldTableCell.forTableColumn());
        name.setEditable(false);
        name.setPrefWidth(500);

        TableColumn<ChangePrice, Integer> value = new TableColumn<>(save.getGame().getLocalisation("LEDGER_VALUE"));
        value.setCellValueFactory(p -> p.getValue() == null ? null :
                                       new SimpleIntegerProperty(p.getValue().getValue()).asObject());
        value.setCellFactory(TextFieldTableCell.forTableColumn(new PercentStringConverter()));
        value.setOnEditCommit(event -> event.getRowValue().setValue(event.getNewValue()));
        value.setPrefWidth(100);

        TableColumn<ChangePrice, LocalDate> expiryDate = new TableColumn<>(save.getGame().getLocalisationCleanNoPunctuation("EXPIRES_ON"));
        expiryDate.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getExpiryDate()));
        expiryDate.setCellFactory(TextFieldTableCell.forTableColumn(new LocalDateStringConverter(ClausewitzUtils.DATE_FORMAT, ClausewitzUtils.DATE_FORMAT)));
        expiryDate.setOnEditCommit(event -> event.getRowValue().setExpiryDate(event.getNewValue()));
        expiryDate.setPrefWidth(100);

        TableColumn<ChangePrice, Void> remove = new TableColumn<>();
        remove.setPrefWidth(48);
        remove.setEditable(false);
        remove.setCellFactory(new ClearCellFactory<>());

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
