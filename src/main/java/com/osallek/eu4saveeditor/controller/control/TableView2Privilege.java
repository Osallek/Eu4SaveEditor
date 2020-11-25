package com.osallek.eu4saveeditor.controller.control;

import com.osallek.eu4parser.common.Eu4Utils;
import com.osallek.eu4parser.model.game.EstatePrivilege;
import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4parser.model.save.country.SaveEstate;
import com.osallek.eu4saveeditor.controller.converter.EstatePrivilegeStringConverter;
import com.osallek.eu4saveeditor.controller.object.Privilege;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.transformation.SortedList;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;

import java.time.LocalDate;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class TableView2Privilege extends TableView<Privilege> {

    private final BooleanProperty disableAddProperty;

    private final ObservableList<EstatePrivilege> estatePrivileges;

    private final Map<Privilege, ObservableList<EstatePrivilege>> privilegesMapSource = new HashMap<>();

    private final Map<Privilege, SortedList<EstatePrivilege>> privilegesMap = new HashMap<>();

    public TableView2Privilege(Country country, SaveEstate estate, ObservableList<Privilege> enactedPrivileges,
                               ObservableList<EstatePrivilege> estatePrivileges) {
        this.estatePrivileges = estatePrivileges;
        TableColumn<Privilege, EstatePrivilege> type = new TableColumn<>(country.getSave()
                                                                                .getGame()
                                                                                .getLocalisationCleanNoPunctuation("PRIVILEGE_PICKER_TITLE"));
        type.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getPrivilege()));
        type.setCellFactory(UniqueComboBoxTableCell.forTableColumn(new EstatePrivilegeStringConverter(), this.privilegesMap));
        type.setOnEditStart(event -> this.privilegesMapSource.get(event.getRowValue()).removeAll(getItems().stream()
                                                                                                           .filter(p -> !p.equals(event.getRowValue()))
                                                                                                           .map(Privilege::getPrivilege)
                                                                                                           .collect(Collectors.toList())));
        type.setOnEditCommit(event -> {
            this.privilegesMapSource.forEach((privilege, privileges) -> {
                if (!privileges.contains(event.getOldValue())) {
                    privileges.add(event.getOldValue());
                }

                if (!privilege.equals(event.getRowValue())) {
                    privileges.remove(event.getNewValue());
                }
            });
            event.getRowValue().setPrivilege(event.getNewValue());
        });
        type.setPrefWidth(350);
        type.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<Privilege, LocalDate> startDate = new TableColumn<>(country.getSave()
                                                                               .getGame()
                                                                               .getLocalisationCleanNoPunctuation("FE_STARTING_DATE"));
        startDate.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getStartDate()));
        startDate.setCellFactory(column -> new DatePickerCell<>(country.getSave().getGame().getStartDate(), country.getSave().getDate()));
        startDate.setOnEditCommit(event -> event.getRowValue().setStartDate(event.getNewValue()));
        startDate.setPrefWidth(150);
        startDate.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<Privilege, Void> remove = new TableColumn<>();
        remove.setPrefWidth(48);
        remove.setEditable(false);
        remove.setCellFactory(ClearCellFactory.forTableColumn());

        setFixedCellSize(40);
        setPrefWidth(550);
        setEditable(true);

        getColumns().clear();
        getColumns().add(type);
        getColumns().add(startDate);
        getColumns().add(remove);
        getItems().setAll(enactedPrivileges.stream().map(Privilege::new).collect(Collectors.toCollection(FXCollections::observableArrayList)));
        getItems().forEach(privilege -> this.privilegesMapSource.put(privilege, getNewList()));
        this.privilegesMapSource.forEach((privilege, privileges) ->
                                                 this.privilegesMap.put(privilege, privileges.sorted(
                                                         Comparator.comparing(EstatePrivilege::getLocalizedName, Eu4Utils.COLLATOR))));

        this.disableAddProperty = new SimpleBooleanProperty(false);

        getItems().addListener((ListChangeListener<? super Privilege>) c -> {
            this.disableAddProperty.setValue(c.getList().size() >= country.getSave().getGame().getEstatePrivilegesMaxConcurrent() ||
                                             c.getList().size() >= estate.getEstateGame().getPrivileges().size());

            while (c.next()) {
                c.getRemoved().forEach(privilege -> {
                    this.privilegesMapSource.remove(privilege);
                    this.privilegesMap.remove(privilege);
                    this.privilegesMapSource.values().forEach(privileges -> privileges.add(privilege.getPrivilege()));
                });
                c.getAddedSubList().forEach(privilege -> {
                    this.privilegesMapSource.values().forEach(privileges -> privileges.remove(privilege.getPrivilege()));
                    ObservableList<EstatePrivilege> privileges = getNewList();
                    this.privilegesMapSource.put(privilege, privileges);
                    this.privilegesMap.put(privilege, privileges.sorted(Comparator.comparing(EstatePrivilege::getLocalizedName, Eu4Utils.COLLATOR)));
                });
            }
        });
    }

    private ObservableList<EstatePrivilege> getNewList() {
        return this.estatePrivileges.stream()
                                    .filter(p -> this.privilegesMapSource.keySet().stream().noneMatch(p2 -> p2.getPrivilege().equals(p)))
                                    .collect(Collectors.toCollection(FXCollections::observableArrayList));
    }

    public boolean isDisableAddProperty() {
        return disableAddProperty.get();
    }

    public BooleanProperty disableAddPropertyProperty() {
        return disableAddProperty;
    }
}
