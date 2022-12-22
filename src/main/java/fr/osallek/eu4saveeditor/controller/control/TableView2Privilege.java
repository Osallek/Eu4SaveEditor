package fr.osallek.eu4saveeditor.controller.control;

import fr.osallek.eu4parser.common.Eu4Utils;
import fr.osallek.eu4parser.model.game.EstatePrivilege;
import fr.osallek.eu4parser.model.game.localisation.Eu4Language;
import fr.osallek.eu4parser.model.save.country.SaveCountry;
import fr.osallek.eu4parser.model.save.country.SaveEstate;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import fr.osallek.eu4saveeditor.controller.converter.EstatePrivilegeStringConverter;
import fr.osallek.eu4saveeditor.controller.object.Privilege;
import java.time.LocalDate;
import java.util.Comparator;
import java.util.stream.Collectors;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;

public class TableView2Privilege extends TableView<Privilege> {

    private final BooleanProperty disableAddProperty;

    private final ObservableList<EstatePrivilege> estatePrivileges;

    private final ObservableMap<Privilege, ObservableList<EstatePrivilege>> privilegesMap = FXCollections.observableHashMap();

    public TableView2Privilege(SaveCountry country, SaveEstate estate, ObservableList<Privilege> enactedPrivileges,
                               ObservableList<EstatePrivilege> estatePrivileges) {
        this.estatePrivileges = estatePrivileges;
        TableColumn<Privilege, EstatePrivilege> type = new TableColumn<>(country.getSave()
                                                                                .getGame()
                                                                                .getLocalisationCleanNoPunctuation("PRIVILEGE_PICKER_TITLE", Eu4Language.getDefault()));
        type.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getPrivilege()));
        type.setCellFactory(UniqueComboBoxTableCell.forTableColumn(new EstatePrivilegeStringConverter(country.getSave().getGame()),
                                                                   this.privilegesMap,
                                                                   Comparator.comparing(p -> Eu4SaveEditorUtils.localize(p.getName(), country.getSave()
                                                                                                                                             .getGame()),
                                                                                        Eu4Utils.COLLATOR),
                                                                   getItems(),
                                                                   this::getNewList
                                                                  ));
        type.setOnEditCommit(event -> event.getRowValue().setPrivilege(event.getNewValue()));
        type.setPrefWidth(350);
        type.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<Privilege, LocalDate> startDate = new TableColumn<>(country.getSave()
                                                                               .getGame()
                                                                               .getLocalisationCleanNoPunctuation("FE_STARTING_DATE", Eu4Language.getDefault()));
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
        getItems().forEach(privilege -> this.privilegesMap.put(privilege, getNewList()));

        this.disableAddProperty = new SimpleBooleanProperty(false);

        getItems().addListener((ListChangeListener<? super Privilege>) c ->
                this.disableAddProperty.setValue(c.getList().size() >= country.getSave().getGame().getEstatePrivilegesMaxConcurrent() ||
                                                 c.getList().size() >= estate.getEstateGame().getPrivileges().size()));
    }

    private ObservableList<EstatePrivilege> getNewList() {
        return this.estatePrivileges.stream()
                                    .filter(p -> this.privilegesMap.keySet().stream().noneMatch(p2 -> p2.getPrivilege().equals(p)))
                                    .collect(Collectors.toCollection(FXCollections::observableArrayList));
    }

    public boolean isDisableAddProperty() {
        return disableAddProperty.get();
    }

    public BooleanProperty disableAddPropertyProperty() {
        return disableAddProperty;
    }
}
