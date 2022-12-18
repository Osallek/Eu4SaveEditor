package fr.osallek.eu4saveeditor.controller.control;

import fr.osallek.eu4parser.common.Eu4Utils;
import fr.osallek.eu4parser.model.game.localisation.Eu4Language;
import fr.osallek.eu4parser.model.save.country.SaveCountry;
import fr.osallek.eu4saveeditor.controller.converter.CountryStringConverter;
import fr.osallek.eu4saveeditor.controller.object.Rival;
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

public class TableView2Rival extends TableView<Rival> {

    private final SaveCountry country;

    private final BooleanProperty disableAddProperty;

    private final ObservableList<SaveCountry> countriesAlive;

    private final ObservableMap<Rival, ObservableList<SaveCountry>> countriesMap = FXCollections.observableHashMap();

    public TableView2Rival(SaveCountry country, ObservableList<Rival> rivals, ObservableList<SaveCountry> countriesAlive) {
        this.country = country;
        this.countriesAlive = countriesAlive;

        TableColumn<Rival, SaveCountry> target = new TableColumn<>(country.getSave().getGame().getLocalisationClean("rival", Eu4Language.getDefault()));
        target.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getTarget()));
        target.setCellFactory(UniqueComboBoxTableCell.forTableColumn(new CountryStringConverter(),
                                                                     this.countriesMap,
                                                                     Comparator.comparing(SaveCountry::getLocalizedName, Eu4Utils.COLLATOR),
                                                                     getItems(),
                                                                     this::getNewList
                                                                    ));
        target.setOnEditCommit(event -> event.getRowValue().setTarget(event.getNewValue()));
        target.setPrefWidth(200);
        target.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<Rival, LocalDate> date = new TableColumn<>(country.getSave().getGame().getLocalisationCleanNoPunctuation("FE_STARTING_DATE", Eu4Language.getDefault()));
        date.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getDate()));
        date.setCellFactory(DatePickerCell.forTableColumn(null, country.getSave().getDate()));
        date.setOnEditCommit(event -> event.getRowValue().setDate(event.getNewValue()));
        date.setPrefWidth(150);
        date.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<Rival, Void> remove = new TableColumn<>();
        remove.setPrefWidth(48);
        remove.setEditable(false);
        remove.setCellFactory(ClearCellFactory.forTableColumn());

        setFixedCellSize(40);
        setPrefWidth(413);
        setEditable(true);

        getColumns().clear();
        getColumns().add(target);
        getColumns().add(date);
        getColumns().add(remove);
        setItems(rivals.stream().map(Rival::new).collect(Collectors.toCollection(FXCollections::observableArrayList)));
        getItems().forEach(rival -> this.countriesMap.put(rival, getNewList()));

        this.disableAddProperty = new SimpleBooleanProperty(getItems().size() >= country.getSave().getGame().getNumPossibleRivals() ||
                                                            getItems().size() >= countriesAlive.size() - 1);

        getItems().addListener((ListChangeListener<? super Rival>) c -> {
            this.disableAddProperty.setValue(c.getList().size() >= country.getSave().getGame().getNumPossibleRivals() ||
                                             c.getList().size() >= countriesAlive.size() - 1); // -1 for current country
        });
    }

    private ObservableList<SaveCountry> getNewList() {
        return this.countriesAlive.stream()
                                  .filter(c -> !c.equals(this.country))
                                  .filter(c -> getItems().stream().noneMatch(rival -> rival.getTarget().equals(c)))
                                  .collect(Collectors.toCollection(FXCollections::observableArrayList));
    }

    public boolean isDisableAddProperty() {
        return disableAddProperty.get();
    }

    public BooleanProperty disableAddPropertyProperty() {
        return disableAddProperty;
    }
}
