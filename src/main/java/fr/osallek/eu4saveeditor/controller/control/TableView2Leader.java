package fr.osallek.eu4saveeditor.controller.control;

import fr.osallek.eu4parser.common.Eu4Utils;
import fr.osallek.eu4parser.model.game.LeaderPersonality;
import fr.osallek.eu4parser.model.save.country.Country;
import fr.osallek.eu4parser.model.save.country.LeaderType;
import fr.osallek.eu4saveeditor.controller.converter.LeaderPersonalityStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.LeaderTypeStringConverter;
import fr.osallek.eu4saveeditor.controller.object.Leader;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.ComboBoxTableCell;
import javafx.util.converter.IntegerStringConverter;

import java.time.LocalDate;
import java.util.Comparator;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class TableView2Leader extends TableView<Leader> {

    public TableView2Leader(Country country, ObservableList<Leader> leaders) {
        TableColumn<Leader, String> name = new TableColumn<>(country.getSave().getGame().getLocalisation("NAME"));
        name.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getName()));
        name.setCellFactory(TextFieldTableCell.forTableColumn());
        name.setOnEditCommit(event -> event.getRowValue().setName(event.getNewValue()));
        name.setPrefWidth(150);
        name.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<Leader, LeaderType> type = new TableColumn<>(country.getSave().getGame().getLocalisationCleanNoPunctuation("LEDGER_TYPE"));
        type.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getType()));
        type.setCellFactory(ComboBoxTableCell.forTableColumn(new LeaderTypeStringConverter(country.getSave().getGame()), LeaderType.values()));
        type.setOnEditCommit(event -> event.getRowValue().setType(event.getNewValue()));
        type.setPrefWidth(100);
        type.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<Leader, Integer> fire = new TableColumn<>(country.getSave().getGame().getLocalisationCleanNoPunctuation("FIRE"));
        fire.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getFire()));
        fire.setCellFactory(SpinnerTableCell.forTableColumn(0, 6, 1, new IntegerStringConverter()));
        fire.setOnEditCommit(event -> event.getRowValue().setFire(event.getNewValue()));
        fire.setPrefWidth(50);
        fire.setEditable(true);
        fire.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<Leader, Integer> shock = new TableColumn<>(country.getSave().getGame().getLocalisationCleanNoPunctuation("SHOCK"));
        shock.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getShock()));
        shock.setCellFactory(SpinnerTableCell.forTableColumn(0, 6, 1, new IntegerStringConverter()));
        shock.setOnEditCommit(event -> event.getRowValue().setShock(event.getNewValue()));
        shock.setPrefWidth(50);
        shock.setEditable(true);
        shock.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<Leader, Integer> maneuever = new TableColumn<>(country.getSave().getGame().getLocalisationCleanNoPunctuation("MANEUEVER"));
        maneuever.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getManuever()));
        maneuever.setCellFactory(SpinnerTableCell.forTableColumn(0, 6, 1, new IntegerStringConverter()));
        maneuever.setOnEditCommit(event -> event.getRowValue().setManuever(event.getNewValue()));
        maneuever.setPrefWidth(100);
        maneuever.setEditable(true);
        maneuever.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<Leader, Integer> siege = new TableColumn<>(country.getSave().getGame().getLocalisationCleanNoPunctuation("SIEGE"));
        siege.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getSiege()));
        siege.setCellFactory(SpinnerTableCell.forTableColumn(0, 6, 1, new IntegerStringConverter()));
        siege.setOnEditCommit(event -> event.getRowValue().setSiege(event.getNewValue()));
        siege.setPrefWidth(50);
        siege.setEditable(true);
        siege.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<Leader, LeaderPersonality> personality = new TableColumn<>(country.getSave().getGame().getLocalisationCleanNoPunctuation("PERSONALITY"));
        personality.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getPersonality()));
        personality.setCellFactory(ComboBoxTableCell.forTableColumn(new LeaderPersonalityStringConverter(),
                                                                    Stream.concat(country.getSave().getGame().getLeaderPersonalities().stream(),
                                                                                  Stream.of((LeaderPersonality) null))
                                                                          .sorted(Comparator.nullsFirst(
                                                                                  Comparator.comparing(LeaderPersonality::getLocalizedName, Eu4Utils.COLLATOR)))
                                                                          .collect(Collectors.toCollection(FXCollections::observableArrayList))));
        personality.setOnEditCommit(event -> event.getRowValue().setPersonality(event.getNewValue()));
        personality.setPrefWidth(200);
        personality.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<Leader, LocalDate> birthDate = new TableColumn<>(country.getSave().getGame().getLocalisationCleanNoPunctuation("DATE_OF_BIRTH_REQUIRED"));
        birthDate.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getBirthDate()));
        birthDate.setCellFactory(DatePickerCell.forTableColumn(null, country.getSave().getDate()));
        birthDate.setOnEditCommit(event -> event.getRowValue().setBirthDate(event.getNewValue()));
        birthDate.setPrefWidth(150);
        birthDate.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<Leader, Void> remove = new TableColumn<>();
        remove.setPrefWidth(48);
        remove.setEditable(false);
        remove.setCellFactory(ClearCellFactory.forTableColumn());

        setFixedCellSize(40);
        setPrefWidth(900);
        setEditable(true);

        getColumns().clear();
        getColumns().add(name);
        getColumns().add(type);
        getColumns().add(fire);
        getColumns().add(shock);
        getColumns().add(maneuever);
        getColumns().add(siege);
        getColumns().add(personality);
        getColumns().add(birthDate);
        getColumns().add(remove);
        setItems(leaders.stream().map(Leader::new).collect(Collectors.toCollection(FXCollections::observableArrayList)));
    }
}
