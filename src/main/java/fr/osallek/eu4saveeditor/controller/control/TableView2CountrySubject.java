package fr.osallek.eu4saveeditor.controller.control;

import fr.osallek.eu4parser.model.game.SubjectType;
import fr.osallek.eu4parser.model.game.localisation.Eu4Language;
import fr.osallek.eu4parser.model.save.country.SaveCountry;
import fr.osallek.eu4saveeditor.controller.converter.CountryStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.SubjectTypeStringConverter;
import fr.osallek.eu4saveeditor.controller.object.CountrySubject;
import java.time.LocalDate;
import java.util.stream.Collectors;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.ComboBoxTableCell;

public class TableView2CountrySubject extends TableView<CountrySubject> {

    public TableView2CountrySubject(SaveCountry country, ObservableList<CountrySubject> countrySubjectsField, ObservableList<SaveCountry> countriesAlive,
                                    ObservableList<SubjectType> subjectTypes) {
        TableColumn<CountrySubject, SaveCountry> subject = new TableColumn<>(country.getSave().getGame().getLocalisationClean("LEDGER_SUBJECT", Eu4Language.getDefault()));
        subject.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getSubject()));
        subject.setCellFactory(ComboBoxTableCell.forTableColumn(new CountryStringConverter(),
                                                                countriesAlive.filtered(c -> !c.equals(country) && !country.getSubjects().contains(c))));
        subject.setOnEditCommit(event -> event.getRowValue().setSubject(event.getNewValue()));
        subject.setPrefWidth(250);
        subject.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<CountrySubject, SubjectType> type = new TableColumn<>(country.getSave()
                                                                                 .getGame()
                                                                                 .getLocalisationCleanNoPunctuation("LIBERTY_DESIRE_FROM_SUBJECT_TYPE", Eu4Language.getDefault()));
        type.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getSubjectType()));
        type.setCellFactory(ComboBoxTableCell.forTableColumn(new SubjectTypeStringConverter(country.getSave().getGame()), subjectTypes));
        type.setOnEditCommit(event -> event.getRowValue().setSubjectType(event.getNewValue()));
        type.setPrefWidth(200);
        type.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<CountrySubject, LocalDate> startDate = new TableColumn<>(country.getSave()
                                                                                    .getGame()
                                                                                    .getLocalisationCleanNoPunctuation("FE_STARTING_DATE", Eu4Language.getDefault()));
        startDate.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getStartDate()));
        startDate.setCellFactory(column -> new DatePickerCell<>(country.getSave().getGame().getStartDate(), country.getSave().getDate()));
        startDate.setOnEditCommit(event -> event.getRowValue().setStartDate(event.getNewValue()));
        startDate.setPrefWidth(150);
        startDate.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<CountrySubject, Void> remove = new TableColumn<>();
        remove.setPrefWidth(48);
        remove.setEditable(false);
        remove.setCellFactory(ClearCellFactory.forTableColumn());

        setFixedCellSize(40);
        setPrefWidth(650);
        setEditable(true);

        getColumns().clear();
        getColumns().add(subject);
        getColumns().add(type);
        getColumns().add(startDate);
        getColumns().add(remove);
        setItems(countrySubjectsField.stream().map(CountrySubject::new).collect(Collectors.toCollection(FXCollections::observableArrayList)));
    }
}
