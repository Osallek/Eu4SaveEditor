package fr.osallek.eu4saveeditor.controller.control;

import fr.osallek.eu4parser.common.Eu4Utils;
import fr.osallek.eu4parser.model.game.IdeaGroup;
import fr.osallek.eu4parser.model.game.localisation.Eu4Language;
import fr.osallek.eu4parser.model.save.country.SaveCountry;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import fr.osallek.eu4saveeditor.controller.converter.IdeaGroupStringConverter;
import fr.osallek.eu4saveeditor.controller.object.Idea;
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
import javafx.util.converter.IntegerStringConverter;

public class TableView2Ideas extends TableView<Idea> {

    private final BooleanProperty disableAddProperty;

    private final ObservableList<IdeaGroup> ideaGroups;

    private final ObservableMap<Idea, ObservableList<IdeaGroup>> ideasMap = FXCollections.observableHashMap();

    public TableView2Ideas(SaveCountry country, ObservableList<Idea> enactedIdeas, ObservableList<IdeaGroup> ideaGroups) {
        this.ideaGroups = ideaGroups;
        TableColumn<Idea, IdeaGroup> ideaGroup = new TableColumn<>(country.getSave()
                                                                          .getGame()
                                                                          .getLocalisationCleanNoPunctuation("IDEA_TITLE", Eu4Language.getDefault()));
        ideaGroup.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getIdeaGroup()));
        ideaGroup.setCellFactory(UniqueComboBoxTableCell.forTableColumn(new IdeaGroupStringConverter(country.getSave().getGame()),
                                                                        this.ideasMap,
                                                                        Comparator.comparing(i -> Eu4SaveEditorUtils.localize(i.getName(),
                                                                                                                              country.getSave().getGame()),
                                                                                             Eu4Utils.COLLATOR),
                                                                        getItems(),
                                                                        this::getNewList
                                                                       ));
        ideaGroup.setOnEditCommit(event -> event.getRowValue().setIdeaGroup(event.getNewValue()));
        ideaGroup.setPrefWidth(250);
        ideaGroup.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<Idea, Integer> amount = new TableColumn<>(country.getSave()
                                                                     .getGame()
                                                                     .getLocalisationCleanNoPunctuation("LEDGER_IDEASUNLOCKED", Eu4Language.getDefault()));
        amount.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getLevel()));
        amount.setCellFactory(SpinnerTableCell.forTableColumn(0, ideaGroups.stream().mapToInt(i -> i.getIdeas().size()).max().orElse(0), 1,
                                                              new IntegerStringConverter(), idea -> idea == null || idea.getIdeaGroup().isFree()));
        amount.setOnEditCommit(event -> event.getRowValue().setLevel(event.getNewValue()));
        amount.setPrefWidth(250);
        amount.setEditable(true);
        amount.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<Idea, Void> remove = new TableColumn<>();
        remove.setPrefWidth(48);
        remove.setEditable(false);
        remove.setCellFactory(ClearCellFactory.forTableColumn(idea -> idea == null || idea.getIdeaGroup().isFree()));

        setFixedCellSize(40);
        setPrefWidth(550);
        setEditable(true);

        getColumns().clear();
        getColumns().add(ideaGroup);
        getColumns().add(amount);
        getColumns().add(remove);
        getItems().setAll(enactedIdeas.stream().map(Idea::new).collect(Collectors.toCollection(FXCollections::observableArrayList)));
        getItems().forEach(idea -> this.ideasMap.put(idea, getNewList()));

        this.disableAddProperty = new SimpleBooleanProperty(
                getItems().stream().filter(idea -> !idea.getIdeaGroup().isFree()).count() >= country.getAllowedIdeaGroups());

        getItems().addListener((ListChangeListener<? super Idea>) c ->
                this.disableAddProperty.setValue(c.getList().stream().filter(idea -> !idea.getIdeaGroup().isFree()).count() >= country.getAllowedIdeaGroups()));
    }

    private ObservableList<IdeaGroup> getNewList() {
        return this.ideaGroups.stream()
                              .filter(ideaGroup -> this.ideasMap.keySet().stream().noneMatch(p2 -> p2.getIdeaGroup().equals(ideaGroup)))
                              .filter(ideaGroup -> this.ideasMap.keySet().stream().noneMatch(i -> i.getIdeaGroup().isFree()) == ideaGroup.isFree())
                              .collect(Collectors.toCollection(FXCollections::observableArrayList));
    }

    public boolean isDisableAddProperty() {
        return disableAddProperty.get();
    }

    public BooleanProperty disableAddPropertyProperty() {
        return disableAddProperty;
    }
}
