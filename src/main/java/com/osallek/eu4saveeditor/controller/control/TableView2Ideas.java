package com.osallek.eu4saveeditor.controller.control;

import com.osallek.eu4parser.common.Eu4Utils;
import com.osallek.eu4parser.model.game.IdeaGroup;
import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4saveeditor.controller.converter.IdeaGroupStringConverter;
import com.osallek.eu4saveeditor.controller.object.Idea;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.transformation.SortedList;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.util.converter.IntegerStringConverter;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class TableView2Ideas extends TableView<Idea> {

    private final BooleanProperty disableAddProperty;

    private final ObservableList<IdeaGroup> ideaGroups;

    private final Map<Idea, ObservableList<IdeaGroup>> ideasMapSource = new HashMap<>();

    private final Map<Idea, SortedList<IdeaGroup>> ideasMap = new HashMap<>();

    public TableView2Ideas(Country country, ObservableList<Idea> enactedIdeas, ObservableList<IdeaGroup> ideaGroups) {
        this.ideaGroups = ideaGroups;
        TableColumn<Idea, IdeaGroup> ideaGroup = new TableColumn<>(country.getSave().getGame().getLocalisationCleanNoPunctuation("IDEA_TITLE"));
        ideaGroup.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getIdeaGroup()));
        ideaGroup.setCellFactory(UniqueComboBoxTableCell.forTableColumn(new IdeaGroupStringConverter(), this.ideasMap));
        ideaGroup.setOnEditStart(event -> this.ideasMapSource.get(event.getRowValue()).removeAll(getItems().stream()
                                                                                                           .filter(p -> !p.equals(event.getRowValue()))
                                                                                                           .map(Idea::getIdeaGroup)
                                                                                                           .collect(Collectors.toList())));
        ideaGroup.setOnEditCommit(event -> {
            this.ideasMapSource.forEach((idea, ideas) -> {
                if (!ideas.contains(event.getOldValue())) {
                    ideas.add(event.getOldValue());
                }

                if (!idea.equals(event.getRowValue())) {
                    ideas.remove(event.getNewValue());
                }
            });
            event.getRowValue().setIdeaGroup(event.getNewValue());
        });
        ideaGroup.setPrefWidth(250);
        ideaGroup.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<Idea, Integer> amount = new TableColumn<>(country.getSave().getGame().getLocalisationCleanNoPunctuation("LEDGER_IDEASUNLOCKED"));
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
        getItems().forEach(idea -> this.ideasMapSource.put(idea, getNewList(this.ideasMapSource.keySet().stream().noneMatch(i -> i.getIdeaGroup().isFree()))));
        this.ideasMapSource.forEach((idea, ideas) ->
                                            this.ideasMap.put(idea, ideas.sorted(Comparator.comparing(IdeaGroup::getLocalizedName, Eu4Utils.COLLATOR))));

        this.disableAddProperty = new SimpleBooleanProperty(getItems().stream().filter(idea -> !idea.getIdeaGroup().isFree()).count()
                                                            >= country.getAllowedIdeaGroups());

        getItems().addListener((ListChangeListener<? super Idea>) c -> {
            this.disableAddProperty.setValue(c.getList().stream().filter(idea -> !idea.getIdeaGroup().isFree()).count() >= country.getAllowedIdeaGroups());

            while (c.next()) {
                c.getRemoved().forEach(idea -> {
                    this.ideasMapSource.remove(idea);
                    this.ideasMap.remove(idea);
                    this.ideasMapSource.values().forEach(ideas -> ideas.add(idea.getIdeaGroup()));
                });
                c.getAddedSubList().forEach(idea -> {
                    this.ideasMapSource.values().forEach(ideas -> ideas.remove(idea.getIdeaGroup()));
                    ObservableList<IdeaGroup> ideas = getNewList(this.ideasMapSource.keySet().stream().noneMatch(i -> i.getIdeaGroup().isFree()));
                    this.ideasMapSource.put(idea, ideas);
                    this.ideasMap.put(idea, ideas.sorted(Comparator.comparing(IdeaGroup::getLocalizedName, Eu4Utils.COLLATOR)));
                });
            }
        });
    }

    private ObservableList<IdeaGroup> getNewList(boolean isFree) {
        return this.ideaGroups.stream()
                              .filter(ideaGroup -> this.ideasMapSource.keySet().stream().noneMatch(p2 -> p2.getIdeaGroup().equals(ideaGroup)))
                              .filter(ideaGroup -> isFree == ideaGroup.isFree())
                              .collect(Collectors.toCollection(FXCollections::observableArrayList));
    }

    public boolean isDisableAddProperty() {
        return disableAddProperty.get();
    }

    public BooleanProperty disableAddPropertyProperty() {
        return disableAddProperty;
    }
}
