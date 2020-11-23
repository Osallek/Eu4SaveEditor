package com.osallek.eu4saveeditor.controller.control;

import com.osallek.eu4parser.common.Eu4Utils;
import com.osallek.eu4parser.model.game.RulerPersonality;
import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4parser.model.save.country.Monarch;
import com.osallek.eu4saveeditor.controller.converter.RulerPersonalityStringConverter;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.transformation.SortedList;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import org.apache.commons.collections4.CollectionUtils;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class TableView2Personalities extends TableView<RulerPersonality> {

    private final BooleanProperty disableAddProperty;

    private final ObservableList<RulerPersonality> personalities;

    private final Map<RulerPersonality, ObservableList<RulerPersonality>> personalitiesMapSource = new HashMap<>();

    private final Map<RulerPersonality, SortedList<RulerPersonality>> personalitiesMap = new HashMap<>();

    public TableView2Personalities(Country country, Monarch monarch, ObservableList<RulerPersonality> enactedPersonalities,
                                   ObservableList<RulerPersonality> personalities) {
        this.personalities = personalities;
        TableColumn<RulerPersonality, RulerPersonality> rulerPersonality = new TableColumn<>(country.getSave()
                                                                                                    .getGame()
                                                                                                    .getLocalisationCleanNoPunctuation("LEDGER_PERSONALITIES"));
        rulerPersonality.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue()));
        rulerPersonality.setCellFactory(CustomComboBoxTableCell.forTableColumn(new RulerPersonalityStringConverter(), this.personalitiesMap));
        rulerPersonality.setOnEditStart(event -> this.personalitiesMapSource.get(event.getRowValue()).removeAll(getItems().stream()
                                                                                                                          .filter(p -> !p.equals(
                                                                                                                                  event.getRowValue()))
                                                                                                                          .collect(Collectors.toList())));
        rulerPersonality.setOnEditCommit(event -> {
            this.personalitiesMapSource.forEach((idea, ideas) -> {
                if (!ideas.contains(event.getOldValue())) {
                    ideas.add(event.getOldValue());
                }

                if (!idea.equals(event.getRowValue())) {
                    ideas.remove(event.getNewValue());
                }
            });

            event.getTableView().getItems().set(event.getTablePosition().getRow(), event.getNewValue());
        });
        rulerPersonality.setPrefWidth(250);
        rulerPersonality.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<RulerPersonality, Void> remove = new TableColumn<>();
        remove.setPrefWidth(48);
        remove.setEditable(false);
        remove.setCellFactory(ClearCellFactory.forTableColumn());

        setFixedCellSize(40);
        setPrefWidth(300);
        setEditable(true);

        getColumns().clear();
        getColumns().add(rulerPersonality);
        getColumns().add(remove);
        getItems().setAll(enactedPersonalities.stream().map(RulerPersonality::new).collect(Collectors.toList()));
        getItems().forEach(idea -> this.personalitiesMapSource.put(idea, getNewList(monarch)));
        this.personalitiesMapSource.forEach((personality, rulerPersonalities) ->
                                                    this.personalitiesMap.put(personality, rulerPersonalities.sorted(
                                                            Comparator.comparing(RulerPersonality::getLocalizedName, Eu4Utils.COLLATOR))));

        this.disableAddProperty = new SimpleBooleanProperty(getItems().size() >= country.getSave().getGame().getMaxExtraPersonalities() + 1
                                                            && CollectionUtils.isNotEmpty(getNewList(monarch)));

        getItems().addListener((ListChangeListener<? super RulerPersonality>) c -> {
            this.disableAddProperty.setValue(c.getList().size() >= country.getSave().getGame().getMaxExtraPersonalities() + 1
                                             && CollectionUtils.isNotEmpty(getNewList(monarch)));

            while (c.next()) {
                c.getRemoved().forEach(personality -> {
                    this.personalitiesMapSource.remove(personality);
                    this.personalitiesMap.remove(personality);
                    this.personalitiesMapSource.values().forEach(rulerPersonalities -> rulerPersonalities.add(personality));
                });
                c.getAddedSubList().forEach(personality -> {
                    this.personalitiesMapSource.values().forEach(rulerPersonalities -> rulerPersonalities.remove(personality));
                    ObservableList<RulerPersonality> rulerPersonalities = getNewList(monarch);
                    this.personalitiesMapSource.put(personality, rulerPersonalities);
                    this.personalitiesMap.put(personality,
                                              rulerPersonalities.sorted(Comparator.comparing(RulerPersonality::getLocalizedName, Eu4Utils.COLLATOR)));
                });
            }
        });
    }

    private ObservableList<RulerPersonality> getNewList(Monarch monarch) {
        return this.personalities.stream()
                                 .filter(personality -> this.personalitiesMapSource.keySet().stream().noneMatch(p2 -> p2.equals(personality)))
                                 .filter(personality -> personality.isMonarchValid(monarch))
                                 .filter(personality -> personality.isMonarchValid(monarch))
                                 .collect(Collectors.toCollection(FXCollections::observableArrayList));
    }

    public boolean isDisableAddProperty() {
        return disableAddProperty.get();
    }

    public BooleanProperty disableAddPropertyProperty() {
        return disableAddProperty;
    }
}
