package fr.osallek.eu4saveeditor.controller.control;

import fr.osallek.eu4parser.common.Eu4Utils;
import fr.osallek.eu4parser.model.save.country.Country;
import fr.osallek.eu4parser.model.save.country.Monarch;
import fr.osallek.eu4saveeditor.controller.converter.PersonalityStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.RulerPersonalityStringConverter;
import fr.osallek.eu4saveeditor.controller.object.Personality;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import org.apache.commons.collections4.CollectionUtils;

import java.util.Comparator;
import java.util.stream.Collectors;

public class TableView2Personalities extends TableView<Personality> {

    private final BooleanProperty disableAddProperty;

    private final ObservableList<Personality> personalities;

    private final Monarch monarch;

    private final ObservableMap<Personality, ObservableList<Personality>> personalitiesMap = FXCollections.observableHashMap();

    public TableView2Personalities(Country country, Monarch monarch, ObservableList<Personality> enactedPersonalities,
                                   ObservableList<Personality> personalities) {
        this.personalities = personalities;
        this.monarch = monarch;
        TableColumn<Personality, Personality> rulerPersonality = new TableColumn<>(country.getSave()
                                                                                                    .getGame()
                                                                                                    .getLocalisationCleanNoPunctuation("LEDGER_PERSONALITIES"));
        rulerPersonality.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue()));
        rulerPersonality.setCellFactory(UniqueComboBoxTableCell.forTableColumn(new PersonalityStringConverter(),
                                                                               this.personalitiesMap,
                                                                               Comparator.comparing(p -> p.getRulerPersonality().getLocalizedName(), Eu4Utils.COLLATOR),
                                                                               getItems(),
                                                                               this::getNewList
                                                                              ));
        rulerPersonality.setOnEditCommit(event -> event.getTableView().getItems().set(event.getTablePosition().getRow(), event.getNewValue()));
        rulerPersonality.setPrefWidth(250);
        rulerPersonality.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<Personality, Void> remove = new TableColumn<>();
        remove.setPrefWidth(48);
        remove.setEditable(false);
        remove.setCellFactory(ClearCellFactory.forTableColumn());

        setFixedCellSize(40);
        setPrefWidth(300);
        setEditable(true);

        getColumns().clear();
        getColumns().add(rulerPersonality);
        getColumns().add(remove);
        getItems().setAll(enactedPersonalities.stream().map(Personality::new).collect(Collectors.toList()));
        getItems().forEach(idea -> this.personalitiesMap.put(idea, getNewList()));

        this.disableAddProperty = new SimpleBooleanProperty(getItems().size() >= country.getSave().getGame().getMaxExtraPersonalities() + 1
                                                            && CollectionUtils.isNotEmpty(getNewList()));

        getItems().addListener((ListChangeListener<? super Personality>) c ->
                this.disableAddProperty.setValue(c.getList().size() >= country.getSave().getGame().getMaxExtraPersonalities() + 1
                                                 && CollectionUtils.isNotEmpty(getNewList())));
    }

    private ObservableList<Personality> getNewList() {
        return this.personalities.stream()
                                 .filter(personality -> this.personalitiesMap.keySet().stream().noneMatch(p2 -> p2.equals(personality)))
                                 .filter(personality -> personality.getRulerPersonality().isMonarchValid(this.monarch))
                                 .filter(personality -> personality.getRulerPersonality().isMonarchValid(this.monarch))
                                 .collect(Collectors.toCollection(FXCollections::observableArrayList));
    }

    public boolean isDisableAddProperty() {
        return disableAddProperty.get();
    }

    public BooleanProperty disableAddPropertyProperty() {
        return disableAddProperty;
    }
}
