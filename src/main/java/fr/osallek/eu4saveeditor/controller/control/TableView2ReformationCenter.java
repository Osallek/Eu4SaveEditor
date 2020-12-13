package fr.osallek.eu4saveeditor.controller.control;

import fr.osallek.eu4parser.common.Eu4Utils;
import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4parser.model.save.province.SaveProvince;
import fr.osallek.eu4saveeditor.controller.converter.ProvinceStringConverter;
import fr.osallek.eu4saveeditor.controller.object.ReformationCenter;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import org.controlsfx.control.SearchableComboBox;

import java.util.Comparator;
import java.util.stream.Collectors;

public class TableView2ReformationCenter extends TableView<ReformationCenter> {

    private final Save save;

    private final BooleanProperty disableAddProperty;

    private final ObservableList<SaveProvince> provinces;

    private final ObservableMap<ReformationCenter, ObservableList<SaveProvince>> reformationCenters = FXCollections.observableHashMap();

    public TableView2ReformationCenter(Save save, ObservableList<ReformationCenter> reformationCenters, ObservableList<SaveProvince> provinces) {
        this.save = save;
        this.provinces = provinces;

        TableColumn<ReformationCenter, SaveProvince> province = new TableColumn<>(save.getGame().getLocalisationCleanNoPunctuation("LEDGER_LOCATION"));
        province.setCellValueFactory(p -> p.getValue() == null ? null : new ReadOnlyObjectWrapper<>(p.getValue().getProvince()));
        province.setCellFactory(UniqueComboBoxTableCell.forTableColumn(new ProvinceStringConverter(),
                                                                       this.reformationCenters,
                                                                       Comparator.comparing(SaveProvince::getName, Eu4Utils.COLLATOR),
                                                                       SearchableComboBox::new));
        province.setOnEditCommit(event -> {
            if (event.getNewValue() != null) {
                event.getRowValue().setProvince(event.getNewValue());
            }
        });
        province.setPrefWidth(250);
        province.setStyle("-fx-alignment: CENTER-LEFT");

        TableColumn<ReformationCenter, Void> remove = new TableColumn<>();
        remove.setPrefWidth(48);
        remove.setEditable(false);
        remove.setCellFactory(ClearCellFactory.forTableColumn());

        setFixedCellSize(40);
        setPrefWidth(300);
        setEditable(true);

        getColumns().clear();
        getColumns().add(province);
        getColumns().add(remove);
        setItems(reformationCenters.stream().map(ReformationCenter::new).collect(Collectors.toCollection(FXCollections::observableArrayList)));
        getItems().forEach(center -> this.reformationCenters.put(center, getNewList()));

        this.disableAddProperty = new SimpleBooleanProperty(getItems().size() >= this.save.getGame().getMaxChristianReligiousCenters() ||
                                                            getItems().size() >= this.provinces.size());

        getItems().addListener((ListChangeListener<? super ReformationCenter>) c -> {
            this.disableAddProperty.setValue(getItems().size() >= this.save.getGame().getMaxChristianReligiousCenters() ||
                                             getItems().size() >= this.provinces.size());

            while (c.next()) {
                c.getRemoved().forEach(s -> {
                    this.reformationCenters.remove(s);
                    this.reformationCenters.values().forEach(list -> list.add(s.getProvince()));
                });
                c.getAddedSubList().forEach(s -> {
                    this.reformationCenters.values().forEach(list -> list.remove(s.getProvince()));
                    this.reformationCenters.put(s, this.getNewList());
                });
            }
        });

    }

    private ObservableList<SaveProvince> getNewList() {
        return this.provinces.stream()
                             .filter(province -> getItems().stream().noneMatch(center -> center.getProvince().equals(province)))
                             .collect(Collectors.toCollection(FXCollections::observableArrayList));
    }

    public boolean isDisableAddProperty() {
        return disableAddProperty.get();
    }

    public BooleanProperty disableAddPropertyProperty() {
        return disableAddProperty;
    }
}
