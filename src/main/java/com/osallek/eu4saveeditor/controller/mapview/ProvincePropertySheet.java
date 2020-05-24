package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.clausewitzparser.common.ClausewitzUtils;
import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4parser.model.save.province.Province;
import com.osallek.eu4saveeditor.controller.converter.CountryStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.CountryStringConverter;
import com.osallek.eu4saveeditor.controller.propertyeditor.CustomPropertyEditorFactory;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.CheckBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.CheckComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableTextItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ComboBoxItem;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.control.Button;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import org.controlsfx.control.CheckComboBox;
import org.controlsfx.control.PropertySheet;
import org.controlsfx.control.SearchableComboBox;

public class ProvincePropertySheet extends VBox {

    private final Province province;

    private final ClearableTextItem nameField;

    private ComboBoxItem<Country> ownerComboBox;

    private ComboBoxItem<Country> controllerComboBox;

    private CheckBoxItem changeControllerAndAddCoreField;

    private CheckComboBoxItem<Country> coresField;

    private final Button submitButton;

    public ProvincePropertySheet(Pane pane, Province province) {
        this.province = province;
        ObservableList<PropertySheet.Item> items = FXCollections.observableArrayList();

        this.nameField = new ClearableTextItem("Category", "Name: ", ClausewitzUtils.removeQuotes(this.province.getName()),
                                               () -> ClausewitzUtils.removeQuotes(this.province.getName()), true);
        items.add(this.nameField);

        if (this.province.isColonizable()) {
            this.changeControllerAndAddCoreField = new CheckBoxItem("Category2", "Change owner and add core", true);

            this.controllerComboBox = new ComboBoxItem<>("Category2", "Controller: ",
                                                         FXCollections.observableArrayList(province.getSave()
                                                                                                   .getPlayableCountries()),
                                                         this.province.getCountry(),
                                                         new SearchableComboBox<>());
            this.controllerComboBox.setConverter(new CountryStringConverter());
            this.controllerComboBox.setCellFactory(new CountryStringCellFactory());

            this.ownerComboBox = new ComboBoxItem<>("Category2", "Owner: ",
                                                    FXCollections.observableArrayList(province.getSave()
                                                                                              .getPlayableCountries()),
                                                    this.province.getCountry(),
                                                    new SearchableComboBox<>());
            this.ownerComboBox.setConverter(new CountryStringConverter());
            this.ownerComboBox.setCellFactory(new CountryStringCellFactory());
            this.ownerComboBox.valueProperty().addListener((observable, oldValue, newValue) -> {
                if (this.changeControllerAndAddCoreField != null
                    && this.changeControllerAndAddCoreField.isSelected()) {
                    this.controllerComboBox.select(newValue);
                    this.coresField.check(newValue);
                    this.coresField.clearCheck(oldValue);
                }
            });

            this.coresField = new CheckComboBoxItem<>("Category2", "Cores: ",
                                                      FXCollections.observableArrayList(province.getSave()
                                                                                                .getPlayableCountries()),
                                                      FXCollections.observableArrayList(province.getCores()),
                                                      new CheckComboBox<>());
            this.coresField.setConverter(new CountryStringConverter());

            items.add(this.ownerComboBox);
            items.add(this.changeControllerAndAddCoreField);
            items.add(this.controllerComboBox);
            items.add(this.coresField);
        }

        this.submitButton = new Button("Submit");
        this.submitButton.setOnAction(this::validate);

        PropertySheet propertySheet = new PropertySheet(items);
        propertySheet.setPropertyEditorFactory(new CustomPropertyEditorFactory());
        propertySheet.setMode(PropertySheet.Mode.CATEGORY);
        VBox.setVgrow(propertySheet, Priority.ALWAYS);

        pane.getChildren().clear();
        pane.getChildren().add(propertySheet);
        pane.getChildren().add(this.submitButton);
    }

    private void validate(ActionEvent actionEvent) {
        boolean nameChanged;
        boolean countryChanged;
        boolean controllerChanged;
        boolean coresChanged;

        if (!ClausewitzUtils.removeQuotes(this.province.getName()).equals(this.nameField.getText())) {
            this.province.setName(this.nameField.getText());
            nameChanged = true;
        }

        if (!this.province.getCountry().equals(this.ownerComboBox.getValue())) {
            this.province.setOwner(this.ownerComboBox.getSelectedValue().getTag());
            countryChanged = true;
        }

        if (!this.province.getController().equals(this.controllerComboBox.getSelectedValue().getTag())) {
            this.province.setController(this.controllerComboBox.getSelectedValue().getTag());
            controllerChanged = true;
        }

        if (!this.province.getCores().equals(this.coresField.getSelectedValues())) {
            this.coresField.getSelectedValues().forEach(country -> this.province.addCore(country.getTag()));
            coresChanged = true;
        }
    }
}
