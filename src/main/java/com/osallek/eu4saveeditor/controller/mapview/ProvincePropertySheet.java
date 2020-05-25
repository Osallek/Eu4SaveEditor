package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.clausewitzparser.common.ClausewitzUtils;
import com.osallek.eu4parser.model.game.culture.Culture;
import com.osallek.eu4parser.model.game.religion.Religion;
import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4parser.model.save.province.Province;
import com.osallek.eu4saveeditor.controller.converter.CountryStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.CountryStringConverter;
import com.osallek.eu4saveeditor.controller.converter.CultureStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.CultureStringConverter;
import com.osallek.eu4saveeditor.controller.converter.ReligionStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.ReligionStringConverter;
import com.osallek.eu4saveeditor.controller.item.ClearableCheckComboBox;
import com.osallek.eu4saveeditor.controller.item.ClearableComboBox;
import com.osallek.eu4saveeditor.controller.propertyeditor.CustomPropertyEditorFactory;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.CheckBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableCheckComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSliderItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSpinnerItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableTextItem;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.control.Button;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import org.controlsfx.control.PropertySheet;
import org.controlsfx.control.SearchableComboBox;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class ProvincePropertySheet extends VBox {

    private final Province province;

    private final ClearableTextItem nameField;

    private ClearableComboBoxItem<Culture> cultureComboBox;

    private ClearableComboBoxItem<Religion> religionComboBox;

    private ClearableTextItem capitalField;

    private ClearableComboBoxItem<Country> ownerComboBox;

    private ClearableComboBoxItem<Country> controllerComboBox;

    private CheckBoxItem changeControllerAndAddCoreField;

    private ClearableCheckComboBoxItem<Country> coresField;

    private ClearableCheckComboBoxItem<Country> claimsField;

    private ClearableSpinnerItem baseTaxField;

    private ClearableSpinnerItem baseProdField;

    private ClearableSpinnerItem baseMPField;

    private List<ClearableSliderItem> institutionFields;

    private final Button submitButton;

    public ProvincePropertySheet(Pane pane, Province province, ObservableList<Country> playableCountries, ObservableList<Culture> cultures, ObservableList<Religion> religions) {
        this.province = province;
        ObservableList<PropertySheet.Item> items = FXCollections.observableArrayList();

        //GENERAL
        this.nameField = new ClearableTextItem(SheetCategory.PROVINCE_GENERAL,
                                               "Name: ",
                                               ClausewitzUtils.removeQuotes(this.province.getName()),
                                               () -> ClausewitzUtils.removeQuotes(this.province.getName()));

        items.add(this.nameField);

        if (this.province.isColonizable()) {
            //GENERAL
            this.capitalField = new ClearableTextItem(SheetCategory.PROVINCE_GENERAL,
                                                      "Capital: ",
                                                      ClausewitzUtils.removeQuotes(this.province.getCapital()),
                                                      () -> ClausewitzUtils.removeQuotes(this.province.getCapital()));

            items.add(this.capitalField);

            this.cultureComboBox = new ClearableComboBoxItem<>(SheetCategory.PROVINCE_GENERAL, "Culture: ",
                                                               cultures,
                                                               this.province.getCulture(),
                                                               new ClearableComboBox<>(new SearchableComboBox<>(), this.province::getCulture));
            this.cultureComboBox.setConverter(new CultureStringConverter());
            this.cultureComboBox.setCellFactory(new CultureStringCellFactory());
            items.add(this.cultureComboBox);

            this.religionComboBox = new ClearableComboBoxItem<>(SheetCategory.PROVINCE_GENERAL, "Religion: ",
                                                                religions,
                                                                this.province.getReligion(),
                                                                new ClearableComboBox<>(new SearchableComboBox<>(), this.province::getReligion));
            this.religionComboBox.setConverter(new ReligionStringConverter());
            this.religionComboBox.setCellFactory(new ReligionStringCellFactory());
            items.add(this.religionComboBox);


            //POLITICAL
            this.changeControllerAndAddCoreField = new CheckBoxItem(SheetCategory.PROVINCE_POLITICAL, "Change owner and add core", true);

            this.controllerComboBox = new ClearableComboBoxItem<>(SheetCategory.PROVINCE_POLITICAL, "Controller: ",
                                                                  playableCountries,
                                                                  this.province.getCountry(),
                                                                  new ClearableComboBox<>(new SearchableComboBox<>(), this.province::getController));
            this.controllerComboBox.setConverter(new CountryStringConverter());
            this.controllerComboBox.setCellFactory(new CountryStringCellFactory());

            this.ownerComboBox = new ClearableComboBoxItem<>(SheetCategory.PROVINCE_POLITICAL,
                                                             "Owner: ",
                                                             playableCountries,
                                                             this.province.getCountry(),
                                                             new ClearableComboBox<>(new SearchableComboBox<>(), this.province::getCountry));
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

            this.coresField = new ClearableCheckComboBoxItem<>(SheetCategory.PROVINCE_POLITICAL,
                                                               "Cores: ",
                                                               playableCountries,
                                                               FXCollections.observableArrayList(province.getCores()),
                                                               new ClearableCheckComboBox<>(this.province::getCores));
            this.coresField.setConverter(new CountryStringConverter());

            this.claimsField = new ClearableCheckComboBoxItem<>(SheetCategory.PROVINCE_POLITICAL, "Claims: ",
                                                                playableCountries,
                                                                FXCollections.observableArrayList(province.getClaims()),
                                                                new ClearableCheckComboBox<>(this.province::getClaims));
            this.claimsField.setConverter(new CountryStringConverter());

            items.add(this.ownerComboBox);
            items.add(this.changeControllerAndAddCoreField);
            items.add(this.controllerComboBox);
            items.add(this.coresField);
            items.add(this.claimsField);

            //ECONOMY
            this.baseTaxField = new ClearableSpinnerItem(SheetCategory.PROVINCE_ECONOMY, "Tax: ", 1, 999,
                                                         this.province.getBaseTax().intValue(),
                                                         () -> this.province.getBaseTax().intValue());
            this.baseProdField = new ClearableSpinnerItem(SheetCategory.PROVINCE_ECONOMY, "Production: ", 1, 999,
                                                          this.province.getBaseProduction().intValue(),
                                                          () -> this.province.getBaseProduction().intValue());
            this.baseMPField = new ClearableSpinnerItem(SheetCategory.PROVINCE_ECONOMY, "Manpower: ", 1, 999,
                                                        this.province.getBaseManpower().intValue(),
                                                        () -> this.province.getBaseManpower().intValue());

            items.add(this.baseTaxField);
            items.add(this.baseProdField);
            items.add(this.baseMPField);

            //INSTITUTIONS
            if (!this.province.getInstitutionsProgress().isEmpty()) {
                this.institutionFields = new ArrayList<>();
                for (int i = 0; i < this.province.getInstitutionsProgress().size(); i++) {
                    int finalI = i;
                    this.institutionFields.add(new ClearableSliderItem(SheetCategory.PROVINCE_INSTITUTIONS,
                                                                       this.province.getSave()
                                                                                    .getGame()
                                                                                    .getInstitution(i)
                                                                                    .getName() + ": ",
                                                                       0, 100,
                                                                       this.province.getInstitutionsProgress(i),
                                                                       () -> this.province.getInstitutionsProgress(finalI)));
                }

                items.addAll(this.institutionFields);
            }
        }


        //Submit
        this.submitButton = new Button("Submit");
        this.submitButton.setOnAction(this::validate);

        PropertySheet propertySheet = new PropertySheet(items);
        propertySheet.setPropertyEditorFactory(new CustomPropertyEditorFactory());
        propertySheet.setMode(PropertySheet.Mode.CATEGORY);
        propertySheet.setModeSwitcherVisible(false);
        VBox.setVgrow(propertySheet, Priority.ALWAYS);

        pane.getChildren().clear();
        pane.getChildren().add(propertySheet);
        pane.getChildren().add(this.submitButton);
    }

    private void validate(ActionEvent actionEvent) {
        boolean nameChanged;
        boolean capitalChanged;
        boolean countryChanged;
        boolean controllerChanged;
        boolean coresChanged;
        boolean claimsChanged;
        boolean cultureChanged;
        boolean religionChanged;
        boolean baseTaxChanged;
        boolean baseProdChanged;
        boolean baseMPChanged;
        boolean anyInstutitionChanged;

        if (!ClausewitzUtils.removeQuotes(this.province.getName()).equals(this.nameField.getText())) {
            this.province.setName(this.nameField.getText());
            nameChanged = true;
        }

        if (this.capitalField != null) {
            if (!ClausewitzUtils.removeQuotes(this.province.getCapital()).equals(this.capitalField.getText())) {
                this.province.setCapital(this.capitalField.getText());
                capitalChanged = true;
            }
        }

        if (this.ownerComboBox != null) {
            if (!this.province.getCountry().equals(this.ownerComboBox.getValue())) {
                this.province.setOwner(this.ownerComboBox.getSelectedValue().getTag());
                countryChanged = true;
            }
        }

        if (this.controllerComboBox != null) {
            if (!this.province.getController().equals(this.controllerComboBox.getSelectedValue())) {
                this.province.setController(this.controllerComboBox.getSelectedValue().getTag());
                controllerChanged = true;
            }
        }

        if (this.coresField != null) {
            if (!this.province.getCores().equals(this.coresField.getSelectedValues())) {
                this.province.setCores(this.coresField.getSelectedValues()
                                                      .stream()
                                                      .map(Country::getTag)
                                                      .collect(Collectors.toList()));
                coresChanged = true;
            }
        }

        if (this.claimsField != null) {
            if (!this.province.getClaims().equals(this.claimsField.getSelectedValues())) {
                this.province.setClaims(this.claimsField.getSelectedValues()
                                                        .stream()
                                                        .map(Country::getTag)
                                                        .collect(Collectors.toList()));
                claimsChanged = true;
            }
        }

        if (this.cultureComboBox != null) {
            if (!this.province.getCulture().equals(this.cultureComboBox.getSelectedValue())) {
                this.province.setCulture(this.cultureComboBox.getSelectedValue().getName());
                cultureChanged = true;
            }
        }

        if (this.religionComboBox != null) {
            if (!this.province.getReligion().equals(this.religionComboBox.getSelectedValue())) {
                this.province.setReligion(this.religionComboBox.getSelectedValue().getName());
                religionChanged = true;
            }
        }

        if (this.baseTaxField != null) {
            if (this.province.getBaseTax().intValue() != this.baseTaxField.getIntValue()) {
                this.province.setBaseTax(this.baseTaxField.getIntValue());
                baseTaxChanged = true;
            }
        }

        if (this.baseProdField != null) {
            if (this.province.getBaseProduction().intValue() != this.baseProdField.getIntValue()) {
                this.province.setBaseProduction(this.baseProdField.getIntValue());
                baseProdChanged = true;
            }
        }

        if (this.baseMPField != null) {
            if (this.province.getBaseManpower().intValue() != this.baseMPField.getIntValue()) {
                this.province.setBaseManpower(this.baseMPField.getIntValue());
                baseMPChanged = true;
            }
        }

        if (this.institutionFields != null) {
            for (int i = 0; i < this.institutionFields.size(); i++) {
                if (this.province.getInstitutionsProgress(i) != this.institutionFields.get(i).getDoubleValue()) {
                    this.province.setInstitutionProgress(i, this.institutionFields.get(i).getDoubleValue());
                    anyInstutitionChanged = true;
                }
            }
        }
    }
}
