package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.clausewitzparser.common.ClausewitzUtils;
import com.osallek.eu4parser.model.game.Building;
import com.osallek.eu4parser.model.game.Culture;
import com.osallek.eu4parser.model.game.Religion;
import com.osallek.eu4parser.model.game.TradeGood;
import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4parser.model.save.province.SaveProvince;
import com.osallek.eu4saveeditor.controller.control.ClearableCheckComboBox;
import com.osallek.eu4saveeditor.controller.control.ClearableComboBox;
import com.osallek.eu4saveeditor.controller.control.ClearableSpinnerDouble;
import com.osallek.eu4saveeditor.controller.control.CustomPropertySheetSkin;
import com.osallek.eu4saveeditor.controller.control.SelectableGridView;
import com.osallek.eu4saveeditor.controller.converter.CountryStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.CountryStringConverter;
import com.osallek.eu4saveeditor.controller.converter.CultureStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.CultureStringConverter;
import com.osallek.eu4saveeditor.controller.converter.ReligionStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.ReligionStringConverter;
import com.osallek.eu4saveeditor.controller.converter.TradeGoodStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.TradeGoodStringConverter;
import com.osallek.eu4saveeditor.controller.propertyeditor.CustomPropertyEditorFactory;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.CheckBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableCheckComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSliderItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSpinnerItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableTextItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.HBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.SelectableGridViewItem;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.ObservableSet;
import javafx.event.ActionEvent;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import org.controlsfx.control.PropertySheet;
import org.controlsfx.control.SearchableComboBox;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class ProvincePropertySheet extends VBox {

    private final SaveProvince province;

    private final Label titleLabel;

    private final ClearableTextItem nameField;

    private ClearableComboBoxItem<Culture> cultureComboBox;

    private ClearableComboBoxItem<Religion> religionComboBox;

    private ClearableTextItem capitalField;

    private ClearableComboBoxItem<Country> ownerComboBox;

    private ClearableComboBoxItem<Country> controllerComboBox;

    private ClearableCheckComboBoxItem<Country> coresField;

    private ClearableCheckComboBoxItem<Country> claimsField;

    private CheckBoxItem hreField;

    private ClearableSpinnerItem<Double> baseTaxField;

    private ClearableSpinnerItem<Double> baseProdField;

    private ClearableSpinnerItem<Double> baseMPField;

    private ClearableComboBoxItem<TradeGood> tradeGoodField;

    private ClearableComboBoxItem<TradeGood> latentTradeGoodField;

    private List<ClearableSliderItem> institutionFields;

    private ClearableSliderItem autonomyField;

    private ClearableSliderItem devastationField;

    private List<SelectableGridViewItem<Building>> buildingsFields;

    private final Button submitButton;

    public ProvincePropertySheet(Pane pane, SaveProvince province, ObservableList<Country> playableCountries,
                                 ObservableList<Culture> cultures, ObservableList<Religion> religions,
                                 ObservableList<TradeGood> tradeGoods) {
        this.province = province;
        this.titleLabel = new Label(ClausewitzUtils.removeQuotes(this.province.getName()));
        ObservableList<PropertySheet.Item> items = FXCollections.observableArrayList();

        //GENERAL
        this.nameField = new ClearableTextItem(SheetCategory.PROVINCE_GENERAL,
                                               this.province.getSave().getGame().getLocalisation("LEDGER_NAME"),
                                               ClausewitzUtils.removeQuotes(this.province.getName()),
                                               () -> ClausewitzUtils.removeQuotes(this.province.getName()));

        items.add(this.nameField);

        if (this.province.isColonizable()) {
            //GENERAL
            this.capitalField = new ClearableTextItem(SheetCategory.PROVINCE_GENERAL,
                                                      this.province.getSave()
                                                                   .getGame()
                                                                   .getLocalisation("TRIGGER_CAPITAL"),
                                                      ClausewitzUtils.removeQuotes(this.province.getCapital()),
                                                      () -> ClausewitzUtils.removeQuotes(this.province.getCapital()));

            this.cultureComboBox = new ClearableComboBoxItem<>(SheetCategory.PROVINCE_GENERAL,
                                                               this.province.getSave()
                                                                            .getGame()
                                                                            .getLocalisation("LEDGER_CULTURE"),
                                                               cultures,
                                                               this.province.getCulture(),
                                                               new ClearableComboBox<>(new SearchableComboBox<>(), this.province::getCulture));
            this.cultureComboBox.setConverter(new CultureStringConverter());
            this.cultureComboBox.setCellFactory(new CultureStringCellFactory());

            this.religionComboBox = new ClearableComboBoxItem<>(SheetCategory.PROVINCE_GENERAL,
                                                                this.province.getSave()
                                                                             .getGame()
                                                                             .getLocalisation("LEDGER_RELIGION"),
                                                                religions,
                                                                this.province.getReligion(),
                                                                new ClearableComboBox<>(new SearchableComboBox<>(), this.province::getReligion));
            this.religionComboBox.setConverter(new ReligionStringConverter());
            this.religionComboBox.setCellFactory(new ReligionStringCellFactory());

            items.add(this.capitalField);
            items.add(this.cultureComboBox);
            items.add(this.religionComboBox);


            //POLITICAL
            this.controllerComboBox = new ClearableComboBoxItem<>(SheetCategory.PROVINCE_POLITICAL,
                                                                  this.province.getSave()
                                                                               .getGame()
                                                                               .getLocalisationClean("SUPPLY_CONTROLLER"),
                                                                  playableCountries,
                                                                  this.province.getCountry(),
                                                                  new ClearableComboBox<>(new SearchableComboBox<>(), this.province::getController));
            this.controllerComboBox.setConverter(new CountryStringConverter());
            this.controllerComboBox.setCellFactory(new CountryStringCellFactory());

            this.ownerComboBox = new ClearableComboBoxItem<>(SheetCategory.PROVINCE_POLITICAL,
                                                             this.province.getSave()
                                                                          .getGame()
                                                                          .getLocalisation("LEDGER_OWNER"),
                                                             playableCountries,
                                                             this.province.getCountry(),
                                                             new ClearableComboBox<>(new SearchableComboBox<>(), this.province::getCountry));
            this.ownerComboBox.setConverter(new CountryStringConverter());
            this.ownerComboBox.setCellFactory(new CountryStringCellFactory());
            this.ownerComboBox.valueProperty().addListener((observable, oldValue, newValue) -> {
                this.controllerComboBox.select(newValue);
                this.coresField.check(newValue);
                this.coresField.clearCheck(oldValue);
            });

            this.coresField = new ClearableCheckComboBoxItem<>(SheetCategory.PROVINCE_POLITICAL,
                                                               this.province.getSave()
                                                                            .getGame()
                                                                            .getLocalisation("LEDGER_CORE"),
                                                               playableCountries,
                                                               FXCollections.observableArrayList(province.getCores()),
                                                               new ClearableCheckComboBox<>(this.province::getCores));
            this.coresField.setConverter(new CountryStringConverter());

            this.claimsField = new ClearableCheckComboBoxItem<>(SheetCategory.PROVINCE_POLITICAL,
                                                                this.province.getSave()
                                                                             .getGame()
                                                                             .getLocalisation("CLAIMS"),
                                                                playableCountries,
                                                                FXCollections.observableArrayList(province.getClaims()),
                                                                new ClearableCheckComboBox<>(this.province::getClaims));
            this.claimsField.setConverter(new CountryStringConverter());

            this.hreField = new CheckBoxItem(SheetCategory.PROVINCE_POLITICAL,
                                             this.province.getSave().getGame().getLocalisation("IS_PART_OF_HRE"),
                                             this.province.inHre());

            items.add(this.ownerComboBox);
            items.add(this.controllerComboBox);
            items.add(this.coresField);
            items.add(this.claimsField);
            items.add(this.hreField);

            //ECONOMY
            this.baseTaxField = new ClearableSpinnerItem<>(SheetCategory.PROVINCE_ECONOMY,
                                                           this.province.getSave()
                                                                        .getGame()
                                                                        .getLocalisation("LEDGER_TAX"),
                                                           new ClearableSpinnerDouble(1, 999,
                                                                                      this.province.getBaseTax(),
                                                                                      1,
                                                                                      this.province::getBaseTax));
            this.baseProdField = new ClearableSpinnerItem<>(SheetCategory.PROVINCE_ECONOMY,
                                                            this.province.getSave()
                                                                         .getGame()
                                                                         .getLocalisation("LEDGER_PRODUCTION"),
                                                            new ClearableSpinnerDouble(1, 999,
                                                                                       this.province.getBaseProduction(),
                                                                                       1,
                                                                                       this.province::getBaseProduction));
            this.baseMPField = new ClearableSpinnerItem<>(SheetCategory.PROVINCE_ECONOMY,
                                                          this.province.getSave()
                                                                       .getGame()
                                                                       .getLocalisation("LEDGER_MANPOWER"),
                                                          new ClearableSpinnerDouble(1, 999,
                                                                                     this.province.getBaseManpower(),
                                                                                     1,
                                                                                     this.province::getBaseManpower));

            this.tradeGoodField = new ClearableComboBoxItem<>(SheetCategory.PROVINCE_ECONOMY,
                                                              this.province.getSave()
                                                                           .getGame()
                                                                           .getLocalisation("LEDGER_GOODS"),
                                                              tradeGoods,
                                                              this.province.getTradeGood(),
                                                              new ClearableComboBox<>(new SearchableComboBox<>(), this.province::getTradeGood));
            this.tradeGoodField.setConverter(new TradeGoodStringConverter());
            this.tradeGoodField.setCellFactory(new TradeGoodStringCellFactory());

            this.latentTradeGoodField = new ClearableComboBoxItem<>(SheetCategory.PROVINCE_ECONOMY,
                                                                    this.province.getSave()
                                                                                 .getGame()
                                                                                 .getLocalisationClean("LATENT_TRADE_GOODS_TOOLTIP_HEADER"),
                                                                    tradeGoods,
                                                                    this.province.getLatentTradeGood(),
                                                                    new ClearableComboBox<>(new SearchableComboBox<>(), this.province::getLatentTradeGood));
            this.latentTradeGoodField.setConverter(new TradeGoodStringConverter());
            this.latentTradeGoodField.setCellFactory(new TradeGoodStringCellFactory());

            this.autonomyField = new ClearableSliderItem(SheetCategory.PROVINCE_ECONOMY,
                                                         this.province.getSave()
                                                                      .getGame()
                                                                      .getLocalisation("local_autonomy"),
                                                         0, 100,
                                                         this.province.getLocalAutonomy(),
                                                         this.province::getLocalAutonomy);

            this.devastationField = new ClearableSliderItem(SheetCategory.PROVINCE_ECONOMY,
                                                            this.province.getSave()
                                                                         .getGame()
                                                                         .getLocalisation("LEDGER_DEVASTATION"),
                                                            0, 100,
                                                            this.province.getDevastation(),
                                                            this.province::getDevastation);

            items.add(this.baseTaxField);
            items.add(this.baseProdField);
            items.add(this.baseMPField);
            items.add(this.tradeGoodField);
            items.add(this.latentTradeGoodField);
            items.add(this.autonomyField);
            items.add(this.devastationField);

            //INSTITUTIONS
            if (!this.province.getInstitutionsProgress().isEmpty()) {
                this.institutionFields = new ArrayList<>();
                for (int i = 0; i < this.province.getInstitutionsProgress().size(); i++) {
                    int finalI = i;
                    this.institutionFields.add(new ClearableSliderItem(SheetCategory.PROVINCE_INSTITUTIONS,
                                                                       this.province.getSave()
                                                                                    .getGame()
                                                                                    .getInstitution(i)
                                                                                    .getLocalizedName(),
                                                                       0, 100,
                                                                       this.province.getInstitutionsProgress(i),
                                                                       () -> this.province.getInstitutionsProgress(finalI)));
                }

                items.addAll(this.institutionFields);
            }

            //BUILDINGS
            this.buildingsFields = new ArrayList<>();
            this.province.getAvailableBuildingsTree().forEach(buildings -> {
                ObservableSet<Building> buildingsBuilt = FXCollections.observableSet(this.province.getBuildings()
                                                                                                  .stream()
                                                                                                  .filter(buildings::contains)
                                                                                                  .collect(Collectors.toSet()));
                SelectableGridViewItem<Building> gridViewItem = new SelectableGridViewItem<>(SheetCategory.PROVINCE_BUILDINGS,
                                                                                             new SelectableGridView<>(FXCollections
                                                                                                                              .observableList(buildings),
                                                                                                                      buildingsBuilt));

                gridViewItem.setCellFactory(Building::getLocalizedName);
                this.buildingsFields.add(gridViewItem);
            });

            for (int i = 0; i < this.buildingsFields.size(); i++) {
                SelectableGridViewItem<Building> current = this.buildingsFields.get(i);
                if (i < this.buildingsFields.size() - 1) {
                    SelectableGridViewItem<Building> next = this.buildingsFields.get(i + 1);
                    if (current.getNbItems() <= 4 && next.getNbItems() <= 4) {
                        HBox hBox = new HBox(13);
                        HBox.setHgrow(current.getSelectableGridView(), Priority.ALWAYS);
                        HBox.setHgrow(next.getSelectableGridView(), Priority.ALWAYS);
                        hBox.getChildren().addAll(current.getSelectableGridView(), next.getSelectableGridView());
                        items.addAll(new HBoxItem<Building>(SheetCategory.PROVINCE_BUILDINGS, hBox));
                        i++;
                    } else {
                        items.add(current);
                    }
                } else {
                    items.add(current);
                }
            }
        }


        //Submit
        this.submitButton = new Button("Submit");
        this.submitButton.setOnAction(this::validate);

        PropertySheet propertySheet = new PropertySheet(items);
        propertySheet.setPropertyEditorFactory(new CustomPropertyEditorFactory());
        propertySheet.setMode(PropertySheet.Mode.CATEGORY);
        propertySheet.setCategoryComparator(Comparator.comparing(SheetCategory::getByLocale));
        propertySheet.setModeSwitcherVisible(false);
        propertySheet.setSkin(new CustomPropertySheetSkin(propertySheet));
        VBox.setVgrow(propertySheet, Priority.ALWAYS);

        pane.getChildren().clear();
        pane.getChildren().add(titleLabel);
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
        boolean hreChanged;
        boolean cultureChanged;
        boolean religionChanged;
        boolean baseTaxChanged;
        boolean baseProdChanged;
        boolean baseMPChanged;
        boolean anyInstitutionChanged;
        boolean tradeGoodChanged;
        boolean latentTradeGoodChanged;
        boolean autonomyChanged;
        boolean devastationChanged;

        if (!ClausewitzUtils.removeQuotes(this.province.getName()).equals(this.nameField.getText())) {
            this.province.setName(this.nameField.getText());
            this.titleLabel.setText(ClausewitzUtils.removeQuotes(this.province.getName()));
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

        if (this.hreField != null) {
            if (this.province.inHre() != this.hreField.isSelected()) {
                this.province.setInHre(this.hreField.isSelected());
                hreChanged = true;
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
            if (!this.province.getBaseTax().equals(this.baseTaxField.getTrueValue())) {
                this.province.setBaseTax(this.baseTaxField.getTrueValue());
                baseTaxChanged = true;
            }
        }

        if (this.baseProdField != null) {
            if (!this.province.getBaseProduction().equals(this.baseProdField.getTrueValue())) {
                this.province.setBaseProduction(this.baseProdField.getTrueValue());
                baseProdChanged = true;
            }
        }

        if (this.baseMPField != null) {
            if (!this.province.getBaseManpower().equals(this.baseMPField.getTrueValue())) {
                this.province.setBaseManpower(this.baseMPField.getTrueValue());
                baseMPChanged = true;
            }
        }

        if (this.institutionFields != null) {
            for (int i = 0; i < this.institutionFields.size(); i++) {
                if (this.province.getInstitutionsProgress(i) != this.institutionFields.get(i).getDoubleValue()) {
                    this.province.setInstitutionProgress(i, this.institutionFields.get(i).getDoubleValue());
                    anyInstitutionChanged = true;
                }
            }
        }

        if (this.tradeGoodField != null) {
            if (!this.province.getTradeGood().equals(this.tradeGoodField.getSelectedValue())) {
                this.province.setTradeGoods(this.tradeGoodField.getSelectedValue().getName());
                tradeGoodChanged = true;
            }
        }

        if (this.latentTradeGoodField != null) {
            if (!Objects.deepEquals(this.province.getLatentTradeGood(), this.latentTradeGoodField.getSelectedValue())) {
                this.province.setLatentTradeGoods(this.latentTradeGoodField.getSelectedValue().getName());
                latentTradeGoodChanged = true;
            }
        }

        if (this.autonomyField != null) {
            if (!Objects.deepEquals(this.province.getLocalAutonomy(), this.autonomyField.getDoubleValue())) {
                this.province.setLocalAutonomy(this.autonomyField.getDoubleValue());
                autonomyChanged = true;
            }
        }

        if (this.devastationField != null) {
            if (!Objects.deepEquals(this.province.getDevastation(), this.devastationField.getDoubleValue())) {
                this.province.setDevastation(this.devastationField.getDoubleValue());
                devastationChanged = true;
            }
        }
    }
}
