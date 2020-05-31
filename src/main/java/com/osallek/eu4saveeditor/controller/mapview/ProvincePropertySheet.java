package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.clausewitzparser.common.ClausewitzUtils;
import com.osallek.eu4parser.model.game.Building;
import com.osallek.eu4parser.model.game.Culture;
import com.osallek.eu4parser.model.game.Religion;
import com.osallek.eu4parser.model.game.TradeGood;
import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4parser.model.save.province.SaveProvince;
import com.osallek.eu4saveeditor.controller.control.ClearableCheckComboBox;
import com.osallek.eu4saveeditor.controller.control.ClearableComboBox;
import com.osallek.eu4saveeditor.controller.control.ClearableSpinnerDouble;
import com.osallek.eu4saveeditor.controller.control.ClearableSpinnerInt;
import com.osallek.eu4saveeditor.controller.control.SelectableGridView;
import com.osallek.eu4saveeditor.controller.converter.CountryStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.CountryStringConverter;
import com.osallek.eu4saveeditor.controller.converter.CultureStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.CultureStringConverter;
import com.osallek.eu4saveeditor.controller.converter.ReligionStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.ReligionStringConverter;
import com.osallek.eu4saveeditor.controller.converter.TradeGoodStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.TradeGoodStringConverter;
import com.osallek.eu4saveeditor.controller.pane.CustomPropertySheetSkin;
import com.osallek.eu4saveeditor.controller.propertyeditor.CustomPropertyEditorFactory;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.CheckBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableCheckComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSliderItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSpinnerItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableTextItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.HBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.SelectableGridViewItem;
import com.osallek.eu4saveeditor.controller.validator.CustomGraphicValidationDecoration;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.BooleanPropertyBase;
import javafx.beans.value.ChangeListener;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.ObservableSet;
import javafx.event.ActionEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import org.controlsfx.control.PropertySheet;
import org.controlsfx.control.SearchableComboBox;
import org.controlsfx.validation.ValidationSupport;
import org.controlsfx.validation.Validator;
import org.controlsfx.validation.decoration.CompoundValidationDecoration;
import org.controlsfx.validation.decoration.StyleClassValidationDecoration;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class ProvincePropertySheet extends VBox {

    private SaveProvince province;

    private final PropertySheet propertySheet;

    private final ValidationSupport validationSupport;

    private final ClearableTextItem nameField;

    private final ClearableComboBoxItem<Culture> cultureComboBox;

    private final ClearableComboBoxItem<Religion> religionComboBox;

    private final ClearableTextItem capitalField;

    private final ClearableComboBoxItem<Country> ownerComboBox;

    private final ClearableComboBoxItem<Country> controllerComboBox;

    private final ClearableCheckComboBoxItem<Country> coresField;

    private final ClearableCheckComboBoxItem<Country> claimsField;

    private final CheckBoxItem hreField;

    private final ClearableComboBoxItem<Country> colonizeForField;

    private final ClearableSliderItem colonySizeField;

    private final ClearableSpinnerItem<Double> baseTaxField;

    private final ClearableSpinnerItem<Double> baseProdField;

    private final ClearableSpinnerItem<Double> baseMPField;

    private final ClearableComboBoxItem<TradeGood> tradeGoodField;

    private final ClearableComboBoxItem<TradeGood> latentTradeGoodField;

    private final ClearableSpinnerItem<Integer> cotField;

    private final List<ClearableSliderItem> institutionFields;

    private final ClearableSliderItem autonomyField;

    private final ClearableSliderItem devastationField;

    private List<SelectableGridViewItem<Building>> buildingsFields;

    private final ChangeListener<? super Country> ownerChangeListener;

    private BooleanProperty countryChanged;

    public ProvincePropertySheet(Save save, ObservableList<Country> playableCountries,
                                 ObservableList<Culture> cultures, ObservableList<Religion> religions,
                                 ObservableList<TradeGood> tradeGoods) {
        this.propertySheet = new PropertySheet();
        this.propertySheet.setPropertyEditorFactory(new CustomPropertyEditorFactory());
        this.propertySheet.setMode(PropertySheet.Mode.CATEGORY);
        this.propertySheet.setCategoryComparator(Comparator.comparing(SheetCategory::getByLocale));
        this.propertySheet.setModeSwitcherVisible(false);
        this.propertySheet.setSearchBoxVisible(false);
        this.propertySheet.setSkin(new CustomPropertySheetSkin(this.propertySheet));

        //GENERAL
        this.nameField = new ClearableTextItem(SheetCategory.PROVINCE_GENERAL,
                                               save.getGame().getLocalisation("LEDGER_NAME"));
        this.nameField.getTextField()
                      .getStylesheets()
                      .add(getClass().getClassLoader().getResource("styles/propertySheetsStyle.css").toExternalForm());

        this.validationSupport = new ValidationSupport();
        this.validationSupport.registerValidator(this.nameField.getTextField(), Validator.createEmptyValidator("Text is required"));
        this.validationSupport.setValidationDecorator(new CompoundValidationDecoration(new CustomGraphicValidationDecoration(),
                                                                                       new StyleClassValidationDecoration("validation-error", null)));

        this.capitalField = new ClearableTextItem(SheetCategory.PROVINCE_GENERAL,
                                                  save.getGame().getLocalisation("TRIGGER_CAPITAL"));

        this.cultureComboBox = new ClearableComboBoxItem<>(SheetCategory.PROVINCE_GENERAL,
                                                           save.getGame().getLocalisation("LEDGER_CULTURE"),
                                                           cultures,
                                                           new ClearableComboBox<>(new SearchableComboBox<>()));
        this.cultureComboBox.setConverter(new CultureStringConverter());
        this.cultureComboBox.setCellFactory(new CultureStringCellFactory());

        this.religionComboBox = new ClearableComboBoxItem<>(SheetCategory.PROVINCE_GENERAL,
                                                            save.getGame().getLocalisation("LEDGER_RELIGION"),
                                                            religions,
                                                            new ClearableComboBox<>(new SearchableComboBox<>()));
        this.religionComboBox.setConverter(new ReligionStringConverter());
        this.religionComboBox.setCellFactory(new ReligionStringCellFactory());

        this.controllerComboBox = new ClearableComboBoxItem<>(SheetCategory.PROVINCE_POLITICAL,
                                                              save.getGame().getLocalisationClean("SUPPLY_CONTROLLER"),
                                                              playableCountries,
                                                              new ClearableComboBox<>(new SearchableComboBox<>()));
        this.controllerComboBox.setConverter(new CountryStringConverter());
        this.controllerComboBox.setCellFactory(new CountryStringCellFactory());

        this.ownerComboBox = new ClearableComboBoxItem<>(SheetCategory.PROVINCE_POLITICAL,
                                                         save.getGame().getLocalisation("LEDGER_OWNER"),
                                                         playableCountries,
                                                         new ClearableComboBox<>(new SearchableComboBox<>()));
        this.ownerComboBox.setConverter(new CountryStringConverter());
        this.ownerComboBox.setCellFactory(new CountryStringCellFactory());

        this.coresField = new ClearableCheckComboBoxItem<>(SheetCategory.PROVINCE_POLITICAL,
                                                           save.getGame().getLocalisation("LEDGER_CORE"),
                                                           playableCountries,
                                                           new ClearableCheckComboBox<>());
        this.coresField.setConverter(new CountryStringConverter());

        this.claimsField = new ClearableCheckComboBoxItem<>(SheetCategory.PROVINCE_POLITICAL,
                                                            save.getGame().getLocalisation("HAVE_CLAIM_IN"),
                                                            playableCountries,
                                                            new ClearableCheckComboBox<>());
        this.claimsField.setConverter(new CountryStringConverter());

        this.hreField = new CheckBoxItem(SheetCategory.PROVINCE_POLITICAL,
                                         save.getGame().getLocalisation("IS_PART_OF_HRE"),
                                         false);

        this.colonizeForField = new ClearableComboBoxItem<>(SheetCategory.PROVINCE_COLONY,
                                                            save.getGame().getLocalisation("COLONIZE_PROVINCE"),
                                                            playableCountries,
                                                            new ClearableComboBox<>(new SearchableComboBox<>()));
        this.colonizeForField.setConverter(new CountryStringConverter());

        this.colonySizeField = new ClearableSliderItem(SheetCategory.PROVINCE_COLONY,
                                                       save.getGame().getLocalisation("LEDGER_POPULATION"),
                                                       0, 1000);

        this.baseTaxField = new ClearableSpinnerItem<>(SheetCategory.PROVINCE_ECONOMY,
                                                       save.getGame().getLocalisation("LEDGER_TAX"),
                                                       new ClearableSpinnerDouble(1, 999, 1));

        this.baseProdField = new ClearableSpinnerItem<>(SheetCategory.PROVINCE_ECONOMY,
                                                        save.getGame().getLocalisation("LEDGER_PRODUCTION"),
                                                        new ClearableSpinnerDouble(1, 999, 1));

        this.baseMPField = new ClearableSpinnerItem<>(SheetCategory.PROVINCE_ECONOMY,
                                                      save.getGame().getLocalisation("LEDGER_MANPOWER"),
                                                      new ClearableSpinnerDouble(1, 999, 1));

        this.tradeGoodField = new ClearableComboBoxItem<>(SheetCategory.PROVINCE_ECONOMY,
                                                          save.getGame().getLocalisation("LEDGER_GOODS"),
                                                          tradeGoods,
                                                          new ClearableComboBox<>(new SearchableComboBox<>()));
        this.tradeGoodField.setConverter(new TradeGoodStringConverter());
        this.tradeGoodField.setCellFactory(new TradeGoodStringCellFactory());

        this.latentTradeGoodField = new ClearableComboBoxItem<>(SheetCategory.PROVINCE_ECONOMY,
                                                                save.getGame()
                                                                    .getLocalisationClean("LATENT_TRADE_GOODS_TOOLTIP_HEADER"),
                                                                tradeGoods,
                                                                new ClearableComboBox<>(new SearchableComboBox<>()));
        this.latentTradeGoodField.setConverter(new TradeGoodStringConverter());
        this.latentTradeGoodField.setCellFactory(new TradeGoodStringCellFactory());

        this.cotField = new ClearableSpinnerItem<>(SheetCategory.PROVINCE_ECONOMY,
                                                   save.getGame().getLocalisationClean("EST_VAL_COT"),
                                                   new ClearableSpinnerInt(0, 3, 1));

        this.autonomyField = new ClearableSliderItem(SheetCategory.PROVINCE_ECONOMY,
                                                     save.getGame().getLocalisation("local_autonomy"),
                                                     0, 100);

        this.devastationField = new ClearableSliderItem(SheetCategory.PROVINCE_ECONOMY,
                                                        save.getGame().getLocalisation("LEDGER_DEVASTATION"),
                                                        0, 100);

        this.institutionFields = new ArrayList<>();
        for (int i = 0; i < save.getInstitutions().getNbInstitutions(); i++) {
            this.institutionFields.add(new ClearableSliderItem(SheetCategory.PROVINCE_INSTITUTIONS,
                                                               save.getGame().getInstitution(i).getLocalizedName(),
                                                               0, 100));
        }

        this.ownerChangeListener = (observable, oldValue, newValue) -> {
            this.controllerComboBox.select(newValue);

            if (this.province.isCity() && this.coresField != null) {
                this.coresField.clearCheck(oldValue);
                this.coresField.check(newValue);
            }
        };
    }

    public PropertySheet update(SaveProvince province) {
        this.province = province;
        this.countryChanged.set(false);

        this.ownerComboBox.valueProperty().removeListener(this.ownerChangeListener);

        List<PropertySheet.Item> items = new ArrayList<>();

        //GENERAL
        this.nameField.setValue(ClausewitzUtils.removeQuotes(this.province.getName()));
        this.nameField.setSupplier(() -> ClausewitzUtils.removeQuotes(this.province.getName()));
        items.add(this.nameField);

        if (province.isColonizable()) {
            //GENERAL
            this.capitalField.setValue(ClausewitzUtils.removeQuotes(this.province.getCapital()));
            this.capitalField.setSupplier(() -> ClausewitzUtils.removeQuotes(this.province.getCapital()));
            items.add(this.capitalField);

            this.cultureComboBox.setValue(this.province.getCulture());
            this.cultureComboBox.setSupplier(this.province::getCulture);
            items.add(this.cultureComboBox);

            this.religionComboBox.setValue(this.province.getReligion());
            this.religionComboBox.setSupplier(this.province::getReligion);
            items.add(this.religionComboBox);

            //POLITICAL
            if (this.province.isCity() || this.province.getColonySize() != null) {
                this.ownerComboBox.setValue(this.province.getCountry());
                this.ownerComboBox.setSupplier(this.province::getCountry);

                this.controllerComboBox.setValue(this.province.getController());
                this.controllerComboBox.setSupplier(this.province::getController);

                items.add(this.ownerComboBox);
                items.add(this.controllerComboBox);
            }

            if (this.province.isCity()) {
                this.coresField.setValue(FXCollections.observableArrayList(this.province.getCores()));
                this.coresField.setSupplier(this.province::getCores);
                items.add(this.coresField);

                this.claimsField.setValue(FXCollections.observableArrayList(this.province.getClaims()));
                this.claimsField.setSupplier(this.province::getClaims);
                items.add(this.claimsField);

                this.hreField.setValue(this.province.inHre());
                items.add(this.hreField);
            }


            //COLONY
            if (!this.province.isCity()) {
                if (this.province.getColonySize() == null) {
                    items.add(this.colonizeForField);
                } else {
                    this.colonySizeField.setValue(this.province.getColonySize());
                    this.colonySizeField.setSupplier(this.province::getColonySize);
                    items.add(this.colonySizeField);
                }
            }


            //ECONOMY
            this.baseTaxField.setValue(this.province.getBaseTax());
            this.baseTaxField.setSupplier(this.province::getBaseTax);
            items.add(this.baseTaxField);

            this.baseProdField.setValue(this.province.getBaseProduction());
            this.baseProdField.setSupplier(this.province::getBaseProduction);
            items.add(this.baseProdField);

            this.baseMPField.setValue(this.province.getBaseManpower());
            this.baseMPField.setSupplier(this.province::getBaseManpower);
            items.add(this.baseMPField);

            this.tradeGoodField.setValue(this.province.getTradeGood());
            this.tradeGoodField.setSupplier(this.province::getTradeGood);
            items.add(this.tradeGoodField);

            this.latentTradeGoodField.setValue(this.province.getLatentTradeGood());
            this.latentTradeGoodField.setSupplier(this.province::getLatentTradeGood);
            items.add(this.latentTradeGoodField);

            this.cotField.setValue(this.province.getCenterOfTrade());
            this.cotField.setSupplier(this.province::getCenterOfTrade);
            items.add(this.cotField);

            if (this.province.isCity()) {
                this.autonomyField.setValue(this.province.getLocalAutonomy());
                this.autonomyField.setSupplier(this.province::getLocalAutonomy);
                items.add(this.autonomyField);

                this.devastationField.setValue(this.province.getDevastation());
                this.devastationField.setSupplier(this.province::getDevastation);
                items.add(this.devastationField);
            }


            //INSTITUTIONS
            if (!this.province.getInstitutionsProgress().isEmpty()) {
                for (int i = 0; i < this.province.getInstitutionsProgress().size(); i++) {
                    int finalI = i;
                    this.institutionFields.get(i).setValue(this.province.getInstitutionsProgress(i));
                    this.institutionFields.get(i).setSupplier(() -> this.province.getInstitutionsProgress(finalI));
                }

                items.addAll(this.institutionFields);
            }

            if (this.province.isCity()) {
                //BUILDINGS
                this.buildingsFields = new ArrayList<>();
                this.province.getAvailableBuildingsTree().forEach(buildings -> {
                    ObservableSet<Building> buildingsBuilt = FXCollections.observableSet(this.province.getBuildings()
                                                                                                      .stream()
                                                                                                      .filter(buildings::contains)
                                                                                                      .collect(Collectors
                                                                                                                       .toSet()));
                    SelectableGridViewItem<Building> gridViewItem = new SelectableGridViewItem<>(SheetCategory.PROVINCE_BUILDINGS,
                                                                                                 new SelectableGridView<>(FXCollections
                                                                                                                                  .observableList(buildings),
                                                                                                                          buildingsBuilt));

                    gridViewItem.setCellFactory(Building::getLocalizedName, Building::getImageFile);
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
                            items.add(new HBoxItem<Building>(SheetCategory.PROVINCE_BUILDINGS, hBox));
                            i++;
                        } else {
                            items.add(current);
                        }
                    } else {
                        items.add(current);
                    }
                }
            }
        }

        this.propertySheet.getItems().setAll(items);

        this.ownerComboBox.valueProperty().addListener(this.ownerChangeListener);
        return this.propertySheet;
    }

    public void validate(ActionEvent actionEvent) {
        boolean nameChanged;
        boolean capitalChanged;
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
        boolean cotChanged;
        boolean autonomyChanged;
        boolean devastationChanged;
        boolean buildingsChanged;
        boolean colonized;
        boolean colonySizeChanged;

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
            if (!Objects.deepEquals(this.province.getCountry(), this.ownerComboBox.getValue())) {
                this.province.setOwner(this.ownerComboBox.getSelectedValue().getTag());
                this.countryChanged.set(true);
            }
        }

        if (this.controllerComboBox != null) {
            if (!Objects.deepEquals(this.province.getController(), this.controllerComboBox.getSelectedValue())) {
                this.province.setController(this.controllerComboBox.getSelectedValue().getTag());
                controllerChanged = true;
            }
        }

        if (this.coresField != null) {
            if (!Objects.deepEquals(this.province.getCores(), this.coresField.getSelectedValues())) {
                this.province.setCores(this.coresField.getSelectedValues()
                                                      .stream()
                                                      .map(Country::getTag)
                                                      .collect(Collectors.toList()));
                coresChanged = true;
            }
        }

        if (this.claimsField != null) {
            if (!Objects.deepEquals(this.province.getClaims(), this.claimsField.getSelectedValues())) {
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
            if (!Objects.deepEquals(this.province.getCulture(), this.cultureComboBox.getSelectedValue())) {
                this.province.setCulture(this.cultureComboBox.getSelectedValue().getName());
                cultureChanged = true;
            }
        }

        if (this.religionComboBox != null) {
            if (!Objects.deepEquals(this.province.getReligion(), this.religionComboBox.getSelectedValue())) {
                this.province.setReligion(this.religionComboBox.getSelectedValue().getName());
                religionChanged = true;
            }
        }

        if (this.baseTaxField != null) {
            if (!Objects.deepEquals(this.province.getBaseTax(), this.baseTaxField.getTrueValue())) {
                this.province.setBaseTax(this.baseTaxField.getTrueValue());
                baseTaxChanged = true;
            }
        }

        if (this.baseProdField != null) {
            if (!Objects.deepEquals(this.province.getBaseProduction(), this.baseProdField.getTrueValue())) {
                this.province.setBaseProduction(this.baseProdField.getTrueValue());
                baseProdChanged = true;
            }
        }

        if (this.baseMPField != null) {
            if (!Objects.deepEquals(this.province.getBaseManpower(), this.baseMPField.getTrueValue())) {
                this.province.setBaseManpower(this.baseMPField.getTrueValue());
                baseMPChanged = true;
            }
        }

        if (this.institutionFields != null) {
            for (int i = 0; i < this.institutionFields.size(); i++) {
                if (!Objects.deepEquals(this.province.getInstitutionsProgress(i), this.institutionFields.get(i)
                                                                                                        .getDoubleValue())) {
                    this.province.setInstitutionProgress(i, this.institutionFields.get(i).getDoubleValue());
                    anyInstitutionChanged = true;
                }
            }
        }

        if (this.tradeGoodField != null) {
            if (!Objects.deepEquals(this.province.getTradeGood(), this.tradeGoodField.getSelectedValue())) {
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

        if (this.cotField != null) {
            if (!Objects.deepEquals(this.province.getCenterOfTrade(), this.cotField.getTrueValue())) {
                this.province.setCenterOfTrade(this.cotField.getTrueValue());
                cotChanged = true;
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

        if (this.buildingsFields != null) {
            List<Building> buildings = this.buildingsFields.stream()
                                                           .map(SelectableGridViewItem::getSelectedValues)
                                                           .flatMap(Collection::stream)
                                                           .collect(Collectors.toList());
            if (!this.province.getBuildings().equals(buildings)) {
                this.province.setBuildings(buildings);
                buildingsChanged = true;
            }
        }

        if (this.colonySizeField != null) {
            if (!Objects.deepEquals(this.province.getColonySize(), this.colonySizeField.getDoubleValue())) {
                this.province.setColonySize(this.colonySizeField.getDoubleValue());
                colonySizeChanged = true;
            }
        }

        if (this.colonizeForField != null) {
            if (this.colonizeForField.getSelectedValue() != null) {
                this.province.colonize(this.colonizeForField.getSelectedValue());
                colonized = true;
            }
        }
    }

    public final BooleanProperty countryChangedProperty() {
        if (this.countryChanged == null) {
            this.countryChanged = new BooleanPropertyBase() {
                @Override
                public Object getBean() {
                    return ProvincePropertySheet.this;
                }

                @Override
                public String getName() {
                    return "countryChanged";
                }
            };
        }

        return countryChanged;
    }

    public SaveProvince getProvince() {
        return province;
    }

    public ValidationSupport getValidationSupport() {
        return validationSupport;
    }

    public PropertySheet getPropertySheet() {
        return propertySheet;
    }
}
