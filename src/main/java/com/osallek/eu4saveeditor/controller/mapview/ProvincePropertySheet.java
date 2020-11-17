package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.clausewitzparser.common.ClausewitzUtils;
import com.osallek.eu4parser.model.game.Building;
import com.osallek.eu4parser.model.game.Culture;
import com.osallek.eu4parser.model.game.TradeGood;
import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.SaveReligion;
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
import com.osallek.eu4saveeditor.controller.pane.CustomPropertySheet;
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
import com.osallek.eu4saveeditor.i18n.SheetCategory;
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

    private final CustomPropertySheet propertySheet;

    private final ValidationSupport validationSupport;

    private final ClearableTextItem nameField;

    private final ClearableComboBoxItem<Culture> cultureComboBox;

    private final ClearableComboBoxItem<SaveReligion> religionComboBox;

    private final ClearableTextItem capitalField;

    private final ClearableComboBoxItem<Country> ownerComboBox;

    private final ClearableComboBoxItem<Country> controllerComboBox;

    private final ClearableCheckComboBoxItem<Country> coresField;

    private final ClearableCheckComboBoxItem<Country> claimsField;

    private final CheckBoxItem hreField;

    private final ClearableComboBoxItem<Country> colonizeForField;

    private final ClearableSliderItem colonySizeField;

    private final ClearableSpinnerItem<Integer> nativeHostilenessField;

    private final ClearableSpinnerItem<Integer> nativeFerocityField;

    private final ClearableSpinnerItem<Integer> nativeSizeField;

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

    private CustomPropertySheetSkin propertySheetSkin;

    public ProvincePropertySheet(Save save, ObservableList<Country> playableCountries,
                                 ObservableList<Culture> cultures, ObservableList<SaveReligion> religions,
                                 ObservableList<TradeGood> tradeGoods) {
        this.propertySheet = new CustomPropertySheet();
        this.propertySheet.setPropertyEditorFactory(new CustomPropertyEditorFactory());
        this.propertySheet.setMode(CustomPropertySheet.Mode.CATEGORY);
        this.propertySheet.setCategoryComparator(Comparator.comparing(SheetCategory::getByLocale));
        this.propertySheet.setModeSwitcherVisible(false);
        this.propertySheet.setSearchBoxVisible(false);

        this.propertySheetSkin = new CustomPropertySheetSkin(this.propertySheet);
        this.propertySheet.setSkin(this.propertySheetSkin);

        //GENERAL
        this.nameField = new ClearableTextItem(SheetCategory.GENERAL,
                                               save.getGame().getLocalisation("LEDGER_NAME"));
        this.nameField.getTextField()
                      .getStylesheets()
                      .add(getClass().getClassLoader().getResource("styles/propertySheetsStyle.css").toExternalForm());

        this.validationSupport = new ValidationSupport();
        this.validationSupport.registerValidator(this.nameField.getTextField(), Validator.createEmptyValidator("Text is required"));
        this.validationSupport.setValidationDecorator(new CompoundValidationDecoration(new CustomGraphicValidationDecoration(),
                                                                                       new StyleClassValidationDecoration("validation-error", null)));

        this.capitalField = new ClearableTextItem(SheetCategory.GENERAL,
                                                  save.getGame().getLocalisation("TRIGGER_CAPITAL"));

        this.cultureComboBox = new ClearableComboBoxItem<>(SheetCategory.GENERAL,
                                                           save.getGame().getLocalisation("LEDGER_CULTURE"),
                                                           cultures,
                                                           new ClearableComboBox<>(new SearchableComboBox<>()));
        this.cultureComboBox.setConverter(new CultureStringConverter());
        this.cultureComboBox.setCellFactory(new CultureStringCellFactory());

        this.religionComboBox = new ClearableComboBoxItem<>(SheetCategory.GENERAL,
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

        this.nativeHostilenessField = new ClearableSpinnerItem<>(SheetCategory.PROVINCE_COLONY,
                                                                 save.getGame().getLocalisationClean("PP_AGGRESSIVE"),
                                                                 new ClearableSpinnerInt(0, 10, 1));

        this.nativeFerocityField = new ClearableSpinnerItem<>(SheetCategory.PROVINCE_COLONY,
                                                              save.getGame().getLocalisationClean("PP_FEROCITY"),
                                                              new ClearableSpinnerInt(0, 10, 1));

        this.nativeSizeField = new ClearableSpinnerItem<>(SheetCategory.PROVINCE_COLONY,
                                                              save.getGame().getLocalisationClean("PP_NATIVES"),
                                                              new ClearableSpinnerInt(0, 10000, 1));

        this.colonySizeField = new ClearableSliderItem(SheetCategory.PROVINCE_COLONY,
                                                       save.getGame().getLocalisation("LEDGER_POPULATION"),
                                                       0, 1000);

        this.baseTaxField = new ClearableSpinnerItem<>(SheetCategory.ECONOMY,
                                                       save.getGame().getLocalisation("LEDGER_TAX"),
                                                       new ClearableSpinnerDouble(1, 999, 1));

        this.baseProdField = new ClearableSpinnerItem<>(SheetCategory.ECONOMY,
                                                        save.getGame().getLocalisation("LEDGER_PRODUCTION"),
                                                        new ClearableSpinnerDouble(1, 999, 1));

        this.baseMPField = new ClearableSpinnerItem<>(SheetCategory.ECONOMY,
                                                      save.getGame().getLocalisation("LEDGER_MANPOWER"),
                                                      new ClearableSpinnerDouble(1, 999, 1));

        this.tradeGoodField = new ClearableComboBoxItem<>(SheetCategory.ECONOMY,
                                                          save.getGame().getLocalisation("LEDGER_GOODS"),
                                                          tradeGoods,
                                                          new ClearableComboBox<>(new SearchableComboBox<>()));
        this.tradeGoodField.setConverter(new TradeGoodStringConverter());
        this.tradeGoodField.setCellFactory(new TradeGoodStringCellFactory());

        this.latentTradeGoodField = new ClearableComboBoxItem<>(SheetCategory.ECONOMY,
                                                                save.getGame()
                                                                    .getLocalisationClean("LATENT_TRADE_GOODS_TOOLTIP_HEADER"),
                                                                tradeGoods,
                                                                new ClearableComboBox<>(new SearchableComboBox<>()));
        this.latentTradeGoodField.setConverter(new TradeGoodStringConverter());
        this.latentTradeGoodField.setCellFactory(new TradeGoodStringCellFactory());

        this.cotField = new ClearableSpinnerItem<>(SheetCategory.ECONOMY,
                                                   save.getGame().getLocalisationClean("EST_VAL_COT"),
                                                   new ClearableSpinnerInt(0, 3, 1));

        this.autonomyField = new ClearableSliderItem(SheetCategory.ECONOMY,
                                                     save.getGame().getLocalisation("local_autonomy"),
                                                     0, 100);

        this.devastationField = new ClearableSliderItem(SheetCategory.ECONOMY,
                                                        save.getGame().getLocalisation("LEDGER_DEVASTATION"),
                                                        0, 100);

        this.institutionFields = new ArrayList<>();
        for (int i = 0; i < save.getInstitutions().getNbInstitutions(); i++) {
            this.institutionFields.add(new ClearableSliderItem(SheetCategory.PROVINCE_INSTITUTIONS,
                                                               save.getGame().getInstitution(i).getLocalizedName(),
                                                               0, 100));
        }

        this.buildingsFields = new ArrayList<>();

        this.ownerChangeListener = (observable, oldValue, newValue) -> {
            this.controllerComboBox.select(newValue);

            if (this.province.isCity()) {
                this.coresField.clearCheck(oldValue);
                this.coresField.check(newValue);
            }
        };
    }

    public void update(SaveProvince province) {
        this.province = province;
        this.countryChanged.set(false);
        String expandedPaneName = this.propertySheetSkin.getAccordion().getExpandedPane() == null ? null :
                                  this.propertySheetSkin.getAccordion().getExpandedPane().getText();

        this.ownerComboBox.valueProperty().removeListener(this.ownerChangeListener);

        List<CustomPropertySheet.Item> items = new ArrayList<>();

        this.cultureComboBox.setEditable(false);
        this.religionComboBox.setEditable(false);
        this.capitalField.setEditable(false);
        this.ownerComboBox.setEditable(false);
        this.controllerComboBox.setEditable(false);
        this.coresField.setEditable(false);
        this.claimsField.setEditable(false);
        this.hreField.setEditable(false);
        this.colonizeForField.setEditable(false);
        this.colonySizeField.setEditable(false);
        this.nativeHostilenessField.setEditable(false);
        this.nativeFerocityField.setEditable(false);
        this.nativeSizeField.setEditable(false);
        this.baseTaxField.setEditable(false);
        this.baseProdField.setEditable(false);
        this.baseMPField.setEditable(false);
        this.tradeGoodField.setEditable(false);
        this.latentTradeGoodField.setEditable(false);
        this.cotField.setEditable(false);
        this.institutionFields.forEach(clearableSliderItem -> clearableSliderItem.setEditable(false));
        this.autonomyField.setEditable(false);
        this.devastationField.setEditable(false);

        //GENERAL
        this.nameField.setValue(ClausewitzUtils.removeQuotes(this.province.getName()));
        this.nameField.setSupplier(() -> ClausewitzUtils.removeQuotes(this.province.getName()));
        items.add(this.nameField);

        if (province.isColonizable()) {
            //GENERAL
            this.capitalField.setValue(ClausewitzUtils.removeQuotes(this.province.getCapital()));
            this.capitalField.setSupplier(() -> ClausewitzUtils.removeQuotes(this.province.getCapital()));
            this.capitalField.setEditable(true);
            items.add(this.capitalField);

            this.cultureComboBox.setValue(this.province.getCulture());
            this.cultureComboBox.setSupplier(this.province::getCulture);
            this.cultureComboBox.setEditable(true);
            items.add(this.cultureComboBox);

            this.religionComboBox.setValue(this.province.getReligion());
            this.religionComboBox.setSupplier(this.province::getReligion);
            this.religionComboBox.setEditable(true);
            items.add(this.religionComboBox);

            //POLITICAL
            if (this.province.isCity() || this.province.getColonySize() != null) {
                this.ownerComboBox.setValue(this.province.getOwner());
                this.ownerComboBox.setSupplier(this.province::getOwner);
                this.ownerComboBox.setEditable(true);
                items.add(this.ownerComboBox);

                this.controllerComboBox.setValue(this.province.getController());
                this.controllerComboBox.setSupplier(this.province::getController);
                this.controllerComboBox.setEditable(true);
                items.add(this.controllerComboBox);
            }

            if (this.province.isCity()) {
                this.coresField.setValue(FXCollections.observableArrayList(this.province.getCores()));
                this.coresField.setSupplier(this.province::getCores);
                this.coresField.setEditable(true);
                items.add(this.coresField);

                this.claimsField.setValue(FXCollections.observableArrayList(this.province.getClaims()));
                this.claimsField.setSupplier(this.province::getClaims);
                this.claimsField.setEditable(true);
                items.add(this.claimsField);

                this.hreField.setValue(this.province.inHre());
                this.hreField.setEditable(true);
                items.add(this.hreField);
            }


            //COLONY
            if (!this.province.isCity()) {
                if (this.province.getColonySize() == null) {
                    this.colonizeForField.setEditable(true);
                    items.add(this.colonizeForField);
                } else {
                    this.colonySizeField.setValue(this.province.getColonySize());
                    this.colonySizeField.setSupplier(this.province::getColonySize);
                    this.colonySizeField.setEditable(true);
                    items.add(this.colonySizeField);
                }

                this.nativeSizeField.setValue(this.province.getNativeSize());
                this.nativeSizeField.setSupplier(this.province::getNativeSize);
                this.nativeSizeField.setEditable(true);
                items.add(this.nativeSizeField);

                this.nativeHostilenessField.setValue(this.province.getNativeHostileness());
                this.nativeHostilenessField.setSupplier(this.province::getNativeHostileness);
                this.nativeHostilenessField.setEditable(true);
                items.add(this.nativeHostilenessField);

                this.nativeFerocityField.setValue(this.province.getNativeFerocity());
                this.nativeFerocityField.setSupplier(this.province::getNativeFerocity);
                this.nativeFerocityField.setEditable(true);
                items.add(this.nativeFerocityField);
            }


            //ECONOMY
            this.baseTaxField.setValue(this.province.getBaseTax());
            this.baseTaxField.setSupplier(this.province::getBaseTax);
            this.baseTaxField.setEditable(true);
            items.add(this.baseTaxField);

            this.baseProdField.setValue(this.province.getBaseProduction());
            this.baseProdField.setSupplier(this.province::getBaseProduction);
            this.baseProdField.setEditable(true);
            items.add(this.baseProdField);

            this.baseMPField.setValue(this.province.getBaseManpower());
            this.baseMPField.setSupplier(this.province::getBaseManpower);
            this.baseMPField.setEditable(true);
            items.add(this.baseMPField);

            this.tradeGoodField.setValue(this.province.getTradeGood());
            this.tradeGoodField.setSupplier(this.province::getTradeGood);
            this.tradeGoodField.setEditable(true);
            items.add(this.tradeGoodField);

            this.latentTradeGoodField.setValue(this.province.getLatentTradeGood());
            this.latentTradeGoodField.setSupplier(this.province::getLatentTradeGood);
            this.latentTradeGoodField.setEditable(true);
            items.add(this.latentTradeGoodField);

            this.cotField.setValue(this.province.getCenterOfTradeLevel());
            this.cotField.setSupplier(this.province::getCenterOfTradeLevel);
            this.cotField.setEditable(true);
            items.add(this.cotField);

            if (this.province.isCity()) {
                this.autonomyField.setValue(this.province.getLocalAutonomy());
                this.autonomyField.setSupplier(this.province::getLocalAutonomy);
                this.autonomyField.setEditable(true);
                items.add(this.autonomyField);

                this.devastationField.setValue(this.province.getDevastation());
                this.devastationField.setSupplier(this.province::getDevastation);
                this.devastationField.setEditable(true);
                items.add(this.devastationField);
            }


            //INSTITUTIONS
            if (!this.province.getInstitutionsProgress().isEmpty()) {
                for (int i = 0; i < this.province.getInstitutionsProgress().size(); i++) {
                    int finalI = i;
                    this.institutionFields.get(i).setEditable(true);
                    this.institutionFields.get(i).setValue(this.province.getInstitutionsProgress(i));
                    this.institutionFields.get(i).setSupplier(() -> this.province.getInstitutionsProgress(finalI));
                }

                items.addAll(this.institutionFields);
            }

            this.buildingsFields.clear();
            if (this.province.isCity()) {
                //BUILDINGS
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

        if (expandedPaneName != null) {
            this.propertySheetSkin.getAccordion()
                                  .getPanes()
                                  .stream()
                                  .filter(titledPane -> titledPane.getText().equals(expandedPaneName))
                                  .findFirst()
                                  .ifPresent(titledPane -> this.propertySheetSkin.getAccordion()
                                                                                 .setExpandedPane(titledPane));
        }

        this.ownerComboBox.valueProperty().addListener(this.ownerChangeListener);
    }

    public void validate(ActionEvent actionEvent) {
        if (!ClausewitzUtils.removeQuotes(this.province.getName()).equals(this.nameField.getText())) {
            this.province.setName(this.nameField.getText());
        }

        if (this.capitalField.isEditable().getValue()) {
            if (!ClausewitzUtils.removeQuotes(this.province.getCapital()).equals(this.capitalField.getText())) {
                this.province.setCapital(this.capitalField.getText());
            }
        }

        if (this.controllerComboBox.isEditable().getValue()) {
            if (!Objects.deepEquals(this.province.getController(), this.controllerComboBox.getSelectedValue())) {
                this.province.setController(this.controllerComboBox.getSelectedValue());
            }
        }

        if (this.ownerComboBox.isEditable().getValue()) {
            if (!Objects.deepEquals(this.province.getOwner(), this.ownerComboBox.getValue())) {
                this.province.setOwner(this.ownerComboBox.getSelectedValue());
                this.countryChanged.set(true);
            }
        }

        if (this.coresField.isEditable().getValue()) {
            if (!Objects.deepEquals(this.province.getCores(), this.coresField.getSelectedValues())) {
                this.province.setCores(new ArrayList<>(this.coresField.getSelectedValues()));
            }
        }

        if (this.claimsField.isEditable().getValue()) {
            if (!Objects.deepEquals(this.province.getClaims(), this.claimsField.getSelectedValues())) {
                this.province.setClaims(new ArrayList<>(this.claimsField.getSelectedValues()));
            }
        }

        if (this.hreField.isEditable().getValue()) {
            if (this.province.inHre() != this.hreField.isSelected()) {
                this.province.setInHre(this.hreField.isSelected());
            }
        }

        if (this.cultureComboBox.isEditable().getValue()) {
            if (!Objects.deepEquals(this.province.getCulture(), this.cultureComboBox.getSelectedValue())) {
                this.province.setCulture(this.cultureComboBox.getSelectedValue());
            }
        }

        if (this.religionComboBox.isEditable().getValue()) {
            if (!Objects.deepEquals(this.province.getReligion(), this.religionComboBox.getSelectedValue())) {
                this.province.setReligion(this.religionComboBox.getSelectedValue());
            }
        }

        if (this.nativeHostilenessField.isEditable().getValue()) {
            if (!Objects.deepEquals(this.province.getNativeHostileness(), this.nativeHostilenessField.getTrueValue())) {
                this.province.setNativeHostileness(this.nativeHostilenessField.getTrueValue());
            }
        }

        if (this.nativeFerocityField.isEditable().getValue()) {
            if (!Objects.deepEquals(this.province.getNativeFerocity(), this.nativeFerocityField.getTrueValue())) {
                this.province.setNativeFerocity(this.nativeFerocityField.getTrueValue());
            }
        }

        if (this.nativeSizeField.isEditable().getValue()) {
            if (!Objects.deepEquals(this.province.getNativeSize(), this.nativeSizeField.getTrueValue())) {
                this.province.setNativeSize(this.nativeSizeField.getTrueValue());
            }
        }

        if (this.baseTaxField.isEditable().getValue()) {
            if (!Objects.deepEquals(this.province.getBaseTax(), this.baseTaxField.getTrueValue())) {
                this.province.setBaseTax(this.baseTaxField.getTrueValue());
            }
        }

        if (this.baseProdField.isEditable().getValue()) {
            if (!Objects.deepEquals(this.province.getBaseProduction(), this.baseProdField.getTrueValue())) {
                this.province.setBaseProduction(this.baseProdField.getTrueValue());
            }
        }

        if (this.baseMPField.isEditable().getValue()) {
            if (!Objects.deepEquals(this.province.getBaseManpower(), this.baseMPField.getTrueValue())) {
                this.province.setBaseManpower(this.baseMPField.getTrueValue());
            }
        }

        if (!this.institutionFields.isEmpty()) {
            for (int i = 0; i < this.institutionFields.size(); i++) {
                if (this.institutionFields.get(i).isEditable().getValue()
                    && !Objects.deepEquals(this.province.getInstitutionsProgress(i),
                                           this.institutionFields.get(i).getDoubleValue())) {
                    this.province.setInstitutionProgress(i, this.institutionFields.get(i).getDoubleValue());
                }
            }
        }

        if (this.tradeGoodField.isEditable().getValue()) {
            if (!Objects.deepEquals(this.province.getTradeGood(), this.tradeGoodField.getSelectedValue())) {
                this.province.setTradeGoods(this.tradeGoodField.getSelectedValue().getName());
            }
        }

        if (this.latentTradeGoodField.isEditable().getValue()) {
            if (!Objects.deepEquals(this.province.getLatentTradeGood(), this.latentTradeGoodField.getSelectedValue())) {
                this.province.setLatentTradeGoods(this.latentTradeGoodField.getSelectedValue().getName());
            }
        }

        if (this.cotField.isEditable().getValue()) {
            if (!Objects.deepEquals(this.province.getCenterOfTradeLevel(), this.cotField.getTrueValue())) {
                this.province.setCenterOfTrade(this.cotField.getTrueValue());
            }
        }

        if (this.autonomyField.isEditable().getValue()) {
            if (!Objects.deepEquals(this.province.getLocalAutonomy(), this.autonomyField.getDoubleValue())) {
                this.province.setLocalAutonomy(this.autonomyField.getDoubleValue());
            }
        }

        if (this.devastationField.isEditable().getValue()) {
            if (!Objects.deepEquals(this.province.getDevastation(), this.devastationField.getDoubleValue())) {
                this.province.setDevastation(this.devastationField.getDoubleValue());
            }
        }

        if (!this.buildingsFields.isEmpty()) {
            List<Building> buildings = this.buildingsFields.stream()
                                                           .map(SelectableGridViewItem::getSelectedValues)
                                                           .flatMap(Collection::stream)
                                                           .collect(Collectors.toList());
            if (!this.province.getBuildings().equals(buildings)) {
                this.province.setBuildings(buildings);
            }
        }

        if (this.colonySizeField.isEditable().getValue()) {
            if (!Objects.deepEquals(this.province.getColonySize(), this.colonySizeField.getDoubleValue())) {
                this.province.setColonySize(this.colonySizeField.getDoubleValue());
            }
        }

        if (this.colonizeForField.isEditable().getValue()) {
            if (this.colonizeForField.getSelectedValue() != null) {
                this.province.colonize(this.colonizeForField.getSelectedValue());
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

    public CustomPropertySheet getPropertySheet() {
        return propertySheet;
    }
}
