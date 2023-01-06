package fr.osallek.eu4saveeditor.controller.mapview;

import fr.osallek.clausewitzparser.common.ClausewitzUtils;
import fr.osallek.eu4parser.common.Eu4Utils;
import fr.osallek.eu4parser.common.NumbersUtils;
import fr.osallek.eu4parser.model.game.Building;
import fr.osallek.eu4parser.model.game.Culture;
import fr.osallek.eu4parser.model.game.ParliamentBribe;
import fr.osallek.eu4parser.model.game.TradeGood;
import fr.osallek.eu4parser.model.game.TradeNode;
import fr.osallek.eu4parser.model.game.localisation.Eu4Language;
import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4parser.model.save.SaveGreatProject;
import fr.osallek.eu4parser.model.save.SaveReligion;
import fr.osallek.eu4parser.model.save.country.SaveCountry;
import fr.osallek.eu4parser.model.save.province.ProvinceBuilding;
import fr.osallek.eu4parser.model.save.province.SaveProvince;
import fr.osallek.eu4saveeditor.Eu4SaveEditorApplication;
import fr.osallek.eu4saveeditor.common.Constants;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import fr.osallek.eu4saveeditor.controller.control.ClearableCheckComboBox;
import fr.osallek.eu4saveeditor.controller.control.ClearableComboBox;
import fr.osallek.eu4saveeditor.controller.control.ClearableSpinnerDouble;
import fr.osallek.eu4saveeditor.controller.control.ClearableSpinnerInt;
import fr.osallek.eu4saveeditor.controller.control.SelectableGridView;
import fr.osallek.eu4saveeditor.controller.control.TableView2Modifier;
import fr.osallek.eu4saveeditor.controller.converter.CountryStringCellFactory;
import fr.osallek.eu4saveeditor.controller.converter.CountryStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.CultureStringCellFactory;
import fr.osallek.eu4saveeditor.controller.converter.CultureStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.ParliamentBribeStringCellFactory;
import fr.osallek.eu4saveeditor.controller.converter.ParliamentBribeStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.SaveReligionStringCellFactory;
import fr.osallek.eu4saveeditor.controller.converter.SaveReligionStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.TradeGoodStringCellFactory;
import fr.osallek.eu4saveeditor.controller.converter.TradeGoodStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.TradeNodeStringCellFactory;
import fr.osallek.eu4saveeditor.controller.converter.TradeNodeStringConverter;
import fr.osallek.eu4saveeditor.controller.object.Modifier;
import fr.osallek.eu4saveeditor.controller.pane.CustomPropertySheet;
import fr.osallek.eu4saveeditor.controller.pane.CustomPropertySheetSkin;
import fr.osallek.eu4saveeditor.controller.pane.TableViewDialog;
import fr.osallek.eu4saveeditor.controller.propertyeditor.CustomPropertyEditorFactory;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ButtonItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableCheckBoxItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableCheckComboBoxItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableComboBoxItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSliderItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSpinnerItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableTextItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.HBoxItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.SelectableGridViewItem;
import fr.osallek.eu4saveeditor.controller.validator.CustomGraphicValidationDecoration;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.BooleanPropertyBase;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ChangeListener;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.controlsfx.control.SearchableComboBox;
import org.controlsfx.validation.ValidationSupport;
import org.controlsfx.validation.Validator;
import org.controlsfx.validation.decoration.CompoundValidationDecoration;
import org.controlsfx.validation.decoration.StyleClassValidationDecoration;
import org.springframework.context.MessageSource;
import org.springframework.core.io.ClassPathResource;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

public class ProvincePropertySheet extends VBox {

    private final MessageSource messageSource;

    private SaveProvince province;

    private final CustomPropertySheet propertySheet;

    private final ValidationSupport validationSupport;

    private final ClearableTextItem nameField;

    private final ClearableComboBoxItem<Culture> cultureComboBox;

    private final ClearableComboBoxItem<SaveReligion> religionComboBox;

    private final ClearableTextItem capitalField;

    private final ClearableComboBoxItem<SaveCountry> ownerComboBox;

    private final ClearableComboBoxItem<SaveCountry> controllerComboBox;

    private final ClearableCheckComboBoxItem<SaveCountry> coresField;

    private final ClearableCheckComboBoxItem<SaveCountry> claimsField;

    private final ClearableCheckComboBoxItem<SaveCountry> discoveredField;

    private final ClearableCheckBoxItem hreField;

    private final ClearableSpinnerItem<Integer> nationalismField;

    private final ClearableComboBoxItem<SaveCountry> colonizeForField;

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

    private final ClearableSpinnerItem<Integer> infrastructureField;

    private final List<ClearableSliderItem> institutionFields;

    private final ClearableSliderItem autonomyField;

    private final ClearableSliderItem devastationField;

    private final ClearableComboBoxItem<TradeNode> tradeNodeField;

    private final ClearableCheckBoxItem tradeCompanyField;

    private final List<SelectableGridViewItem<Building>> buildingsFields;

    private final File defaultBuildingImage;

    private final ButtonItem modifiersButton;

    private final ObservableList<Modifier> modifiers;

    private Map<SaveGreatProject, ClearableSpinnerItem<Integer>> greatProjectsField;

    private final ClearableCheckBoxItem parliamentField;

    private final ClearableCheckBoxItem parliamentBackingField;

    private final ClearableComboBoxItem<ParliamentBribe> parliamentBribeField;

    private final ChangeListener<? super SaveCountry> ownerChangeListener;

    private BooleanProperty countryChanged;

    private final CustomPropertySheetSkin propertySheetSkin;

    public ProvincePropertySheet(MessageSource messageSource, Save save, ObservableList<SaveCountry> playableCountries, ObservableList<Culture> cultures,
                                 ObservableList<SaveReligion> religions, ObservableList<TradeGood> tradeGoods, ObservableList<TradeNode> tradeNodes) {
        this.messageSource = messageSource;
        this.propertySheet = new CustomPropertySheet();
        this.propertySheet.setPropertyEditorFactory(new CustomPropertyEditorFactory());
        this.propertySheet.setMode(CustomPropertySheet.Mode.CATEGORY);
        this.propertySheet.setModeSwitcherVisible(false);
        this.propertySheet.setSearchBoxVisible(false);

        this.propertySheetSkin = new CustomPropertySheetSkin(this.propertySheet);
        this.propertySheet.setSkin(this.propertySheetSkin);

        //GENERAL
        this.nameField = new ClearableTextItem(this.messageSource.getMessage("ose.category.general", null, Constants.LOCALE),
                                               save.getGame().getLocalisationClean("LEDGER_NAME", Eu4Language.getDefault()));
        this.nameField.getTextField().getStylesheets().add(Eu4SaveEditorApplication.class.getResource("/styles/style.css").toExternalForm());

        this.validationSupport = new ValidationSupport();
        this.validationSupport.registerValidator(this.nameField.getTextField(), Validator.createEmptyValidator("Text is required"));
        this.validationSupport.setValidationDecorator(new CompoundValidationDecoration(new CustomGraphicValidationDecoration(),
                                                                                       new StyleClassValidationDecoration("validation-error", null)));

        this.capitalField = new ClearableTextItem(this.messageSource.getMessage("ose.category.general", null, Constants.LOCALE),
                                                  save.getGame().getLocalisationClean("TRIGGER_CAPITAL", Eu4Language.getDefault()));

        this.cultureComboBox = new ClearableComboBoxItem<>(this.messageSource.getMessage("ose.category.general", null, Constants.LOCALE),
                                                           save.getGame().getLocalisationClean("LEDGER_CULTURE", Eu4Language.getDefault()),
                                                           cultures,
                                                           new ClearableComboBox<>(new SearchableComboBox<>()));
        this.cultureComboBox.setConverter(CultureStringConverter.INSTANCE);
        this.cultureComboBox.setCellFactory(CultureStringCellFactory.INSTANCE);

        this.religionComboBox = new ClearableComboBoxItem<>(this.messageSource.getMessage("ose.category.general", null, Constants.LOCALE),
                                                            save.getGame().getLocalisationClean("LEDGER_RELIGION", Eu4Language.getDefault()),
                                                            religions,
                                                            new ClearableComboBox<>(new SearchableComboBox<>()));
        this.religionComboBox.setConverter(SaveReligionStringConverter.INSTANCE);
        this.religionComboBox.setCellFactory(SaveReligionStringCellFactory.INSTANCE);

        this.controllerComboBox = new ClearableComboBoxItem<>(this.messageSource.getMessage("ose.category.political", null, Constants.LOCALE),
                                                              save.getGame().getLocalisationClean("SUPPLY_CONTROLLER", Eu4Language.getDefault()),
                                                              playableCountries,
                                                              new ClearableComboBox<>(new SearchableComboBox<>()));
        this.controllerComboBox.setConverter(CountryStringConverter.INSTANCE);
        this.controllerComboBox.setCellFactory(new CountryStringCellFactory());

        this.ownerComboBox = new ClearableComboBoxItem<>(this.messageSource.getMessage("ose.category.political", null, Constants.LOCALE),
                                                         save.getGame().getLocalisationClean("LEDGER_OWNER", Eu4Language.getDefault()),
                                                         playableCountries,
                                                         new ClearableComboBox<>(new SearchableComboBox<>()));
        this.ownerComboBox.setConverter(CountryStringConverter.INSTANCE);
        this.ownerComboBox.setCellFactory(new CountryStringCellFactory());

        this.coresField = new ClearableCheckComboBoxItem<>(this.messageSource.getMessage("ose.category.political", null, Constants.LOCALE),
                                                           save.getGame().getLocalisationClean("LEDGER_CORE", Eu4Language.getDefault()),
                                                           playableCountries,
                                                           new ClearableCheckComboBox<>());
        this.coresField.setConverter(CountryStringConverter.INSTANCE);

        this.claimsField = new ClearableCheckComboBoxItem<>(this.messageSource.getMessage("ose.category.political", null, Constants.LOCALE),
                                                            save.getGame().getLocalisationClean("HAVE_CLAIM_IN", Eu4Language.getDefault()),
                                                            playableCountries,
                                                            new ClearableCheckComboBox<>());
        this.claimsField.setConverter(CountryStringConverter.INSTANCE);

        this.discoveredField = new ClearableCheckComboBoxItem<>(this.messageSource.getMessage("ose.category.political", null, Constants.LOCALE),
                                                                this.messageSource.getMessage("province.discoveredBy", null, Constants.LOCALE),
                                                                playableCountries,
                                                                new ClearableCheckComboBox<>());
        this.discoveredField.setConverter(CountryStringConverter.INSTANCE);

        this.hreField = new ClearableCheckBoxItem(this.messageSource.getMessage("ose.category.political", null, Constants.LOCALE),
                                                  save.getGame().getLocalisationClean("IS_PART_OF_HRE", Eu4Language.getDefault()));

        this.nationalismField = new ClearableSpinnerItem<>(this.messageSource.getMessage("ose.category.political", null, Constants.LOCALE),
                                                           this.messageSource.getMessage("province.nationalism", null, Constants.LOCALE),
                                                           new ClearableSpinnerInt(0, 100, 1));

        this.colonizeForField = new ClearableComboBoxItem<>(this.messageSource.getMessage("ose.category.colony", null, Constants.LOCALE),
                                                            save.getGame().getLocalisationClean("COLONIZE_PROVINCE", Eu4Language.getDefault()),
                                                            playableCountries,
                                                            new ClearableComboBox<>(new SearchableComboBox<>()));
        this.colonizeForField.setConverter(CountryStringConverter.INSTANCE);

        this.nativeHostilenessField = new ClearableSpinnerItem<>(this.messageSource.getMessage("ose.category.colony", null, Constants.LOCALE),
                                                                 save.getGame().getLocalisationClean("PP_AGGRESSIVE", Eu4Language.getDefault()),
                                                                 new ClearableSpinnerInt(0, 10, 1));

        this.nativeFerocityField = new ClearableSpinnerItem<>(this.messageSource.getMessage("ose.category.colony", null, Constants.LOCALE),
                                                              save.getGame().getLocalisationClean("PP_FEROCITY", Eu4Language.getDefault()),
                                                              new ClearableSpinnerInt(0, 10, 1));

        this.nativeSizeField = new ClearableSpinnerItem<>(this.messageSource.getMessage("ose.category.colony", null, Constants.LOCALE),
                                                          save.getGame().getLocalisationClean("PP_NATIVES", Eu4Language.getDefault()),
                                                          new ClearableSpinnerInt(0, 10000, 1));

        this.colonySizeField = new ClearableSliderItem(this.messageSource.getMessage("ose.category.colony", null, Constants.LOCALE),
                                                       save.getGame().getLocalisationClean("LEDGER_POPULATION", Eu4Language.getDefault()),
                                                       0, 1000);

        this.baseTaxField = new ClearableSpinnerItem<>(this.messageSource.getMessage("ose.category.economy", null, Constants.LOCALE),
                                                       save.getGame().getLocalisationClean("LEDGER_TAX", Eu4Language.getDefault()),
                                                       new ClearableSpinnerDouble(1, 999, 1));

        this.baseProdField = new ClearableSpinnerItem<>(this.messageSource.getMessage("ose.category.economy", null, Constants.LOCALE),
                                                        save.getGame().getLocalisationClean("LEDGER_PRODUCTION", Eu4Language.getDefault()),
                                                        new ClearableSpinnerDouble(1, 999, 1));

        this.baseMPField = new ClearableSpinnerItem<>(this.messageSource.getMessage("ose.category.economy", null, Constants.LOCALE),
                                                      save.getGame().getLocalisationClean("LEDGER_MANPOWER", Eu4Language.getDefault()),
                                                      new ClearableSpinnerDouble(1, 999, 1));

        this.tradeGoodField = new ClearableComboBoxItem<>(this.messageSource.getMessage("ose.category.economy", null, Constants.LOCALE),
                                                          save.getGame().getLocalisationClean("LEDGER_GOODS", Eu4Language.getDefault()),
                                                          tradeGoods,
                                                          new ClearableComboBox<>(new SearchableComboBox<>()));
        this.tradeGoodField.setConverter(TradeGoodStringConverter.INSTANCE);
        this.tradeGoodField.setCellFactory(new TradeGoodStringCellFactory(save.getGame()));

        this.tradeNodeField = new ClearableComboBoxItem<>(this.messageSource.getMessage("ose.category.economy", null, Constants.LOCALE),
                                                          save.getGame().getLocalisationClean("TRADENODE", Eu4Language.getDefault()),
                                                          tradeNodes,
                                                          new ClearableComboBox<>(new SearchableComboBox<>()));
        this.tradeNodeField.setConverter(TradeNodeStringConverter.INSTANCE);
        this.tradeNodeField.setCellFactory(new TradeNodeStringCellFactory(save.getGame()));
        this.tradeCompanyField = new ClearableCheckBoxItem(messageSource.getMessage("ose.category.economy", null, Constants.LOCALE),
                                                           messageSource.getMessage("province.trade-company", null, Constants.LOCALE));

        this.latentTradeGoodField = new ClearableComboBoxItem<>(this.messageSource.getMessage("ose.category.economy", null, Constants.LOCALE),
                                                                save.getGame()
                                                                    .getLocalisationClean("LATENT_TRADE_GOODS_TOOLTIP_HEADER", Eu4Language.getDefault()),
                                                                tradeGoods,
                                                                new ClearableComboBox<>(new SearchableComboBox<>()));
        this.latentTradeGoodField.setConverter(TradeGoodStringConverter.INSTANCE);
        this.latentTradeGoodField.setCellFactory(new TradeGoodStringCellFactory(save.getGame()));

        this.cotField = new ClearableSpinnerItem<>(this.messageSource.getMessage("ose.category.economy", null, Constants.LOCALE),
                                                   save.getGame().getLocalisationClean("EST_VAL_COT", Eu4Language.getDefault()),
                                                   new ClearableSpinnerInt(0, 3, 1));

        this.infrastructureField = new ClearableSpinnerItem<>(this.messageSource.getMessage("ose.category.economy", null, Constants.LOCALE),
                                                              this.messageSource.getMessage("province.infrastructure", null, Constants.LOCALE),
                                                              new ClearableSpinnerInt(0, 100, 1));

        this.autonomyField = new ClearableSliderItem(this.messageSource.getMessage("ose.category.economy", null, Constants.LOCALE),
                                                     save.getGame().getLocalisationClean("local_autonomy", Eu4Language.getDefault()),
                                                     0, 100);

        this.devastationField = new ClearableSliderItem(this.messageSource.getMessage("ose.category.economy", null, Constants.LOCALE),
                                                        save.getGame().getLocalisationClean("LEDGER_DEVASTATION", Eu4Language.getDefault()),
                                                        0, 100);

        this.institutionFields = new ArrayList<>();
        for (int i = 0; i < save.getInstitutions().getNbInstitutions(); i++) {
            this.institutionFields.add(new ClearableSliderItem(this.messageSource.getMessage("ose.category.institutions", null, Constants.LOCALE),
                                                               Eu4SaveEditorUtils.localize(save.getGame().getInstitution(i).getName(), save.getGame()),
                                                               0, 100));
        }

        File defaultBuildingImage1;
        this.buildingsFields = new ArrayList<>();
        try {
            defaultBuildingImage1 = new ClassPathResource("images/no_building.png").getFile();
        } catch (IOException ignored) {
            defaultBuildingImage1 = null;
        }
        this.defaultBuildingImage = defaultBuildingImage1;

        Eu4Utils.buildingsTree(save.getGame().getBuildings()).forEach(buildings -> {
            buildings.removeIf(b -> "manufactory".equals(b.getName()));

            if (CollectionUtils.isNotEmpty(buildings)) {
                SelectableGridViewItem<Building> grid = new SelectableGridViewItem<>(this.messageSource.getMessage("ose.category.buildings", null, Constants.LOCALE),
                                                                                     new SelectableGridView<>(FXCollections.observableList(buildings),
                                                                                                              buildings.stream()
                                                                                                                       .anyMatch(b -> CollectionUtils.isNotEmpty(b.getManufactoryFor())),
                                                                                                              FXCollections.observableSet(new HashSet<>())));
                grid.setCellFactory(b -> Eu4SaveEditorUtils.localize("building_" + b.getName(), this.province.getSave().getGame()),
                                    Building::getImage, this.defaultBuildingImage);
                this.buildingsFields.add(grid);
            }
        });

        //Modifiers
        this.modifiers = FXCollections.observableArrayList();
        this.modifiersButton = new ButtonItem(save.getGame().getLocalisationClean("DOMESTIC_MODIFIERS", Eu4Language.getDefault()), null,
                                              save.getGame().getLocalisationClean("DOMESTIC_MODIFIERS", Eu4Language.getDefault()));

        //Great projects
        this.greatProjectsField = new LinkedHashMap<>();

        //Parliament
        this.parliamentField = new ClearableCheckBoxItem(messageSource.getMessage("ose.category.parliament", null, Constants.LOCALE),
                                                         messageSource.getMessage("province.parliament.seat", null, Constants.LOCALE));
        this.parliamentBackingField = new ClearableCheckBoxItem(messageSource.getMessage("ose.category.parliament", null, Constants.LOCALE),
                                                                messageSource.getMessage("province.parliament.backing", null, Constants.LOCALE));
        this.parliamentBackingField.isVisible().bind(this.parliamentField.selectedProperty());
        this.parliamentBackingField.isEditable().bind(this.parliamentField.isEditable().and(this.parliamentField.selectedProperty()));
        this.parliamentBribeField = new ClearableComboBoxItem<>(this.messageSource.getMessage("ose.category.parliament", null, Constants.LOCALE),
                                                                this.messageSource.getMessage("province.parliament.bribe", null, Constants.LOCALE),
                                                                FXCollections.observableList(save.getGame().getParliamentBribes())
                                                                             .sorted(Comparator.comparing(ParliamentBribeStringConverter.INSTANCE::toString,
                                                                                                          Eu4Utils.COLLATOR)),
                                                                new ClearableComboBox<>(new SearchableComboBox<>()));
        this.parliamentBribeField.setConverter(ParliamentBribeStringConverter.INSTANCE);
        this.parliamentBribeField.setCellFactory(ParliamentBribeStringCellFactory.INSTANCE);
        this.parliamentBribeField.isVisible().bind(this.parliamentField.selectedProperty());
        this.parliamentBribeField.editableProperty().bind(this.parliamentField.isEditable().and(this.parliamentField.selectedProperty()));

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
        this.discoveredField.setEditable(false);
        this.hreField.setEditable(false);
        this.nationalismField.setEditable(false);
        this.colonizeForField.setEditable(false);
        this.colonizeForField.setValue(null);
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
        this.infrastructureField.setEditable(false);
        this.institutionFields.forEach(clearableSliderItem -> clearableSliderItem.setEditable(false));
        this.autonomyField.setEditable(false);
        this.devastationField.setEditable(false);
        this.tradeNodeField.setEditable(false);
        this.parliamentField.setEditable(false);
        this.tradeCompanyField.setEditable(false);

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
                this.coresField.setValue(FXCollections.observableList(this.province.getCores()));
                this.coresField.setSupplier(this.province::getCores);
                this.coresField.setEditable(true);
                items.add(this.coresField);

                this.claimsField.setValue(FXCollections.observableList(this.province.getClaims()));
                this.claimsField.setSupplier(this.province::getClaims);
                this.claimsField.setEditable(true);
                items.add(this.claimsField);

                this.hreField.setValue(this.province.inHre());
                this.hreField.setEditable(true);
                items.add(this.hreField);
            }

            this.discoveredField.setValue(FXCollections.observableList(this.province.getDiscoveredBy()));
            this.discoveredField.setSupplier(this.province::getDiscoveredBy);
            this.discoveredField.setEditable(true);
            items.add(this.discoveredField);

            this.nationalismField.setValue(this.province.getNationalism());
            this.nationalismField.setSupplier(this.province::getNationalism);
            this.nationalismField.setEditable(true);
            items.add(this.nationalismField);

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

            this.infrastructureField.setValue(this.province.getExpandInfrastructure());
            this.infrastructureField.setSupplier(this.province::getExpandInfrastructure);
            this.infrastructureField.setEditable(true);
            items.add(this.infrastructureField);

            if (this.province.isCity()) {
                this.autonomyField.setValue(this.province.getTrueLocalAutonomy());
                this.autonomyField.setSupplier(this.province::getTrueLocalAutonomy);
                this.autonomyField.setEditable(true);
                items.add(this.autonomyField);

                this.devastationField.setValue(this.province.getDevastation());
                this.devastationField.setSupplier(this.province::getDevastation);
                this.devastationField.setEditable(true);
                items.add(this.devastationField);
            }

            this.tradeNodeField.setValue(this.province.getTradeNode());
            this.tradeNodeField.setSupplier(this.province::getTradeNode);
            this.tradeNodeField.setEditable(true);
            items.add(this.tradeNodeField);

            if (this.province.getOwner() != null && !this.province.inHre()
                && (this.province.getSaveArea() == null || this.province.getSaveArea().getCountryState(this.province.getOwner()) == null)
                && this.province.getSave().getGame().getTradeCompanies().stream().anyMatch(c -> c.getProvinces().contains(province.getId()))
                && !this.province.getArea().getRegion().getSuperRegion()
                                 .equals(this.province.getOwner().getCapital().getArea().getRegion().getSuperRegion())) {
                this.tradeCompanyField.setEditable(true);
                this.tradeCompanyField.setValue(BooleanUtils.toBoolean(province.activeTradeCompany()));
                this.tradeCompanyField.setSupplier(() -> BooleanUtils.toBoolean(province.activeTradeCompany()));
                items.add(this.tradeCompanyField);
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

            if (this.province.isCity()) {
                //BUILDINGS
                Set<Building> availableBuildings = new HashSet<>(this.province.getAvailableBuildings());
                Set<Building> buildings = this.province.getBuildings().stream().map(ProvinceBuilding::getBuilding).collect(Collectors.toSet());
                this.buildingsFields.forEach(item -> {
                    item.getSelectableGridView().setFilter(availableBuildings::contains);
                    item.getSelectableGridView().getSelectedItems().clear();
                    item.getSelectableGridView().getSelectedItems().addAll(buildings);
                });
                List<SelectableGridViewItem<Building>> grids = this.buildingsFields.stream()
                                                                                   .filter(item -> !item.getSelectableGridView().getItems().isEmpty())
                                                                                   .toList();
                for (int i = 0; i < grids.size(); i++) {
                    SelectableGridViewItem<Building> current = grids.get(i);
                    HBox hBox = new HBox(13);
                    HBox.setHgrow(current.getSelectableGridView(), Priority.ALWAYS);

                    if (i < grids.size() - 1) {
                        SelectableGridViewItem<Building> next = grids.get(i + 1);

                        if (current.getNbItems() <= 4 && next.getNbItems() <= 4) {
                            HBox.setHgrow(next.getSelectableGridView(), Priority.ALWAYS);
                            hBox.getChildren().addAll(current.getSelectableGridView(), next.getSelectableGridView());
                            i++;
                        } else {
                            hBox.getChildren().add(current.getSelectableGridView());
                        }
                    } else {
                        hBox.getChildren().add(current.getSelectableGridView());
                    }

                    items.add(new HBoxItem<>(this.messageSource.getMessage("ose.category.buildings", null, Constants.LOCALE), hBox));
                }
            }
        }

        //Modifiers
        this.modifiers.setAll(this.province.getModifiers().values().stream().map(Modifier::new).collect(Collectors.toList()));
        this.modifiersButton.getButton().setOnAction(event -> {
            TableView2Modifier tableView2Modifier = new TableView2Modifier(this.province.getSave(), this.modifiers);
            TableViewDialog<Modifier> dialog = new TableViewDialog<>(this.province.getSave(),
                                                                     tableView2Modifier,
                                                                     this.province.getSave()
                                                                                  .getGame()
                                                                                  .getLocalisationClean("DOMESTIC_MODIFIERS", Eu4Language.getDefault()),
                                                                     list -> null,
                                                                     () -> this.modifiers);
            dialog.setDisableAddProperty(new SimpleBooleanProperty(true));
            Optional<List<Modifier>> modifierList = dialog.showAndWait();

            modifierList.ifPresent(this.modifiers::setAll);
        });
        items.add(this.modifiersButton);

        //Great projects
        this.greatProjectsField = this.province.getGreatProjects()
                                               .stream()
                                               .map(p -> Map.entry(p,
                                                                   new ClearableSpinnerItem<>(this.messageSource.getMessage("ose.category.great-projects", null, Constants.LOCALE),
                                                                                              Eu4SaveEditorUtils.localize(p.getName(), this.province.getSave()
                                                                                                                                                    .getGame()),
                                                                                              new ClearableSpinnerInt(0, p.getGreatProject().getMaxLevel(),
                                                                                                                      p.getDevelopmentTier(), 1, p::getDevelopmentTier))))
                                               .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        items.addAll(this.greatProjectsField.values());

        //Parliament
        if (province.getOwner() != null && province.getOwner().getParliament() != null) {
            this.parliamentField.setEditable(true);
            this.parliamentField.setValue(province.getSeatInParliament() != null);
            this.parliamentField.setSupplier(() -> province.getSeatInParliament() != null);
            items.add(this.parliamentField);

            this.parliamentBribeField.setFilter(bribe -> bribe.getTrigger() == null || bribe.getTrigger().apply(province.getOwner(), province));

            if (province.getSeatInParliament() != null) {
                this.parliamentBackingField.setValue(BooleanUtils.toBoolean(province.getSeatInParliament().getBack()));
                this.parliamentBackingField.setSupplier(() -> BooleanUtils.toBoolean(province.getSeatInParliament().getBack()));
                this.parliamentBribeField.setValue(province.getSeatInParliament().getBribe());
                this.parliamentBribeField.setSupplier(() -> province.getSeatInParliament().getBribe());
            } else {
                this.parliamentBackingField.setValue(false);
                this.parliamentBackingField.setSupplier(() -> false);
                this.parliamentBribeField.setValue(this.parliamentBribeField.getChoices().get(0));
                this.parliamentBribeField.setSupplier(() -> this.parliamentBribeField.getChoices().get(0));
            }

            items.add(this.parliamentBackingField);
            items.add(this.parliamentBribeField);
        }

        this.propertySheet.getItems().setAll(items);

        if (expandedPaneName != null) {
            this.propertySheetSkin.getAccordion()
                                  .getPanes()
                                  .stream()
                                  .filter(titledPane -> titledPane.getText().equals(expandedPaneName))
                                  .findFirst()
                                  .ifPresent(titledPane -> this.propertySheetSkin.getAccordion().setExpandedPane(titledPane));
        }

        this.ownerComboBox.valueProperty().addListener(this.ownerChangeListener);
    }

    public void validate() {
        if (!ClausewitzUtils.removeQuotes(this.province.getName()).equals(this.nameField.getText())) {
            this.province.setName(this.nameField.getText());
        }

        if (this.capitalField.isEditable().get()) {
            if (!Objects.equals(ClausewitzUtils.removeQuotes(this.province.getCapital()), this.capitalField.getText())) {
                this.province.setCapital(this.capitalField.getText());
            }
        }

        if (this.controllerComboBox.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getController(), this.controllerComboBox.getSelectedValue())) {
                this.province.setController(this.controllerComboBox.getSelectedValue());
            }
        }

        if (this.ownerComboBox.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getOwner(), this.ownerComboBox.getValue())) {
                this.province.setOwner(this.ownerComboBox.getSelectedValue());
                this.countryChanged.set(true);
            }
        }

        if (this.coresField.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getCores(), this.coresField.getSelectedValues())) {
                this.province.setCores(new ArrayList<>(this.coresField.getSelectedValues()));
            }
        }

        if (this.claimsField.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getClaims(), this.claimsField.getSelectedValues())) {
                this.province.setClaims(new ArrayList<>(this.claimsField.getSelectedValues()));
            }
        }

        if (this.discoveredField.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getDiscoveredBy(), this.discoveredField.getSelectedValues())) {
                this.province.setDiscoveredBy(new ArrayList<>(this.discoveredField.getSelectedValues()));
            }
        }

        if (this.hreField.isEditable().get()) {
            if (this.province.inHre() != this.hreField.isSelected()) {
                this.province.setInHre(this.hreField.isSelected());
            }
        }

        if (this.nationalismField.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getNationalism(), this.nationalismField.getTrueValue())) {
                this.province.setNationalism(NumbersUtils.intOrDefault(this.nationalismField.getTrueValue()));
            }
        }

        if (this.cultureComboBox.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getCulture(), this.cultureComboBox.getSelectedValue())) {
                this.province.setCulture(this.cultureComboBox.getSelectedValue());
            }
        }

        if (this.religionComboBox.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getReligion(), this.religionComboBox.getSelectedValue())) {
                this.province.setReligion(this.religionComboBox.getSelectedValue());
            }
        }

        if (this.nativeHostilenessField.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getNativeHostileness(), this.nativeHostilenessField.getTrueValue())) {
                this.province.setNativeHostileness(this.nativeHostilenessField.getTrueValue());
            }
        }

        if (this.nativeFerocityField.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getNativeFerocity(), this.nativeFerocityField.getTrueValue())) {
                this.province.setNativeFerocity(this.nativeFerocityField.getTrueValue());
            }
        }

        if (this.nativeSizeField.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getNativeSize(), this.nativeSizeField.getTrueValue())) {
                this.province.setNativeSize(this.nativeSizeField.getTrueValue());
            }
        }

        if (this.baseTaxField.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getBaseTax(), this.baseTaxField.getTrueValue())) {
                this.province.setBaseTax(this.baseTaxField.getTrueValue());
            }
        }

        if (this.baseProdField.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getBaseProduction(), this.baseProdField.getTrueValue())) {
                this.province.setBaseProduction(this.baseProdField.getTrueValue());
            }
        }

        if (this.baseMPField.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getBaseManpower(), this.baseMPField.getTrueValue())) {
                this.province.setBaseManpower(this.baseMPField.getTrueValue());
            }
        }

        if (!this.institutionFields.isEmpty()) {
            for (int i = 0; i < this.institutionFields.size(); i++) {
                if (this.institutionFields.get(i).isEditable().get()
                    && !Objects.deepEquals(this.province.getInstitutionsProgress(i),
                                           this.institutionFields.get(i).getDoubleValue())) {
                    this.province.setInstitutionProgress(i, this.institutionFields.get(i).getDoubleValue());
                }
            }
        }

        if (this.tradeGoodField.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getTradeGood(), this.tradeGoodField.getSelectedValue())) {
                this.province.setTradeGoods(this.tradeGoodField.getSelectedValue().getName());
            }
        }

        if (this.latentTradeGoodField.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getLatentTradeGood(), this.latentTradeGoodField.getSelectedValue())) {
                this.province.setLatentTradeGoods(this.latentTradeGoodField.getSelectedValue().getName());
            }
        }

        if (this.cotField.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getCenterOfTradeLevel(), this.cotField.getTrueValue())) {
                this.province.setCenterOfTrade(this.cotField.getTrueValue());
            }
        }

        if (this.infrastructureField.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getExpandInfrastructure(), this.infrastructureField.getTrueValue())) {
                this.province.setExpandInfrastructure(this.infrastructureField.getTrueValue());
            }
        }

        if (this.autonomyField.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getTrueLocalAutonomy(), this.autonomyField.getDoubleValue())) {
                this.province.setLocalAutonomy(this.autonomyField.getDoubleValue());
            }
        }

        if (this.devastationField.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getDevastation(), this.devastationField.getDoubleValue())) {
                this.province.setDevastation(this.devastationField.getDoubleValue());
            }
        }

        if (this.tradeNodeField.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getTradeNode(), this.tradeNodeField.getSelectedValue())) {
                this.province.setTradeNode(this.tradeNodeField.getSelectedValue());
            }
        }

        if (this.tradeCompanyField.isEditable().get()) {
            if (!Objects.deepEquals(BooleanUtils.toBoolean(this.province.activeTradeCompany()), this.tradeCompanyField.isSelected())) {
                this.province.setActiveTradeCompany(this.tradeCompanyField.isSelected(), Eu4Language.getDefault());
            }
        }

        if (!this.buildingsFields.isEmpty()) {
            List<Building> buildings = this.buildingsFields.stream()
                                                           .map(SelectableGridViewItem::getSelectedValues)
                                                           .flatMap(Collection::stream)
                                                           .distinct()
                                                           .collect(Collectors.toList());
            if (!CollectionUtils.isEqualCollection(this.province.getBuildings().stream().map(ProvinceBuilding::getBuilding).toList(), buildings)) {
                this.province.setBuildings(buildings);
            }
        }

        if (this.colonySizeField.isEditable().get()) {
            if (!Objects.deepEquals(this.province.getColonySize(), this.colonySizeField.getDoubleValue())) {
                this.province.setColonySize(this.colonySizeField.getDoubleValue());
            }
        }

        if (this.colonizeForField.isEditable().get()) {
            if (this.colonizeForField.getSelectedValue() != null) {
                this.province.colonize(this.colonizeForField.getSelectedValue());
                this.countryChanged.set(true);
            }
        }

        if (this.province.getModifiers().size() != this.modifiers.size() || this.modifiers.stream().anyMatch(Modifier::isChanged)) {
            this.province.getModifiers()
                         .values()
                         .forEach(saveModifier -> this.modifiers.stream()
                                                                .filter(modifier -> saveModifier.getModifier().equals(modifier.getModifier()))
                                                                .findFirst()
                                                                .ifPresentOrElse(modifier -> {
                                                                                     if (!Objects.equals(modifier.getDate(), saveModifier.getDate())) {
                                                                                         saveModifier.setDate(modifier.getDate());
                                                                                     }

                                                                                     this.modifiers.remove(modifier);
                                                                                 },
                                                                                 () -> this.province.removeModifier(saveModifier.getModifier())));
        }

        if (MapUtils.isNotEmpty(this.greatProjectsField) && CollectionUtils.isNotEmpty(this.province.getGreatProjects())) {
            this.greatProjectsField.forEach((p, item) -> {
                if (!Objects.equals(p.getDevelopmentTier(), item.getTrueValue())) {
                    p.setDevelopmentTier(item.getTrueValue());
                }
            });
        }

        if (this.parliamentField.isEditable().get()) {
            if (this.parliamentField.isSelected() && this.province.getSeatInParliament() == null) {
                this.province.addSeatInParliament(this.parliamentBribeField.getSelectedValue());
            } else if (!this.parliamentField.isSelected() && this.province.getSeatInParliament() != null) {
                this.province.removeSeatInParliament();
            }
        }

        if (this.parliamentBackingField.isEditable().get() && this.province.getSeatInParliament() != null) {
            if (this.parliamentBackingField.isSelected() != BooleanUtils.toBoolean(this.province.getSeatInParliament().getBack())) {
                this.province.getSeatInParliament().setBack(this.parliamentBackingField.isSelected());
            }
        }

        if (this.parliamentBribeField.isEditable().get() && this.province.getSeatInParliament() != null) {
            if (!Objects.deepEquals(this.province.getSeatInParliament().getBribe(), this.parliamentBribeField.getSelectedValue())) {
                this.province.getSeatInParliament().setBribe(this.parliamentBribeField.getSelectedValue());
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
