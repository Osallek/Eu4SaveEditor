package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.eu4parser.model.game.ImperialReform;
import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.changeprices.ChangePrice;
import com.osallek.eu4parser.model.save.changeprices.ChangePriceGood;
import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4parser.model.save.gameplayoptions.CustomNationDifficulty;
import com.osallek.eu4parser.model.save.gameplayoptions.Difficulty;
import com.osallek.eu4parser.model.save.province.SaveProvince;
import com.osallek.eu4saveeditor.controller.control.ClearableComboBox;
import com.osallek.eu4saveeditor.controller.control.ClearableSpinnerDouble;
import com.osallek.eu4saveeditor.controller.control.ListSelectionViewCountry;
import com.osallek.eu4saveeditor.controller.control.ListSelectionViewImperialReform;
import com.osallek.eu4saveeditor.controller.control.RequiredComboBox;
import com.osallek.eu4saveeditor.controller.control.TableView2ChangePrice;
import com.osallek.eu4saveeditor.controller.converter.CountryStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.CountryStringConverter;
import com.osallek.eu4saveeditor.controller.converter.CustomNationDifficultyStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.CustomNationDifficultyStringConverter;
import com.osallek.eu4saveeditor.controller.converter.DifficultyStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.DifficultyStringConverter;
import com.osallek.eu4saveeditor.controller.converter.ProvinceStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.ProvinceStringConverter;
import com.osallek.eu4saveeditor.controller.pane.CustomPropertySheetSkin;
import com.osallek.eu4saveeditor.controller.pane.ListSelectionViewDialog;
import com.osallek.eu4saveeditor.controller.pane.TableView2Dialog;
import com.osallek.eu4saveeditor.controller.propertyeditor.CustomPropertyEditorFactory;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ButtonItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.CheckBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSliderItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSpinnerItem;
import com.osallek.eu4saveeditor.controller.validator.CustomGraphicValidationDecoration;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.event.ActionEvent;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import org.controlsfx.control.PropertySheet;
import org.controlsfx.control.SearchableComboBox;
import org.controlsfx.validation.ValidationSupport;
import org.controlsfx.validation.decoration.CompoundValidationDecoration;
import org.controlsfx.validation.decoration.StyleClassValidationDecoration;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class SavePropertySheet extends VBox {

    private Save save;

    private final PropertySheet propertySheet;

    private final ValidationSupport validationSupport;

    private final ClearableComboBoxItem<Difficulty> difficultyField;

    private final CheckBoxItem allowHotJoinField;

    private final CheckBoxItem allowCoopField;

    private final CheckBoxItem terraIncognitaField;

    private final CheckBoxItem saveEditableField;

    private final CheckBoxItem lockedLedgerField;

    private final CheckBoxItem limitedLedgerField;

    private final CheckBoxItem dynamicProvinceNamesField;

    private final ClearableComboBoxItem<CustomNationDifficulty> customNationDifficultyField;

    private final CheckBoxItem addNationsToGameField;

    private final CheckBoxItem showMonthlyTaxIncomeField;

    private final CheckBoxItem colorWastelandsField;

    private final CheckBoxItem exclavesRegionNameField;

    private final CheckBoxItem blockNationRuiningField;

    private final CheckBoxItem unlimitedIdeaGroupsField;

    private final CheckBoxItem allowNameChangeField;

    private final CheckBoxItem onlyHostCanPauseField;

    private final CheckBoxItem onlyHostAndObserversCanSaveField;

    private final CheckBoxItem allowTeamsField;

    private final CheckBoxItem allowFreeTeamCreationField;

    private final List<CheckBoxItem> institutionAvailableFields;

    private final List<ClearableComboBoxItem<SaveProvince>> institutionOriginFields;

    private final List<ClearableSpinnerItem<Double>> goodsPricesFields;

    private final Map<String, List<ChangePrice>> goodsChangePrices;

    private ClearableComboBoxItem<Country> hreEmperor;

    private ObservableList<Country> hreElectors;

    private ClearableSliderItem hreInfluenceField;

    private ObservableList<ImperialReform> passedHreMainLineReforms;

    private ObservableList<ImperialReform> notPassedHreMainLineReforms;

    private ObservableList<ImperialReform> passedHreLeftBranchReforms;

    private ObservableList<ImperialReform> notPassedHreLeftBranchReforms;

    private ObservableList<ImperialReform> passedHreRightBranchReforms;

    private ObservableList<ImperialReform> notPassedHreRightBranchReforms;

    private CustomPropertySheetSkin propertySheetSkin;

    public SavePropertySheet(Save save, ObservableList<Country> playableCountries, ObservableList<Country> countriesAlive, ObservableList<SaveProvince> cities) {
        this.save = save;
        this.propertySheet = new PropertySheet();
        this.propertySheet.setPropertyEditorFactory(new CustomPropertyEditorFactory());
        this.propertySheet.setMode(PropertySheet.Mode.CATEGORY);
        this.propertySheet.setCategoryComparator(Comparator.comparing(SheetCategory::getByLocale));
        this.propertySheet.setModeSwitcherVisible(false);
        this.propertySheet.setSearchBoxVisible(false);

        List<PropertySheet.Item> items = new ArrayList<>();

        this.propertySheetSkin = new CustomPropertySheetSkin(this.propertySheet);
        this.propertySheet.setSkin(this.propertySheetSkin);

        this.validationSupport = new ValidationSupport();
        this.validationSupport.setValidationDecorator(new CompoundValidationDecoration(new CustomGraphicValidationDecoration(),
                                                                                       new StyleClassValidationDecoration("validation-error", null)));

        //GAME OPTIONS
        this.difficultyField = new ClearableComboBoxItem<>(SheetCategory.SAVE_GAME_OPTIONS,
                                                           this.save.getGame().getLocalisation("FE_DIFFICULTY"),
                                                           FXCollections.observableArrayList(Difficulty.values()),
                                                           this.save.getGameplayOptions().getDifficulty(),
                                                           this.save.getGame().getLocalisationClean("FE_BONUSES_DESC"),
                                                           new ClearableComboBox<>(new RequiredComboBox<>()));
        this.difficultyField.setConverter(new DifficultyStringConverter(this.save));
        this.difficultyField.setCellFactory(new DifficultyStringCellFactory(this.save));
        this.difficultyField.setSupplier(() -> this.save.getGameplayOptions().getDifficulty());

        this.allowHotJoinField = new CheckBoxItem(SheetCategory.SAVE_GAME_OPTIONS,
                                                  this.save.getGame().getLocalisation("ALLOW_HOTJOIN"),
                                                  this.save.getGameplayOptions().getAllowHotjoin(),
                                                  this.save.getGame().getLocalisationClean("FE_HOTJOIN_DESC"));

        this.allowCoopField = new CheckBoxItem(SheetCategory.SAVE_GAME_OPTIONS,
                                               this.save.getGame().getLocalisation("ALLOW_COOP_MP"),
                                               this.save.getGameplayOptions().getAllowCoop(),
                                               this.save.getGame().getLocalisationClean("FE_COOP_DESC"));

        this.terraIncognitaField = new CheckBoxItem(SheetCategory.SAVE_GAME_OPTIONS,
                                                    this.save.getGame().getLocalisation("FE_USE_TI"),
                                                    this.save.getGameplayOptions().getTerraIncognita(),
                                                    this.save.getGame().getLocalisationClean("FE_USETI_DESC"));

        this.saveEditableField = new CheckBoxItem(SheetCategory.SAVE_GAME_OPTIONS,
                                                  this.save.getGame().getLocalisation("FE_EDIT_SAVE"),
                                                  this.save.getGameplayOptions().getSaveEditable(),
                                                  this.save.getGame().getLocalisationClean("FE_EDIT_SAVE_DESC"));

        this.lockedLedgerField = new CheckBoxItem(SheetCategory.SAVE_GAME_OPTIONS,
                                                  this.save.getGame().getLocalisation("FE_LOCK_LEDGER"),
                                                  this.save.getGameplayOptions().getLockedLedger(),
                                                  this.save.getGame().getLocalisationClean("FE_LOCKED_LEDGER_DESC"));

        this.limitedLedgerField = new CheckBoxItem(SheetCategory.SAVE_GAME_OPTIONS,
                                                   this.save.getGame().getLocalisation("FE_LIMITED_LEDGER"),
                                                   this.save.getGameplayOptions().getLimitedLedger(),
                                                   this.save.getGame().getLocalisationClean("FE_LIMITED_LEDGER_DESC"));

        this.dynamicProvinceNamesField = new CheckBoxItem(SheetCategory.SAVE_GAME_OPTIONS,
                                                          this.save.getGame()
                                                                   .getLocalisation("FE_USE_DYNAMIC_PROVINCE_NAMES"),
                                                          this.save.getGameplayOptions().getDynamicProvinceNames(),
                                                          this.save.getGame()
                                                                   .getLocalisationClean("FE_DYNAMIC_PROVINCE_DESC"));

        this.customNationDifficultyField = new ClearableComboBoxItem<>(SheetCategory.SAVE_GAME_OPTIONS,
                                                                       this.save.getGame()
                                                                                .getLocalisation("FE_CUSTOM_NATION_DIFFICULTY"),
                                                                       FXCollections.observableArrayList(CustomNationDifficulty
                                                                                                                 .values()),
                                                                       this.save.getGameplayOptions()
                                                                                .getCustomNationDifficulty(),
                                                                       this.save.getGame()
                                                                                .getLocalisation("CN_DIFFICULTY_TOOLTIP"),
                                                                       new ClearableComboBox<>(new RequiredComboBox<>()));
        this.customNationDifficultyField.setConverter(new CustomNationDifficultyStringConverter(this.save));
        this.customNationDifficultyField.setCellFactory(new CustomNationDifficultyStringCellFactory(this.save));
        this.customNationDifficultyField.setSupplier(() -> this.save.getGameplayOptions().getCustomNationDifficulty());

        this.addNationsToGameField = new CheckBoxItem(SheetCategory.SAVE_GAME_OPTIONS,
                                                      this.save.getGame()
                                                               .getLocalisation("FE_CUSTOM_NATION_ADD_TO_SAVE"),
                                                      this.save.getGameplayOptions().getAddNationsToGame(),
                                                      this.save.getGame()
                                                               .getLocalisationClean("CN_ADD_TO_SAVE_TOOLTIP1"));

        this.showMonthlyTaxIncomeField = new CheckBoxItem(SheetCategory.SAVE_GAME_OPTIONS,
                                                          this.save.getGame()
                                                                   .getLocalisation("SHOW_MONTHLY_TAX_INCOME"),
                                                          this.save.getGameplayOptions().getShowMonthlyTaxIncome(),
                                                          this.save.getGame()
                                                                   .getLocalisationClean("SHOW_MONTHLY_TAX_INCOME_TOOLTIP"));

        this.colorWastelandsField = new CheckBoxItem(SheetCategory.SAVE_GAME_OPTIONS,
                                                     this.save.getGame().getLocalisation("COLOR_WASTELANDS"),
                                                     this.save.getGameplayOptions().getColorWastelands(),
                                                     this.save.getGame()
                                                              .getLocalisationClean("COLOR_WASTELANDS_TOOLTIP"));

        this.exclavesRegionNameField = new CheckBoxItem(SheetCategory.SAVE_GAME_OPTIONS,
                                                        this.save.getGame().getLocalisation("USE_REGION_NAMES"),
                                                        this.save.getGameplayOptions().getExclavesRegionName(),
                                                        this.save.getGame()
                                                                 .getLocalisationClean("USE_REGION_NAMES_TOOLTIP"));

        this.blockNationRuiningField = new CheckBoxItem(SheetCategory.SAVE_GAME_OPTIONS,
                                                        this.save.getGame().getLocalisation("FE_BLOCK_NATION_RUINING"),
                                                        this.save.getGameplayOptions().getBlockNationRuining(),
                                                        this.save.getGame()
                                                                 .getLocalisationClean("FE_BLOCK_NATION_RUINING_DESC"));

        this.unlimitedIdeaGroupsField = new CheckBoxItem(SheetCategory.SAVE_GAME_OPTIONS,
                                                         this.save.getGame().getLocalisation("USE_ANY_IDEAGROUP"),
                                                         this.save.getGameplayOptions().getUnlimitedIdeaGroups(),
                                                         this.save.getGame()
                                                                  .getLocalisationClean("FE_NO_LIMITS_ON_IDEAS_DESC"));

        this.allowNameChangeField = new CheckBoxItem(SheetCategory.SAVE_GAME_OPTIONS,
                                                     this.save.getGame().getLocalisation("FE_ALLOW_NAME_CHANGE"),
                                                     this.save.getGameplayOptions().getAllowNameChange(),
                                                     this.save.getGame()
                                                              .getLocalisationClean("FE_ALLOW_NAME_CHANGE_DESC"));

        this.onlyHostCanPauseField = new CheckBoxItem(SheetCategory.SAVE_GAME_OPTIONS,
                                                      this.save.getGame().getLocalisation("FE_ONLY_HOST_PAUSE"),
                                                      this.save.getGameplayOptions().getOnlyHostCanPause(),
                                                      this.save.getGame()
                                                               .getLocalisationClean("FE_ONLY_HOST_PAUSE_DESC"));

        this.onlyHostAndObserversCanSaveField = new CheckBoxItem(SheetCategory.SAVE_GAME_OPTIONS,
                                                                 this.save.getGame()
                                                                          .getLocalisation("FE_ONLY_HOST_SAVE"),
                                                                 this.save.getGameplayOptions()
                                                                          .getOnlyHostAndObserversCanSave(),
                                                                 this.save.getGame()
                                                                          .getLocalisationClean("FE_ONLY_HOST_SAVE_DESC"));

        this.allowTeamsField = new CheckBoxItem(SheetCategory.SAVE_GAME_OPTIONS,
                                                this.save.getGame().getLocalisation("FE_USE_TEAMS"),
                                                this.save.getGameplayOptions()
                                                         .getAllowTeams(),
                                                this.save.getGame()
                                                         .getLocalisationClean("USE_TEAMS_TOOLTIP"));

        this.allowFreeTeamCreationField = new CheckBoxItem(SheetCategory.SAVE_GAME_OPTIONS,
                                                           this.save.getGame()
                                                                    .getLocalisation("FE_ALLOW_FREE_TEAM_CREATION"),
                                                           this.save.getGameplayOptions()
                                                                    .getAllowFreeTeamCreation(),
                                                           this.save.getGame()
                                                                    .getLocalisationClean("ALLOW_FREE_TEAM_CREATION_TOOLTIP"));
        items.add(this.difficultyField);
        items.add(this.terraIncognitaField);
        items.add(this.dynamicProvinceNamesField);
        items.add(this.showMonthlyTaxIncomeField);
        items.add(this.colorWastelandsField);
        items.add(this.exclavesRegionNameField);
        items.add(this.unlimitedIdeaGroupsField);
        items.add(this.allowHotJoinField);
        items.add(this.allowCoopField);
        items.add(this.onlyHostAndObserversCanSaveField);
        items.add(this.onlyHostCanPauseField);
        items.add(this.saveEditableField);
        items.add(this.lockedLedgerField);
        items.add(this.limitedLedgerField);
        items.add(this.allowTeamsField);
        items.add(this.allowFreeTeamCreationField);
        items.add(this.blockNationRuiningField);
        items.add(this.allowNameChangeField);
        items.add(this.customNationDifficultyField);
        items.add(this.addNationsToGameField);

        //INSTITUTIONS
        this.institutionOriginFields = new ArrayList<>();
        this.institutionAvailableFields = new ArrayList<>();
        for (int i = 0; i < this.save.getInstitutions().getNbInstitutions(); i++) {
            CheckBoxItem checkBoxItem = new CheckBoxItem(SheetCategory.SAVE_INSTITUTIONS,
                                                         this.save.getGame().getInstitution(i).getLocalizedName(),
                                                         this.save.getInstitutions().isAvailable(i));

            int finalI = i;
            ClearableComboBoxItem<SaveProvince> comboBoxItem = new ClearableComboBoxItem<>(SheetCategory.SAVE_INSTITUTIONS,
                                                                                           this.save.getGame()
                                                                                                    .getInstitution(i)
                                                                                                    .getLocalizedName(),
                                                                                           cities,
                                                                                           this.save.getInstitutions()
                                                                                                    .getOrigin(i),
                                                                                           new ClearableComboBox<>(new SearchableComboBox<>(),
                                                                                                                   () -> this.save
                                                                                                                           .getInstitutions()
                                                                                                                           .getOrigin(finalI)));
            comboBoxItem.setConverter(new ProvinceStringConverter());
            comboBoxItem.setCellFactory(new ProvinceStringCellFactory());

            this.institutionAvailableFields.add(checkBoxItem);
            this.institutionOriginFields.add(comboBoxItem);
            items.add(checkBoxItem);
            items.add(comboBoxItem);
        }

        //GOODS
        this.goodsPricesFields = new ArrayList<>();
        this.goodsChangePrices = new HashMap<>();
        for (int i = 0; i < this.save.getChangePrices().getGoods().size(); i++) {
            ChangePriceGood good = this.save.getChangePrices().getGood(i);
            this.goodsChangePrices.put(good.getName(), new ArrayList<>(good.getChangePrices()));
            Text text = new Text(goodToDesc(good, good.getCurrentPrice()));
            ClearableSpinnerItem<Double> priceSpinnerItem = new ClearableSpinnerItem<>(SheetCategory.SAVE_GOODS,
                                                                                       save.getGame()
                                                                                           .getLocalisation(good.getName()),
                                                                                       new ClearableSpinnerDouble(0,
                                                                                                                  100,
                                                                                                                  good.getCurrentPrice(),
                                                                                                                  0.5,
                                                                                                                  good::getCurrentPrice,
                                                                                                                  text));
            priceSpinnerItem.getSpinner()
                            .getSpinner()
                            .getEditor()
                            .textProperty()
                            .addListener((observable, oldValue, newValue) -> {
                                try {
                                    text.setText(goodToDesc(good, NumberFormat.getInstance()
                                                                              .parse(newValue)
                                                                              .doubleValue()));
                                } catch (ParseException e) {
                                    text.setText(oldValue);
                                }
                            });

            ButtonItem buttonItem = new ButtonItem(SheetCategory.SAVE_GOODS,
                                                   " ",
                                                   save.getGame()
                                                       .getLocalisationClean("TSI_CURR_MOD_BY"));

            buttonItem.getButton().setOnAction(event -> {
                Supplier<ChangePrice> supplier = () -> {
                    ChangePrice changePrice = new ChangePrice(
                            good.getName() + "_modifier_" + (this.goodsChangePrices.get(good.getName()).size() + 1),
                            0,
                            new Date());
                    this.goodsChangePrices.get(good.getName()).add(changePrice);
                    return changePrice;
                };

                TableView2Dialog<ChangePrice> dialog = new TableView2Dialog<>(this.save,
                                                                              new TableView2ChangePrice(this.goodsChangePrices
                                                                                                                .get(good.getName()),
                                                                                                        this.save),
                                                                              this.save.getGame()
                                                                                       .getLocalisationClean("TSI_CURR_MOD_BY"),
                                                                              supplier,
                                                                              good::getChangePrices);
                Optional<List<ChangePrice>> changePrices = dialog.showAndWait();

                if (changePrices.isPresent()) {
                    this.goodsChangePrices.put(good.getName(), changePrices.get());
                } else {
                    this.goodsChangePrices.put(good.getName(), good.getChangePrices());
                }

                try {
                    text.setText(goodToDesc(good, NumberFormat.getInstance()
                                                              .parse(priceSpinnerItem.getSpinner()
                                                                                     .getSpinner()
                                                                                     .getEditor()
                                                                                     .getText())
                                                              .doubleValue()));
                } catch (ParseException e) {
                }
            });

            this.goodsPricesFields.add(priceSpinnerItem);
            items.add(priceSpinnerItem);
            items.add(buttonItem);
        }

        //HRE
        if (!this.save.getHre().dismantled()) {
            this.hreEmperor = new ClearableComboBoxItem<>(SheetCategory.SAVE_HRE,
                                                          save.getGame().getLocalisation("HINT_EMPEROR_TITLE"),
                                                          new FilteredList<>(countriesAlive,
                                                                             country ->
                                                                                     country.getCapital().getContinent()
                                                                                     == this.save.getHre()
                                                                                                 .getContinent()),
                                                          this.save.getHre().getEmperor(),
                                                          new ClearableComboBox<>(new SearchableComboBox<>(),
                                                                                  () -> this.save.getHre()
                                                                                                 .getEmperor()));
            this.hreEmperor.setConverter(new CountryStringConverter());
            this.hreEmperor.setCellFactory(new CountryStringCellFactory());

            ButtonItem hreElectorsButtonItem = new ButtonItem(SheetCategory.SAVE_HRE,
                                                              null,
                                                              save.getGame()
                                                                  .getLocalisationClean("HINT_ELECTOR_TITLE"));

            this.hreElectors = FXCollections.observableArrayList(new ArrayList<>(this.save.getHre().getElectors()));
            ObservableList<Country> members = FXCollections.observableArrayList(countriesAlive.stream()
                                                                                              .filter(country -> country
                                                                                                      .getCapital()
                                                                                                      .inHre())
                                                                                              .filter(country -> !this.hreElectors
                                                                                                      .contains(country))
                                                                                              .collect(Collectors.toList()));
            hreElectorsButtonItem.getButton().setOnAction(event -> {
                ListSelectionViewDialog<Country> dialog = new ListSelectionViewDialog<>(this.save,
                                                                                        new ListSelectionViewCountry(members,
                                                                                                                     this.hreElectors
                                                                                        ),
                                                                                        this.save.getGame()
                                                                                                 .getLocalisationClean("HINT_ELECTOR_TITLE"),
                                                                                        () -> countriesAlive.stream()
                                                                                                            .filter(country -> country
                                                                                                                    .getCapital()
                                                                                                                    .inHre())
                                                                                                            .filter(country -> !this.hreElectors
                                                                                                                    .contains(country))
                                                                                                            .collect(Collectors
                                                                                                                             .toList()),
                                                                                        () -> this.save.getHre()
                                                                                                       .getElectors());
                Optional<List<Country>> newElectors = dialog.showAndWait();

                if (!newElectors.isPresent()) {
                    this.hreElectors = FXCollections.observableArrayList(new ArrayList<>(this.save.getHre()
                                                                                                  .getElectors()));
                }
            });

            this.hreInfluenceField = new ClearableSliderItem(SheetCategory.SAVE_HRE,
                                                             save.getGame().getLocalisation("HRE_INFLUENCE"),
                                                             0, 100,
                                                             this.save.getHre().getImperialInfluence(),
                                                             () -> this.save.getHre().getImperialInfluence());

            ButtonItem hreMainLineReformsButtonItem = new ButtonItem(SheetCategory.SAVE_HRE,
                                                                     null,
                                                                     save.getGame()
                                                                         .getLocalisationClean("HRE_REFORMS"));

            this.passedHreMainLineReforms = FXCollections.observableArrayList(this.save.getHre()
                                                                                       .getMainLinePassedReforms());

            this.notPassedHreMainLineReforms = FXCollections.observableArrayList(this.save.getHre()
                                                                                          .getMainLineNotPassedReforms());
            hreMainLineReformsButtonItem.getButton().setOnAction(event -> {
                ListSelectionViewImperialReform listSelectionView = new ListSelectionViewImperialReform(this.notPassedHreMainLineReforms,
                                                                                                        this.passedHreMainLineReforms);

                ObservableList<ImperialReform> tmpPassedHreMainLineReforms = FXCollections.observableArrayList(this.passedHreMainLineReforms);
                ObservableList<ImperialReform> tmpNotPassedHreMainLineReforms = FXCollections.observableArrayList(this.notPassedHreMainLineReforms);

                ListSelectionViewDialog<ImperialReform> dialog = new ListSelectionViewDialog<>(this.save,
                                                                                               listSelectionView,
                                                                                               this.save.getGame()
                                                                                                        .getLocalisationClean("HRE_REFORMS"),
                                                                                               () -> this.save
                                                                                                       .getHre()
                                                                                                       .getMainLineNotPassedReforms(),
                                                                                               () -> this.save
                                                                                                       .getHre()
                                                                                                       .getMainLinePassedReforms());

                Optional<List<ImperialReform>> newMainLineReforms = dialog.showAndWait();

                if (!newMainLineReforms.isPresent()) {
                    this.passedHreMainLineReforms.setAll(tmpPassedHreMainLineReforms);
                    this.notPassedHreMainLineReforms.setAll(tmpNotPassedHreMainLineReforms);
                }
            });

            ButtonItem hreLeftBranchReformsButtonItem = new ButtonItem(SheetCategory.SAVE_HRE,
                                                                       null,
                                                                       this.save.getGame()
                                                                                .getLocalisationClean("HRE_LEFTBRANCH"));

            this.passedHreLeftBranchReforms = FXCollections.observableArrayList(this.save.getHre()
                                                                                         .getLeftBranchPassedReforms());

            this.notPassedHreLeftBranchReforms = FXCollections.observableArrayList(this.save.getHre()
                                                                                            .getLeftBranchNotPassedReforms());
            hreLeftBranchReformsButtonItem.setOnAction(event -> {
                ListSelectionViewImperialReform listSelectionView = new ListSelectionViewImperialReform(this.notPassedHreLeftBranchReforms,
                                                                                                        this.passedHreLeftBranchReforms);

                ObservableList<ImperialReform> tmpPassedHreLeftBranchReforms = FXCollections.observableArrayList(this.passedHreLeftBranchReforms);
                ObservableList<ImperialReform> tmpNotPassedHreLeftBranchReforms = FXCollections.observableArrayList(this.notPassedHreLeftBranchReforms);

                ListSelectionViewDialog<ImperialReform> dialog = new ListSelectionViewDialog<>(this.save,
                                                                                               listSelectionView,
                                                                                               this.save.getGame()
                                                                                                        .getLocalisationClean("HRE_LEFTBRANCH"),
                                                                                               () -> this.save
                                                                                                       .getHre()
                                                                                                       .getLeftBranchNotPassedReforms(),
                                                                                               () -> this.save
                                                                                                       .getHre()
                                                                                                       .getLeftBranchPassedReforms());
                Optional<List<ImperialReform>> newLeftBranchReforms = dialog.showAndWait();

                if (newLeftBranchReforms.isPresent()) {
                    if (!newLeftBranchReforms.get().isEmpty()) {
                        this.passedHreRightBranchReforms.clear();
                        this.notPassedHreRightBranchReforms.setAll(this.save.getHre().getRightBranchReforms());
                    }
                } else {
                    this.passedHreLeftBranchReforms.setAll(tmpPassedHreLeftBranchReforms);
                    this.notPassedHreLeftBranchReforms.setAll(tmpNotPassedHreLeftBranchReforms);
                }
            });

            ButtonItem hreRightBranchReformsButtonItem = new ButtonItem(SheetCategory.SAVE_HRE,
                                                                        null,
                                                                        this.save.getGame()
                                                                                 .getLocalisationClean("HRE_RIGHTBRANCH"));

            this.passedHreRightBranchReforms = FXCollections.observableArrayList(this.save.getHre()
                                                                                          .getRightBranchPassedReforms());

            this.notPassedHreRightBranchReforms = FXCollections.observableArrayList(this.save.getHre()
                                                                                             .getRightBranchNotPassedReforms());
            hreRightBranchReformsButtonItem.setOnAction(event -> {
                ObservableList<ImperialReform> tmpPassedHreRightBranchReforms = FXCollections.observableArrayList(this.passedHreRightBranchReforms);
                ObservableList<ImperialReform> tmpNotPassedHreRightBranchReforms = FXCollections.observableArrayList(this.notPassedHreRightBranchReforms);

                ListSelectionViewDialog<ImperialReform> dialog = new ListSelectionViewDialog<>(this.save,
                                                                                               new ListSelectionViewImperialReform(this.notPassedHreRightBranchReforms,
                                                                                                                                   this.passedHreRightBranchReforms
                                                                                               ),
                                                                                               this.save.getGame()
                                                                                                        .getLocalisationClean("HRE_RIGHTBRANCH"),
                                                                                               () -> this.save
                                                                                                       .getHre()
                                                                                                       .getRightBranchNotPassedReforms(),
                                                                                               () -> this.save
                                                                                                       .getHre()
                                                                                                       .getRightBranchPassedReforms());
                Optional<List<ImperialReform>> newRightBranchReforms = dialog.showAndWait();

                if (newRightBranchReforms.isPresent()) {
                    if (!newRightBranchReforms.get().isEmpty()) {
                        this.passedHreLeftBranchReforms.clear();
                        this.notPassedHreLeftBranchReforms.setAll(this.save.getHre().getLeftBranchReforms());
                    }
                } else {
                    this.passedHreRightBranchReforms.setAll(tmpPassedHreRightBranchReforms);
                    this.notPassedHreRightBranchReforms.setAll(tmpNotPassedHreRightBranchReforms);
                }
            });

            items.add(this.hreEmperor);
            items.add(hreElectorsButtonItem);
            items.add(this.hreInfluenceField);
            items.add(hreMainLineReformsButtonItem);
            items.add(hreLeftBranchReformsButtonItem);
            items.add(hreRightBranchReformsButtonItem);
        }

        this.propertySheet.getItems().setAll(items);
    }

    public void update() {
        //GAME OPTIONS
        this.difficultyField.setValue(this.save.getGameplayOptions().getDifficulty());
        this.allowHotJoinField.setValue(this.save.getGameplayOptions().getAllowHotjoin());
        this.allowCoopField.setValue(this.save.getGameplayOptions().getAllowCoop());
        this.terraIncognitaField.setValue(this.save.getGameplayOptions().getTerraIncognita());
        this.saveEditableField.setValue(this.save.getGameplayOptions().getSaveEditable());
        this.lockedLedgerField.setValue(this.save.getGameplayOptions().getLockedLedger());
        this.limitedLedgerField.setValue(this.save.getGameplayOptions().getLimitedLedger());
        this.dynamicProvinceNamesField.setValue(this.save.getGameplayOptions().getDynamicProvinceNames());
        this.customNationDifficultyField.setValue(this.save.getGameplayOptions().getCustomNationDifficulty());
        this.addNationsToGameField.setValue(this.save.getGameplayOptions().getAddNationsToGame());
        this.showMonthlyTaxIncomeField.setValue(this.save.getGameplayOptions().getShowMonthlyTaxIncome());
        this.colorWastelandsField.setValue(this.save.getGameplayOptions().getColorWastelands());
        this.exclavesRegionNameField.setValue(this.save.getGameplayOptions().getExclavesRegionName());
        this.blockNationRuiningField.setValue(this.save.getGameplayOptions().getBlockNationRuining());
        this.unlimitedIdeaGroupsField.setValue(this.save.getGameplayOptions().getUnlimitedIdeaGroups());
        this.allowNameChangeField.setValue(this.save.getGameplayOptions().getAllowNameChange());
        this.onlyHostCanPauseField.setValue(this.save.getGameplayOptions().getOnlyHostCanPause());
        this.onlyHostAndObserversCanSaveField.setValue(this.save.getGameplayOptions().getOnlyHostAndObserversCanSave());
        this.allowTeamsField.setValue(this.save.getGameplayOptions().getAllowTeams());
        this.allowFreeTeamCreationField.setValue(this.save.getGameplayOptions().getAllowFreeTeamCreation());

        //INSTITUTIONS
        for (int i = 0; i < this.institutionOriginFields.size(); i++) {
            this.institutionAvailableFields.get(i).setValue(this.save.getInstitutions().isAvailable(i));
            this.institutionOriginFields.get(i).setValue(this.save.getInstitutions().getOrigin(i));
        }

        //GOODS
        for (int i = 0; i < this.goodsPricesFields.size(); i++) {
            this.goodsPricesFields.get(i)
                                  .getSpinner()
                                  .setValue(this.save.getChangePrices().getGood(i).getCurrentPrice());
        }

        this.goodsChangePrices.clear();
        this.save.getChangePrices()
                 .getGoods()
                 .forEach((name, changePriceGood) -> this.goodsChangePrices.put(name, new ArrayList<>(changePriceGood.getChangePrices())));

        //HRE
        if (!this.save.getHre().dismantled()) {
            this.hreEmperor.setValue(this.save.getHre().getEmperor());
            this.hreElectors = FXCollections.observableArrayList(new ArrayList<>(this.save.getHre().getElectors()));
            this.hreInfluenceField.setValue(this.save.getHre().getImperialInfluence());
            this.passedHreMainLineReforms.setAll(this.save.getHre().getMainLinePassedReforms());
            this.notPassedHreMainLineReforms.setAll(this.save.getHre().getMainLineNotPassedReforms());
            this.passedHreLeftBranchReforms.setAll(this.save.getHre().getLeftBranchPassedReforms());
            this.notPassedHreLeftBranchReforms.setAll(this.save.getHre().getLeftBranchNotPassedReforms());
            this.passedHreRightBranchReforms.setAll(this.save.getHre().getRightBranchPassedReforms());
            this.notPassedHreRightBranchReforms.setAll(this.save.getHre().getRightBranchNotPassedReforms());
        }
    }

    public void validate(ActionEvent actionEvent) {
        //GAME OPTIONS
        if (!this.save.getGameplayOptions().getDifficulty().equals(this.difficultyField.getSelectedValue())) {
            this.save.getGameplayOptions().setDifficulty(this.difficultyField.getSelectedValue());
        }

        if (this.save.getGameplayOptions().getAllowHotjoin() != this.allowHotJoinField.isSelected()) {
            this.save.getGameplayOptions().setAllowHotjoin(this.allowHotJoinField.isSelected());
        }

        if (this.save.getGameplayOptions().getAllowCoop() != this.allowCoopField.isSelected()) {
            this.save.getGameplayOptions().setAllowCoop(this.allowCoopField.isSelected());
        }

        if (this.save.getGameplayOptions().getTerraIncognita() != this.terraIncognitaField.isSelected()) {
            this.save.getGameplayOptions().setTerraIncognita(this.terraIncognitaField.isSelected());
        }

        if (this.save.getGameplayOptions().getSaveEditable() != this.saveEditableField.isSelected()) {
            this.save.getGameplayOptions().setSaveEditable(this.saveEditableField.isSelected());
        }

        if (this.save.getGameplayOptions().getLockedLedger() != this.lockedLedgerField.isSelected()) {
            this.save.getGameplayOptions().setLockedLedger(this.lockedLedgerField.isSelected());
        }

        if (this.save.getGameplayOptions().getLimitedLedger() != this.limitedLedgerField.isSelected()) {
            this.save.getGameplayOptions().setLimitedLedger(this.limitedLedgerField.isSelected());
        }

        if (this.save.getGameplayOptions().getDynamicProvinceNames() != this.dynamicProvinceNamesField.isSelected()) {
            this.save.getGameplayOptions().setDynamicProvinceNames(this.dynamicProvinceNamesField.isSelected());
        }

        if (!this.save.getGameplayOptions()
                      .getCustomNationDifficulty()
                      .equals(this.customNationDifficultyField.getSelectedValue())) {
            this.save.getGameplayOptions()
                     .setCustomNationDifficulty(this.customNationDifficultyField.getSelectedValue());
        }

        if (this.save.getGameplayOptions().getAddNationsToGame() != this.addNationsToGameField.isSelected()) {
            this.save.getGameplayOptions().setAddNationsToGame(this.addNationsToGameField.isSelected());
        }

        if (this.save.getGameplayOptions().getShowMonthlyTaxIncome() != this.showMonthlyTaxIncomeField.isSelected()) {
            this.save.getGameplayOptions().setShowMonthlyTaxIncome(this.showMonthlyTaxIncomeField.isSelected());
        }

        if (this.save.getGameplayOptions().getColorWastelands() != this.colorWastelandsField.isSelected()) {
            this.save.getGameplayOptions().setColorWastelands(this.colorWastelandsField.isSelected());
        }

        if (this.save.getGameplayOptions().getExclavesRegionName() != this.exclavesRegionNameField.isSelected()) {
            this.save.getGameplayOptions().setExclavesRegionName(this.exclavesRegionNameField.isSelected());
        }

        if (this.save.getGameplayOptions().getBlockNationRuining() != this.blockNationRuiningField.isSelected()) {
            this.save.getGameplayOptions().setBlockNationRuining(this.blockNationRuiningField.isSelected());
        }

        if (this.save.getGameplayOptions().getUnlimitedIdeaGroups() != this.unlimitedIdeaGroupsField.isSelected()) {
            this.save.getGameplayOptions().setUnlimitedIdeaGroups(this.unlimitedIdeaGroupsField.isSelected());
        }

        if (this.save.getGameplayOptions().getAllowNameChange() != this.allowNameChangeField.isSelected()) {
            this.save.getGameplayOptions().setAllowNameChange(this.allowNameChangeField.isSelected());
        }

        if (this.save.getGameplayOptions().getOnlyHostCanPause() != this.onlyHostCanPauseField.isSelected()) {
            this.save.getGameplayOptions().setOnlyHostCanPause(this.onlyHostCanPauseField.isSelected());
        }

        if (this.save.getGameplayOptions().getOnlyHostAndObserversCanSave()
            != this.onlyHostAndObserversCanSaveField.isSelected()) {
            this.save.getGameplayOptions()
                     .setOnlyHostAndObserversCanSave(this.onlyHostAndObserversCanSaveField.isSelected());
        }

        if (this.save.getGameplayOptions().getAllowTeams() != this.allowTeamsField.isSelected()) {
            this.save.getGameplayOptions().setAllowFreeTeamCreation(this.allowTeamsField.isSelected());
        }

        if (this.save.getGameplayOptions().getAllowFreeTeamCreation() != this.allowFreeTeamCreationField.isSelected()) {
            this.save.getGameplayOptions().setAllowFreeTeamCreation(this.allowFreeTeamCreationField.isSelected());
        }

        //INSTITUTIONS
        if (!this.institutionAvailableFields.isEmpty()) {
            for (int i = 0; i < this.institutionAvailableFields.size(); i++) {
                if ((this.institutionAvailableFields.get(i).isSelected() !=
                     this.save.getInstitutions().isAvailable(i))
                    || this.institutionOriginFields.get(i).getSelectedValue() != this.save.getInstitutions()
                                                                                          .getOrigin(i)) {
                    if (this.institutionAvailableFields.get(i).isSelected()) {
                        this.save.getInstitutions()
                                 .availableIn(i, this.institutionOriginFields.get(i).getSelectedValue());
                    } else {
                        this.save.getInstitutions().disable(i);
                        this.institutionOriginFields.get(i).getComboBox().reset();
                    }
                }
            }
        }

        //GOODS
        if (!this.goodsPricesFields.isEmpty()) {
            for (int i = 0; i < this.goodsPricesFields.size(); i++) {
                if (!this.goodsPricesFields.get(i)
                                           .getTrueValue()
                                           .equals(this.save.getChangePrices().getGood(i).getCurrentPrice())) {
                    this.save.getChangePrices()
                             .getGood(i)
                             .setCurrentPrice(this.goodsPricesFields.get(i).getTrueValue());
                }
            }
        }

        if (!this.goodsChangePrices.isEmpty()) {
            this.goodsChangePrices.forEach((name, changePrices) -> {
                if (!changePrices.equals(this.save.getChangePrices().getGood(name).getChangePrices())) {
                    this.save.getChangePrices().getGood(name).setChangePrices(changePrices);
                }
            });
        }

        //HRE
        if (!this.save.getHre().dismantled()) {
            if (this.save.getHre().getEmperor() != this.hreEmperor.getSelectedValue()) {
                this.save.getHre().setEmperor(this.hreEmperor.getSelectedValue());
            }

            if (!this.save.getHre().getElectors().equals(this.hreElectors)) {
                this.save.getHre().setElectors(this.hreElectors);
            }

            if (!this.save.getHre().getImperialInfluence().equals(this.hreInfluenceField.getDoubleValue())) {
                this.save.getHre().setImperialInfluence(this.hreInfluenceField.getDoubleValue());
            }

            if (!this.save.getHre().getMainLinePassedReforms().equals(this.passedHreMainLineReforms)
                || !this.save.getHre().getLeftBranchPassedReforms().equals(this.passedHreLeftBranchReforms)
                || !this.save.getHre().getRightBranchPassedReforms().equals(this.passedHreRightBranchReforms)) {
                this.save.getHre().setPassedReforms(
                        Stream.of(this.passedHreMainLineReforms, this.passedHreLeftBranchReforms, this.passedHreRightBranchReforms)
                              .flatMap(Collection::stream)
                              .collect(Collectors.toList()));
            }
        }
    }

    private String goodToDesc(ChangePriceGood good, Double price) {
        double modifiersSum = this.goodsChangePrices.get(good.getName())
                                                    .stream()
                                                    .mapToDouble(ChangePrice::getValue)
                                                    .sum();
        BigDecimal basePrice = BigDecimal.valueOf(price)
                                         .multiply(BigDecimal.valueOf(100))
                                         .divide(BigDecimal.valueOf(100)
                                                           .add(BigDecimal.valueOf(modifiersSum)), RoundingMode.HALF_EVEN)
                                         .setScale(3, RoundingMode.HALF_EVEN);
        return "("
               + basePrice.toPlainString()
               + (modifiersSum < 0 ? " " : " +")
               + modifiersSum
               + "%)";
    }

    public Save getSave() {
        return save;
    }

    public ValidationSupport getValidationSupport() {
        return validationSupport;
    }

    public PropertySheet getPropertySheet() {
        return propertySheet;
    }
}
