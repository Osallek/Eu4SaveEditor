package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.gameplayoptions.CustomNationDifficulty;
import com.osallek.eu4parser.model.save.gameplayoptions.Difficulty;
import com.osallek.eu4saveeditor.controller.control.ClearableComboBox;
import com.osallek.eu4saveeditor.controller.control.RequiredComboBox;
import com.osallek.eu4saveeditor.controller.converter.CustomNationDifficultyStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.CustomNationDifficultyStringConverter;
import com.osallek.eu4saveeditor.controller.converter.DifficultyStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.DifficultyStringConverter;
import com.osallek.eu4saveeditor.controller.pane.CustomPropertySheetSkin;
import com.osallek.eu4saveeditor.controller.propertyeditor.CustomPropertyEditorFactory;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.CheckBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableComboBoxItem;
import com.osallek.eu4saveeditor.controller.validator.CustomGraphicValidationDecoration;
import javafx.collections.FXCollections;
import javafx.event.ActionEvent;
import javafx.scene.layout.VBox;
import org.controlsfx.control.PropertySheet;
import org.controlsfx.validation.ValidationSupport;
import org.controlsfx.validation.decoration.CompoundValidationDecoration;
import org.controlsfx.validation.decoration.StyleClassValidationDecoration;

import java.util.Comparator;

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

    private CustomPropertySheetSkin propertySheetSkin;

    public SavePropertySheet(Save save) {
        this.save = save;
        this.propertySheet = new PropertySheet();
        this.propertySheet.setPropertyEditorFactory(new CustomPropertyEditorFactory());
        this.propertySheet.setMode(PropertySheet.Mode.CATEGORY);
        this.propertySheet.setCategoryComparator(Comparator.comparing(SheetCategory::getByLocale));
        this.propertySheet.setModeSwitcherVisible(false);
        this.propertySheet.setSearchBoxVisible(false);

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
        this.propertySheet.getItems().add(this.difficultyField);
        this.propertySheet.getItems().add(this.terraIncognitaField);
        this.propertySheet.getItems().add(this.dynamicProvinceNamesField);
        this.propertySheet.getItems().add(this.showMonthlyTaxIncomeField);
        this.propertySheet.getItems().add(this.colorWastelandsField);
        this.propertySheet.getItems().add(this.exclavesRegionNameField);
        this.propertySheet.getItems().add(this.unlimitedIdeaGroupsField);
        this.propertySheet.getItems().add(this.allowHotJoinField);
        this.propertySheet.getItems().add(this.allowCoopField);
        this.propertySheet.getItems().add(this.onlyHostAndObserversCanSaveField);
        this.propertySheet.getItems().add(this.onlyHostCanPauseField);
        this.propertySheet.getItems().add(this.saveEditableField);
        this.propertySheet.getItems().add(this.lockedLedgerField);
        this.propertySheet.getItems().add(this.limitedLedgerField);
        this.propertySheet.getItems().add(this.allowTeamsField);
        this.propertySheet.getItems().add(this.allowFreeTeamCreationField);
        this.propertySheet.getItems().add(this.blockNationRuiningField);
        this.propertySheet.getItems().add(this.allowNameChangeField);
        this.propertySheet.getItems().add(this.customNationDifficultyField);
        this.propertySheet.getItems().add(this.addNationsToGameField);
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
    }

    public void validate(ActionEvent actionEvent) {
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
