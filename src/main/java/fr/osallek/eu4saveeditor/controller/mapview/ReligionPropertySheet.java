package fr.osallek.eu4saveeditor.controller.mapview;

import fr.osallek.eu4parser.model.game.localisation.Eu4Language;
import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4parser.model.save.SaveReligion;
import fr.osallek.eu4parser.model.save.country.SaveCountry;
import fr.osallek.eu4parser.model.save.province.SaveProvince;
import fr.osallek.eu4saveeditor.controller.EditorController;
import fr.osallek.eu4saveeditor.controller.control.ClearableComboBox;
import fr.osallek.eu4saveeditor.controller.control.ClearableSpinnerDouble;
import fr.osallek.eu4saveeditor.controller.control.TableView2ReformationCenter;
import fr.osallek.eu4saveeditor.controller.converter.CountryStringCellFactory;
import fr.osallek.eu4saveeditor.controller.converter.CountryStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.GoldenBullStringCellFactory;
import fr.osallek.eu4saveeditor.controller.converter.GoldenBullStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.SaveReligionStringConverter;
import fr.osallek.eu4saveeditor.controller.object.GoldenBull;
import fr.osallek.eu4saveeditor.controller.object.ReformationCenter;
import fr.osallek.eu4saveeditor.controller.pane.CustomPropertySheet;
import fr.osallek.eu4saveeditor.controller.pane.CustomPropertySheetSkin;
import fr.osallek.eu4saveeditor.controller.pane.TableViewDialog;
import fr.osallek.eu4saveeditor.controller.propertyeditor.CustomPropertyEditorFactory;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ButtonItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.CheckBoxItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableComboBoxItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSliderItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSpinnerItem;
import fr.osallek.eu4saveeditor.controller.validator.CustomGraphicValidationDecoration;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.scene.layout.VBox;
import org.apache.commons.collections4.CollectionUtils;
import org.controlsfx.control.SearchableComboBox;
import org.controlsfx.validation.ValidationSupport;
import org.controlsfx.validation.decoration.CompoundValidationDecoration;
import org.controlsfx.validation.decoration.StyleClassValidationDecoration;

public class ReligionPropertySheet extends VBox {

    private final Save save;

    private final SaveReligion religion;

    private final CustomPropertySheet propertySheet;

    private final ValidationSupport validationSupport;

    private CheckBoxItem enableField;

    private ClearableComboBoxItem<SaveCountry> defenderOfFaithField;

    private ClearableComboBoxItem<SaveCountry> papalControllerField;

    private ClearableComboBoxItem<SaveCountry> crusadeTargetField;

    private ClearableSliderItem reformDesireField;

    private ClearableSpinnerItem<Double> curiaTreasuryField;

    private ClearableComboBoxItem<GoldenBull> goldenBullField;

    private ObservableList<ReformationCenter> reformationCenters;

    public ReligionPropertySheet(Save save, SaveReligion religion, ObservableList<SaveCountry> countriesAlive, ObservableList<SaveProvince> provinces) {
        String category = SaveReligionStringConverter.INSTANCE.toString(religion);
        this.save = save;
        this.religion = religion;
        this.propertySheet = new CustomPropertySheet();
        this.propertySheet.setPropertyEditorFactory(new CustomPropertyEditorFactory());
        this.propertySheet.setMode(CustomPropertySheet.Mode.CATEGORY);
        this.propertySheet.setModeSwitcherVisible(false);
        this.propertySheet.setSearchBoxVisible(false);

        List<CustomPropertySheet.Item> items = new ArrayList<>();

        CustomPropertySheetSkin propertySheetSkin = new CustomPropertySheetSkin(this.propertySheet);
        this.propertySheet.setSkin(propertySheetSkin);

        this.validationSupport = new ValidationSupport();
        this.validationSupport.setValidationDecorator(
                new CompoundValidationDecoration(new CustomGraphicValidationDecoration(), new StyleClassValidationDecoration("validation-error", null)));

        if (this.religion.hasDate() && this.religion.getEnable() == null) {
            this.enableField = new CheckBoxItem(category,
                                                this.save.getGame().getLocalisationClean("ENABLE", Eu4Language.getDefault()),
                                                this.religion.getEnable() != null);
            items.add(this.enableField);
        }

        ObservableList<SaveCountry> countries = FXCollections.observableArrayList(countriesAlive.stream()
                                                                                                .filter(country -> religion.equals(country.getReligion()))
                                                                                                .collect(Collectors.toList()));
        countries.add(0, EditorController.dummyCountry);

        ObservableList<SaveCountry> otherReligionGroupCountries =
                FXCollections.observableArrayList(countriesAlive.stream()
                                                                .filter(country -> country.getReligion() == null
                                                                                   || !country.getReligion()
                                                                                              .getReligionGroup()
                                                                                              .equals(religion.getReligionGroup()))
                                                                .collect(Collectors.toList()));
        otherReligionGroupCountries.add(0, EditorController.dummyCountry);

        FilteredList<SaveCountry> possibleDefenders = countries.filtered(c -> c.getOverlord() == null && !EditorController.dummyCountry.equals(c));
        if (this.religion.hasDefenderOfFaith() && !possibleDefenders.isEmpty()) {
            this.defenderOfFaithField = new ClearableComboBoxItem<>(category,
                                                                    this.save.getGame().getLocalisationClean("defender_of_faith", Eu4Language.getDefault()),
                                                                    possibleDefenders,
                                                                    this.religion.getDefender() == null ? EditorController.dummyCountry :
                                                                    this.religion.getDefender(),
                                                                    new ClearableComboBox<>(new SearchableComboBox<>(), this.religion::getDefender));
            this.defenderOfFaithField.setConverter(new CountryStringConverter());
            this.defenderOfFaithField.setCellFactory(new CountryStringCellFactory());
            items.add(this.defenderOfFaithField);
        }

        if (this.religion.hasPapacy() && Boolean.TRUE.equals(this.religion.getPapacy().getPapacyActive())) {
            FilteredList<SaveCountry> possibleControllers = countries.filtered(country -> country.getCapital() != null
                                                                                          && country.getCapital().getContinent() != null
                                                                                          && !EditorController.dummyCountry.equals(country));

            if (!possibleControllers.isEmpty()) {
                this.papalControllerField = new ClearableComboBoxItem<>(category,
                                                                        this.save.getGame()
                                                                                 .getLocalisationClean("HINT_PAPALCONTROLLER_TITLE", Eu4Language.getDefault()),
                                                                        possibleControllers,
                                                                        this.religion.getPapacy().getController(),
                                                                        new ClearableComboBox<>(new SearchableComboBox<>(),
                                                                                                () -> this.religion.getPapacy().getController()));
                this.papalControllerField.setConverter(new CountryStringConverter());
                this.papalControllerField.setCellFactory(new CountryStringCellFactory());
                items.add(this.papalControllerField);
            }

            if (otherReligionGroupCountries.contains(EditorController.dummyCountry) ?
                otherReligionGroupCountries.size() > 1 : !otherReligionGroupCountries.isEmpty()) {

                this.crusadeTargetField = new ClearableComboBoxItem<>(category,
                                                                      this.save.getGame().getLocalisationClean("IS_CRUSADE_TARGET", Eu4Language.getDefault()),
                                                                      otherReligionGroupCountries,
                                                                      this.religion.getPapacy().getCrusadeTarget(),
                                                                      new ClearableComboBox<>(new SearchableComboBox<>(),
                                                                                              () -> this.religion.getPapacy().getCrusadeTarget()));
                this.crusadeTargetField.setConverter(new CountryStringConverter());
                this.crusadeTargetField.setCellFactory(new CountryStringCellFactory());
                items.add(this.crusadeTargetField);
            }

            this.reformDesireField = new ClearableSliderItem(category,
                                                             save.getGame().getLocalisationClean("HINT_REFORMDESIRE_TITLE", Eu4Language.getDefault()),
                                                             0, 200,
                                                             this.religion.getPapacy().getReformDesire(),
                                                             () -> this.religion.getPapacy().getReformDesire());
            items.add(this.reformDesireField);

            this.curiaTreasuryField = new ClearableSpinnerItem<>(category,
                                                                 save.getGame()
                                                                     .getLocalisationCleanNoPunctuation("CURIA_TREASURY", Eu4Language.getDefault()),
                                                                 new ClearableSpinnerDouble(0, 100000,
                                                                                            this.religion.getPapacy().getCuriaTreasury(),
                                                                                            100,
                                                                                            () -> this.religion.getPapacy().getCuriaTreasury()));
            items.add(this.curiaTreasuryField);

            ObservableList<GoldenBull> goldenBulls = FXCollections.observableArrayList(this.save.getGame()
                                                                                                .getGoldenBulls()
                                                                                                .stream()
                                                                                                .map(g -> new GoldenBull(g, this.save))
                                                                                                .toList());
            goldenBulls.add(0, new GoldenBull(this.save));

            this.goldenBullField = new ClearableComboBoxItem<>(category,
                                                               this.save.getGame()
                                                                        .getLocalisationClean("GOLDEN_BULL_PICKED_TITLE", Eu4Language.getDefault()),
                                                               goldenBulls,
                                                               new GoldenBull(this.religion.getPapacy().getGoldenBull(), this.save),
                                                               new ClearableComboBox<>(new SearchableComboBox<>(),
                                                                                       () -> new GoldenBull(this.religion.getPapacy().getGoldenBull(),
                                                                                                            this.save)));
            this.goldenBullField.setConverter(new GoldenBullStringConverter());
            this.goldenBullField.setCellFactory(new GoldenBullStringCellFactory());
            items.add(this.goldenBullField);
        }

        if (CollectionUtils.isNotEmpty(provinces) && this.religion.getGameReligion() != null
            && CollectionUtils.isNotEmpty(this.religion.getGameReligion().getAllowedCenterConversion())) {
            this.reformationCenters = FXCollections.observableArrayList();
            this.reformationCenters.setAll(this.religion.getReformationCenters().stream().map(ReformationCenter::new).collect(Collectors.toList()));
            ButtonItem reformationCentersButton = new ButtonItem(category, null,
                                                                 save.getGame()
                                                                     .getLocalisationClean("protestant_center_of_reformation", Eu4Language.getDefault()),
                                                                 items.isEmpty() ? 1 : 2);
            reformationCentersButton.getButton().setOnAction(event -> {
                TableView2ReformationCenter tableView2 = new TableView2ReformationCenter(this.save, this.reformationCenters, provinces);
                TableViewDialog<ReformationCenter> dialog = new TableViewDialog<>(this.save,
                                                                                  tableView2,
                                                                                  this.save.getGame()
                                                                                           .getLocalisationClean("protestant_center_of_reformation", Eu4Language.getDefault()),
                                                                                  list -> new ReformationCenter(this.religion,
                                                                                                                provinces.stream()
                                                                                                                         .filter(Predicate.not(
                                                                                                                                 SaveProvince::centerOfReligion))
                                                                                                                         .findFirst()
                                                                                                                         .get()),
                                                                                  () -> this.reformationCenters);
                dialog.setDisableAddProperty(tableView2.disableAddPropertyProperty());
                Optional<List<ReformationCenter>> modifierList = dialog.showAndWait();

                modifierList.ifPresent(this.reformationCenters::setAll);
            });
            items.add(reformationCentersButton);
        }

        this.propertySheet.getItems().setAll(items);
    }

    public void update() {
        if (this.enableField != null) {
            this.enableField.setValue(this.religion.getEnable() != null);
        }

        if (this.defenderOfFaithField != null) {
            this.defenderOfFaithField.setValue(this.religion.getDefender() == null ? EditorController.dummyCountry : this.religion.getDefender());
        }

        if (this.papalControllerField != null) {
            this.papalControllerField.setValue(this.religion.getPapacy().getController());
        }

        if (this.crusadeTargetField != null) {
            this.crusadeTargetField.setValue(this.religion.getPapacy().getCrusadeTarget());
        }

        if (this.reformDesireField != null) {
            this.reformDesireField.setValue(this.religion.getPapacy().getReformDesire());
        }

        if (this.curiaTreasuryField != null) {
            this.curiaTreasuryField.setValue(this.religion.getPapacy().getCuriaTreasury());
        }

        if (this.goldenBullField != null) {
            this.goldenBullField.setValue(new GoldenBull(this.religion.getPapacy().getGoldenBull(), this.save));
        }

        if (this.reformationCenters != null) {
            this.reformationCenters.setAll(this.religion.getReformationCenters().stream().map(ReformationCenter::new).collect(Collectors.toList()));
        }
    }

    public void validate() {
        if (this.enableField != null) {
            if (this.enableField.isSelected()) {
                this.religion.setEnable(this.save.getDate());
            }
        }

        if (this.defenderOfFaithField != null) {
            if (!Objects.equals(this.religion.getDefender(), this.defenderOfFaithField.getSelectedValue())) {
                this.religion.setDefender(this.defenderOfFaithField.getSelectedValue(), this.save.getDate());
            }
        }

        if (this.papalControllerField != null) {
            if (!Objects.equals(this.religion.getPapacy().getController(), this.papalControllerField.getSelectedValue())) {
                this.religion.getPapacy().setController(this.papalControllerField.getSelectedValue());
            }
        }

        if (this.crusadeTargetField != null) {
            if (!Objects.equals(this.religion.getPapacy().getCrusadeTarget(), this.crusadeTargetField.getSelectedValue())) {
                this.religion.getPapacy().setCrusadeTarget(this.crusadeTargetField.getSelectedValue());
            }
        }

        if (this.reformDesireField != null) {
            if (!Objects.equals(this.religion.getPapacy().getReformDesire(), this.reformDesireField.getDoubleValue())) {
                this.religion.getPapacy().setReformDesire(this.reformDesireField.getDoubleValue());
            }
        }

        if (this.curiaTreasuryField != null) {
            if (!Objects.equals(this.religion.getPapacy().getCuriaTreasury(), this.curiaTreasuryField.getTrueValue())) {
                this.religion.getPapacy().setCuriaTreasury(this.curiaTreasuryField.getTrueValue());
            }
        }

        if (this.goldenBullField != null) {
            if ((this.religion.getPapacy().getGoldenBull() == null && this.goldenBullField.getSelectedValue().getGoldenBull() != null)
                || !Objects.equals(this.religion.getPapacy().getGoldenBull().getName(), this.goldenBullField.getSelectedValue().getGoldenBull())) {
                this.religion.getPapacy().setGoldenBull(this.save.getGame().getGoldenBull(this.goldenBullField.getSelectedValue().getGoldenBull()));
            }
        }

        if (this.reformationCenters != null) {
            if (this.religion.getReformationCenters().size() != this.reformationCenters.size()
                || this.reformationCenters.stream().anyMatch(ReformationCenter::isChanged)) {
                this.religion.getReformationCenters()
                             .forEach(reformationCenter -> this.reformationCenters.stream()
                                                                                  .filter(r -> reformationCenter.getProvince().equals(r.getProvince()))
                                                                                  .findFirst()
                                                                                  .ifPresentOrElse(r -> this.reformationCenters.remove(r),
                                                                                                   () -> this.religion.removeReformationCenter(
                                                                                                           reformationCenter)));
                this.reformationCenters.forEach(reformationCenter -> this.religion.addReformationCenter(reformationCenter.getProvince()));
            }
        }
    }

    public SaveReligion getReligion() {
        return religion;
    }

    public ValidationSupport getValidationSupport() {
        return validationSupport;
    }

    public CustomPropertySheet getPropertySheet() {
        return propertySheet;
    }
}
