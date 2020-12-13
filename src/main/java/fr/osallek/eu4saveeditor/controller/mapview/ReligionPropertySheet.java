package fr.osallek.eu4saveeditor.controller.mapview;

import fr.osallek.eu4parser.model.game.GoldenBull;
import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4parser.model.save.SaveReligion;
import fr.osallek.eu4parser.model.save.country.Country;
import fr.osallek.eu4parser.model.save.province.SaveProvince;
import fr.osallek.eu4saveeditor.controller.EditorController;
import fr.osallek.eu4saveeditor.controller.control.ClearableComboBox;
import fr.osallek.eu4saveeditor.controller.control.ClearableSpinnerDouble;
import fr.osallek.eu4saveeditor.controller.control.TableView2ReformationCenter;
import fr.osallek.eu4saveeditor.controller.converter.CountryStringCellFactory;
import fr.osallek.eu4saveeditor.controller.converter.CountryStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.GoldenBullStringCellFactory;
import fr.osallek.eu4saveeditor.controller.converter.GoldenBullStringConverter;
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
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.event.ActionEvent;
import javafx.scene.layout.VBox;
import org.apache.commons.collections4.CollectionUtils;
import org.controlsfx.control.SearchableComboBox;
import org.controlsfx.validation.ValidationSupport;
import org.controlsfx.validation.decoration.CompoundValidationDecoration;
import org.controlsfx.validation.decoration.StyleClassValidationDecoration;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class ReligionPropertySheet extends VBox {

    private final Save save;

    private final SaveReligion religion;

    private final CustomPropertySheet propertySheet;

    private final ValidationSupport validationSupport;

    private CheckBoxItem enableField;

    private ClearableComboBoxItem<Country> defenderOfFaithField;

    private ClearableComboBoxItem<Country> papalControllerField;

    private ClearableComboBoxItem<Country> crusadeTargetField;

    private ClearableSliderItem reformDesireField;

    private ClearableSpinnerItem<Double> curiaTreasuryField;

    private ClearableComboBoxItem<GoldenBull> goldenBullField;

    private ButtonItem reformationCentersButton;

    private ObservableList<ReformationCenter> reformationCenters;

    private CustomPropertySheetSkin propertySheetSkin;

    public ReligionPropertySheet(Save save, SaveReligion religion, ObservableList<Country> countriesAlive, ObservableList<SaveProvince> provinces) {
        this.save = save;
        this.religion = religion;
        this.propertySheet = new CustomPropertySheet();
        this.propertySheet.setPropertyEditorFactory(new CustomPropertyEditorFactory());
        this.propertySheet.setMode(CustomPropertySheet.Mode.CATEGORY);
        this.propertySheet.setModeSwitcherVisible(false);
        this.propertySheet.setSearchBoxVisible(false);

        List<CustomPropertySheet.Item> items = new ArrayList<>();

        this.propertySheetSkin = new CustomPropertySheetSkin(this.propertySheet);
        this.propertySheet.setSkin(this.propertySheetSkin);

        this.validationSupport = new ValidationSupport();
        this.validationSupport.setValidationDecorator(
                new CompoundValidationDecoration(new CustomGraphicValidationDecoration(), new StyleClassValidationDecoration("validation-error", null)));

        if (this.religion.hasDate() && this.religion.getEnable() == null) {
            this.enableField = new CheckBoxItem(this.religion.getLocalizedName(), this.save.getGame().getLocalisation("ENABLE"),
                                                this.religion.getEnable() != null);
            items.add(this.enableField);
        }

        ObservableList<Country> countries = FXCollections.observableArrayList(countriesAlive.stream()
                                                                                            .filter(country -> religion.equals(
                                                                                                    country.getReligion()))
                                                                                            .collect(
                                                                                                    Collectors.toList()));
        ObservableList<Country> otherReligionGroupCountries =
                FXCollections.observableArrayList(countriesAlive.stream()
                                                                .filter(country -> !country.getReligion()
                                                                                           .getReligionGroup()
                                                                                           .equals(religion.getReligionGroup()))
                                                                .collect(Collectors.toList()));

        FilteredList<Country> possibleDefenders = countries.filtered(
                country -> country.getOverlord() == null && !EditorController.dummyCountry.equals(country));
        if (this.religion.hasDefenderOfFaith() && !possibleDefenders.isEmpty()) {
            if (!countries.contains(EditorController.dummyCountry)) {
                countries.add(0, EditorController.dummyCountry);
            }

            this.defenderOfFaithField = new ClearableComboBoxItem<>(this.religion.getLocalizedName(),
                                                                    this.save.getGame()
                                                                             .getLocalisation("defender_of_faith"),
                                                                    possibleDefenders,
                                                                    this.religion.getDefender() == null ?
                                                                    EditorController.dummyCountry :
                                                                    this.religion.getDefender(),
                                                                    new ClearableComboBox<>(new SearchableComboBox<>(),
                                                                                            this.religion::getDefender));
            this.defenderOfFaithField.setConverter(new CountryStringConverter());
            this.defenderOfFaithField.setCellFactory(new CountryStringCellFactory());
            items.add(this.defenderOfFaithField);
        }

        if (this.religion.hasPapacy() && Boolean.TRUE.equals(this.religion.getPapacy().getPapacyActive())) {
            FilteredList<Country> possibleControllers = countries.filtered(country -> country.getCapital() != null
                                                                                      && country.getCapital().getContinent() != null
                                                                                      && !EditorController.dummyCountry.equals(country));

            if (!possibleControllers.isEmpty()) {
                if (!countries.contains(EditorController.dummyCountry)) {
                    countries.add(0, EditorController.dummyCountry);
                }

                this.papalControllerField = new ClearableComboBoxItem<>(this.religion.getLocalizedName(),
                                                                        this.save.getGame().getLocalisation("HINT_PAPALCONTROLLER_TITLE"),
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
                if (!otherReligionGroupCountries.contains(EditorController.dummyCountry)) {
                    otherReligionGroupCountries.add(0, EditorController.dummyCountry);
                }

                this.crusadeTargetField = new ClearableComboBoxItem<>(this.religion.getLocalizedName(),
                                                                      this.save.getGame()
                                                                               .getLocalisation(
                                                                                       "IS_CRUSADE_TARGET"),
                                                                      otherReligionGroupCountries,
                                                                      this.religion.getPapacy().getCrusadeTarget(),
                                                                      new ClearableComboBox<>(
                                                                              new SearchableComboBox<>(),
                                                                              () -> this.religion.getPapacy()
                                                                                                 .getCrusadeTarget()));
                this.crusadeTargetField.setConverter(new CountryStringConverter());
                this.crusadeTargetField.setCellFactory(new CountryStringCellFactory());
                items.add(this.crusadeTargetField);
            }

            this.reformDesireField = new ClearableSliderItem(this.religion.getLocalizedName(),
                                                             save.getGame().getLocalisation("HINT_REFORMDESIRE_TITLE"),
                                                             0, 200,
                                                             this.religion.getPapacy().getReformDesire(),
                                                             () -> this.religion.getPapacy().getReformDesire());
            items.add(this.reformDesireField);

            this.curiaTreasuryField = new ClearableSpinnerItem<>(this.religion.getLocalizedName(),
                                                                 save.getGame()
                                                                     .getLocalisationCleanNoPunctuation("CURIA_TREASURY"),
                                                                 new ClearableSpinnerDouble(0, 100000,
                                                                                            this.religion.getPapacy().getCuriaTreasury(),
                                                                                            100,
                                                                                            () -> this.religion.getPapacy().getCuriaTreasury()));
            items.add(this.curiaTreasuryField);

            ObservableList<GoldenBull> goldenBulls = FXCollections.observableArrayList(this.save.getGame().getGoldenBulls());
            goldenBulls.add(0, new GoldenBull((String) null));

            this.goldenBullField = new ClearableComboBoxItem<>(this.religion.getLocalizedName(),
                                                               this.save.getGame()
                                                                        .getLocalisation("GOLDEN_BULL_PICKED_TITLE"),
                                                               goldenBulls,
                                                               this.religion.getPapacy().getGoldenBull(),
                                                               new ClearableComboBox<>(new SearchableComboBox<>(),
                                                                                       () -> this.religion.getPapacy().getGoldenBull()));
            this.goldenBullField.setConverter(new GoldenBullStringConverter());
            this.goldenBullField.setCellFactory(new GoldenBullStringCellFactory());
            items.add(this.goldenBullField);

            this.religion.getPapacy().getGoldenBull();
        }

        if (CollectionUtils.isNotEmpty(provinces) && this.religion.getGameReligion() != null
            && CollectionUtils.isNotEmpty(this.religion.getGameReligion().getAllowedCenterConversion())) {
            this.reformationCenters = FXCollections.observableArrayList();
            this.reformationCenters.setAll(this.religion.getReformationCenters().stream().map(ReformationCenter::new).collect(Collectors.toList()));
            this.reformationCentersButton = new ButtonItem(this.religion.getLocalizedName(), null,
                                                           save.getGame().getLocalisationClean("protestant_center_of_reformation"),
                                                           items.isEmpty() ? 1 : 2);
            this.reformationCentersButton.getButton().setOnAction(event -> {
                TableView2ReformationCenter tableView2 = new TableView2ReformationCenter(this.save, this.reformationCenters, provinces);
                TableViewDialog<ReformationCenter> dialog = new TableViewDialog<>(this.save,
                                                                                  tableView2,
                                                                                  this.save.getGame().getLocalisationClean("protestant_center_of_reformation"),
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
            items.add(this.reformationCentersButton);
        }

        this.propertySheet.getItems().setAll(items);
    }

    public void update() {
        if (this.enableField != null) {
            this.enableField.setValue(this.religion.getEnable() != null);
        }

        if (this.defenderOfFaithField != null) {
            this.defenderOfFaithField.setValue(this.religion.getDefender() == null ?
                                               EditorController.dummyCountry : this.religion.getDefender());
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
            this.goldenBullField.setValue(this.religion.getPapacy().getGoldenBull());
        }

        if (this.reformationCenters != null) {
            this.reformationCenters.setAll(this.religion.getReformationCenters().stream().map(ReformationCenter::new).collect(Collectors.toList()));
        }
    }

    public void validate(ActionEvent actionEvent) {
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
            if (!Objects.equals(this.religion.getPapacy().getController(),
                                this.papalControllerField.getSelectedValue())) {
                this.religion.getPapacy().setController(this.papalControllerField.getSelectedValue());
            }
        }

        if (this.crusadeTargetField != null) {
            if (!Objects.equals(this.religion.getPapacy().getCrusadeTarget(),
                                this.crusadeTargetField.getSelectedValue())) {
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
            if (!Objects.equals(this.religion.getPapacy().getGoldenBull(), this.goldenBullField.getSelectedValue())) {
                this.religion.getPapacy().setGoldenBull(this.goldenBullField.getSelectedValue());
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
