package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.eu4parser.model.game.GoldenBull;
import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.SaveReligion;
import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4saveeditor.controller.EditorController;
import com.osallek.eu4saveeditor.controller.control.ClearableComboBox;
import com.osallek.eu4saveeditor.controller.control.ClearableSpinnerDouble;
import com.osallek.eu4saveeditor.controller.converter.CountryStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.CountryStringConverter;
import com.osallek.eu4saveeditor.controller.converter.GoldenBullStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.GoldenBullStringConverter;
import com.osallek.eu4saveeditor.controller.pane.CustomPropertySheetSkin;
import com.osallek.eu4saveeditor.controller.propertyeditor.CustomPropertyEditorFactory;
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
import org.controlsfx.control.PropertySheet;
import org.controlsfx.control.SearchableComboBox;
import org.controlsfx.validation.ValidationSupport;
import org.controlsfx.validation.decoration.CompoundValidationDecoration;
import org.controlsfx.validation.decoration.StyleClassValidationDecoration;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class ReligionPropertySheet extends VBox {

    private final Save save;

    private final SaveReligion religion;

    private final PropertySheet propertySheet;

    private final ValidationSupport validationSupport;

    private CheckBoxItem enableField;

    private ClearableComboBoxItem<Country> defenderOfFaithField;

    private ClearableComboBoxItem<Country> papalControllerField;

    private ClearableComboBoxItem<Country> crusadeTargetField;

    private ClearableSliderItem reformDesireField;

    private ClearableSpinnerItem<Double> curiaTreasuryField;

    private ClearableComboBoxItem<GoldenBull> goldenBullField;

    private CustomPropertySheetSkin propertySheetSkin;

    public ReligionPropertySheet(Save save, SaveReligion religion, ObservableList<Country> countriesAlive) {
        this.save = save;
        this.religion = religion;
        this.propertySheet = new PropertySheet();
        this.propertySheet.setPropertyEditorFactory(new CustomPropertyEditorFactory());
        this.propertySheet.setMode(PropertySheet.Mode.CATEGORY);
        this.propertySheet.setModeSwitcherVisible(false);
        this.propertySheet.setSearchBoxVisible(false);

        List<PropertySheet.Item> items = new ArrayList<>();

        this.propertySheetSkin = new CustomPropertySheetSkin(this.propertySheet);
        this.propertySheet.setSkin(this.propertySheetSkin);

        this.validationSupport = new ValidationSupport();
        this.validationSupport.setValidationDecorator(
                new CompoundValidationDecoration(new CustomGraphicValidationDecoration(),
                                                 new StyleClassValidationDecoration("validation-error", null)));

        if (this.religion.hasDate() && this.religion.getEnable() == null) {
            this.enableField = new CheckBoxItem(this.religion.getLocalizedName(),
                                                this.save.getGame().getLocalisation("ENABLE"),
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

        FilteredList<Country> possibleDefenders = countries.filtered(country -> country.getOverlord() == null && !EditorController.dummyCountry.equals(country));
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
                                                                                      && country.getCapital().getContinent() == 0
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
                                                                                            this.religion.getPapacy()
                                                                                                         .getCuriaTreasury(),
                                                                                            100,
                                                                                            () -> this.religion.getPapacy()
                                                                                                               .getCuriaTreasury()));
            items.add(this.curiaTreasuryField);

            ObservableList<GoldenBull> goldenBulls = FXCollections.observableArrayList(this.save.getGame()
                                                                                                .getGoldenBulls());
            goldenBulls.add(0, new GoldenBull((String) null));

            this.goldenBullField = new ClearableComboBoxItem<>(this.religion.getLocalizedName(),
                                                               this.save.getGame()
                                                                        .getLocalisation("GOLDEN_BULL_PICKED_TITLE"),
                                                               goldenBulls,
                                                               this.religion.getPapacy().getGoldenBull(),
                                                               new ClearableComboBox<>(new SearchableComboBox<>(),
                                                                                       () -> this.religion.getPapacy()
                                                                                                          .getGoldenBull()));
            this.goldenBullField.setConverter(new GoldenBullStringConverter());
            this.goldenBullField.setCellFactory(new GoldenBullStringCellFactory());
            items.add(this.goldenBullField);

            this.religion.getPapacy().getGoldenBull();
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
    }

    public SaveReligion getReligion() {
        return religion;
    }

    public ValidationSupport getValidationSupport() {
        return validationSupport;
    }

    public PropertySheet getPropertySheet() {
        return propertySheet;
    }
}
