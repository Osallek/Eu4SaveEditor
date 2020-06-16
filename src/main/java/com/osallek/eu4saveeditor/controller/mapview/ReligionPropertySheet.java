package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.SaveReligion;
import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4saveeditor.controller.EditorController;
import com.osallek.eu4saveeditor.controller.control.ClearableComboBox;
import com.osallek.eu4saveeditor.controller.control.ClearableDatePicker;
import com.osallek.eu4saveeditor.controller.converter.CountryStringCellFactory;
import com.osallek.eu4saveeditor.controller.converter.CountryStringConverter;
import com.osallek.eu4saveeditor.controller.pane.CustomPropertySheetSkin;
import com.osallek.eu4saveeditor.controller.propertyeditor.CustomPropertyEditorFactory;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableDatePickerItem;
import com.osallek.eu4saveeditor.controller.validator.CustomGraphicValidationDecoration;
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

public class ReligionPropertySheet extends VBox {

    private final Save save;

    private final SaveReligion religion;

    private final PropertySheet propertySheet;

    private final ValidationSupport validationSupport;

    private ClearableDatePickerItem enableField;

    private ClearableComboBoxItem<Country> defenderOfFaithField;

    private CustomPropertySheetSkin propertySheetSkin;

    public ReligionPropertySheet(Save save, SaveReligion religion, ObservableList<Country> countries) {
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
            this.enableField = new ClearableDatePickerItem(this.religion.getLocalizedName(), "Date",
                                                           new ClearableDatePicker(null, this.religion::getEnable));
            items.add(this.enableField);
        }

        FilteredList<Country> possibleDefenders = countries.filtered(country -> country.getOverlord() == null);
        if (this.religion.hasDefenderOfFaith() && !possibleDefenders.isEmpty()) {
            countries.add(0, EditorController.dummyCountry);

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

        this.propertySheet.getItems().setAll(items);
    }

    public void update() {
        if (this.enableField != null) {
            this.enableField.setValue(this.religion.getEnable());
        }

        if (this.defenderOfFaithField != null) {
            this.defenderOfFaithField.setValue(this.religion.getDefender() == null ?
                                               EditorController.dummyCountry : this.religion.getDefender());
        }
    }

    public void validate(ActionEvent actionEvent) {
        if (this.enableField != null) {
            if (!Objects.equals(this.religion.getEnable(), this.enableField.getTrueValue())) {
                this.religion.setEnable(this.enableField.getTrueValue());
            }
        }
        if (this.defenderOfFaithField != null) {
            if (!Objects.equals(this.religion.getDefender(), this.defenderOfFaithField.getSelectedValue())) {
                this.religion.setDefender(this.defenderOfFaithField.getSelectedValue(), this.save.getDate());
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
