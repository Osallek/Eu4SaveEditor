package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.clausewitzparser.common.ClausewitzUtils;
import com.osallek.eu4parser.model.game.Culture;
import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.SaveReligion;
import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4saveeditor.controller.control.ClearableSpinnerInt;
import com.osallek.eu4saveeditor.controller.pane.CustomPropertySheet;
import com.osallek.eu4saveeditor.controller.pane.CustomPropertySheetSkin;
import com.osallek.eu4saveeditor.controller.propertyeditor.CustomPropertyEditorFactory;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.CheckBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSpinnerItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableTextItem;
import com.osallek.eu4saveeditor.controller.validator.CustomGraphicValidationDecoration;
import com.osallek.eu4saveeditor.i18n.SheetCategory;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.layout.VBox;
import org.controlsfx.validation.ValidationSupport;
import org.controlsfx.validation.Validator;
import org.controlsfx.validation.decoration.CompoundValidationDecoration;
import org.controlsfx.validation.decoration.StyleClassValidationDecoration;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

public class CountryPropertySheet extends VBox {

    private Country country;

    private final CustomPropertySheet propertySheet;

    private final ValidationSupport validationSupport;

    private final ClearableTextItem nameField;

    private CheckBoxItem wasPlayerField;

    private final ClearableSpinnerItem<Integer> governmentRankField;

    private CustomPropertySheetSkin propertySheetSkin;

    public CountryPropertySheet(Save save, ObservableList<Country> playableCountries, ObservableList<Culture> cultures,
                                ObservableList<SaveReligion> religions) {
        this.propertySheet = new CustomPropertySheet();
        this.propertySheet.setPropertyEditorFactory(new CustomPropertyEditorFactory());
        this.propertySheet.setMode(CustomPropertySheet.Mode.CATEGORY);
        this.propertySheet.setCategoryComparator(Comparator.comparing(SheetCategory::getByLocale));
        this.propertySheet.setModeSwitcherVisible(false);
        this.propertySheet.setSearchBoxVisible(false);

        this.propertySheetSkin = new CustomPropertySheetSkin(this.propertySheet);
        this.propertySheet.setSkin(this.propertySheetSkin);

        //GENERAL
        this.nameField = new ClearableTextItem(SheetCategory.GENERAL, save.getGame().getLocalisation("LEDGER_NAME"));
        this.nameField.getTextField()
                      .getStylesheets()
                      .add(getClass().getClassLoader().getResource("styles/propertySheetsStyle.css").toExternalForm());

        this.wasPlayerField = new CheckBoxItem(SheetCategory.GENERAL, save.getGame().getLocalisationClean("WAS_PLAYER"), false);

        this.governmentRankField = new ClearableSpinnerItem<>(SheetCategory.GENERAL, save.getGame().getLocalisationClean("GOV_RANK"),
                                                              new ClearableSpinnerInt(1, save.getGame().getMaxGovRank(), 1));

        //LEDGER_GOVERNMENT_NAME

        this.validationSupport = new ValidationSupport();
        this.validationSupport.registerValidator(this.nameField.getTextField(), Validator.createEmptyValidator("Text is required"));
        this.validationSupport.setValidationDecorator(new CompoundValidationDecoration(new CustomGraphicValidationDecoration(),
                                                                                       new StyleClassValidationDecoration("validation-error", null)));
    }

    public void update(Country country) {
        update(country, false);
    }

    public void update(Country country, boolean force) {
        if (force || this.country == null || !this.country.equals(country)) {
            this.country = country;

            if (this.country == null) {
                this.propertySheet.getItems().clear();
            } else {
                String expandedPaneName = this.propertySheetSkin.getAccordion().getExpandedPane() == null ? null :
                                          this.propertySheetSkin.getAccordion().getExpandedPane().getText();

                List<CustomPropertySheet.Item> items = new ArrayList<>();

                //GENERAL
                this.nameField.setValue(ClausewitzUtils.removeQuotes(this.country.getLocalizedName()));
                this.nameField.setSupplier(() -> ClausewitzUtils.removeQuotes(this.country.getLocalizedName()));
                this.nameField.setEditable(this.country.isNameEditable());
                items.add(this.nameField);

                this.wasPlayerField.setValue(Boolean.TRUE.equals(this.country.wasPlayer()));
                items.add(this.wasPlayerField);

                this.governmentRankField.setValue(this.country.getGovernmentRank());
                this.governmentRankField.setSupplier(this.country::getGovernmentRank);
                items.add(this.governmentRankField);

                this.propertySheet.getItems().setAll(items);

                if (expandedPaneName != null) {
                    this.propertySheetSkin.getAccordion()
                                          .getPanes()
                                          .stream()
                                          .filter(titledPane -> titledPane.getText().equals(expandedPaneName))
                                          .findFirst()
                                          .ifPresent(titledPane -> this.propertySheetSkin.getAccordion().setExpandedPane(titledPane));
                }
            }
        }
    }

    public void validate(ActionEvent actionEvent) {
        if (!ClausewitzUtils.removeQuotes(this.country.getLocalizedName()).equals(this.nameField.getText())) {
            this.country.setLocalizedName(this.nameField.getText());
        }

        if (!Objects.equals(this.country.wasPlayer(), this.wasPlayerField.isSelected())) {
            this.country.setWasPlayer(this.wasPlayerField.isSelected());
        }

        update(this.country, true);
    }

    public Country getCountry() {
        return country;
    }

    public ValidationSupport getValidationSupport() {
        return validationSupport;
    }

    public CustomPropertySheet getPropertySheet() {
        return propertySheet;
    }
}
