package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4saveeditor.controller.pane.CustomPropertySheetSkin;
import com.osallek.eu4saveeditor.controller.propertyeditor.CustomPropertyEditorFactory;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableTextItem;
import com.osallek.eu4saveeditor.controller.validator.CustomGraphicValidationDecoration;
import javafx.event.ActionEvent;
import javafx.scene.layout.VBox;
import org.controlsfx.control.PropertySheet;
import org.controlsfx.validation.ValidationSupport;
import org.controlsfx.validation.Validator;
import org.controlsfx.validation.decoration.CompoundValidationDecoration;
import org.controlsfx.validation.decoration.StyleClassValidationDecoration;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

public class SavePropertySheet extends VBox {

    private Save save;

    private final PropertySheet propertySheet;

    private final ValidationSupport validationSupport;

    private final ClearableTextItem nameField;

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

        this.nameField = new ClearableTextItem(SheetCategory.PROVINCE_GENERAL,
                                               save.getGame().getLocalisation("LEDGER_NAME"));
        this.nameField.getTextField()
                      .getStylesheets()
                      .add(getClass().getClassLoader().getResource("styles/propertySheetsStyle.css").toExternalForm());
        this.validationSupport.registerValidator(this.nameField.getTextField(), Validator.createEmptyValidator("Text is required"));
    }

    public void update() {
        String expandedPaneName = this.propertySheetSkin.getAccordion().getExpandedPane() == null ? null :
                                  this.propertySheetSkin.getAccordion().getExpandedPane().getText();

        List<PropertySheet.Item> items = new ArrayList<>();

        this.nameField.setValue(this.save.getName());
        this.nameField.setSupplier(this.save::getName);
        items.add(this.nameField);

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
    }

    public void validate(ActionEvent actionEvent) {
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
