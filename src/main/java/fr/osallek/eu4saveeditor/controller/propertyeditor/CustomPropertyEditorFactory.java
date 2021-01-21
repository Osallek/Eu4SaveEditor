package fr.osallek.eu4saveeditor.controller.propertyeditor;

import fr.osallek.eu4saveeditor.controller.pane.CustomDefaultPropertyEditorFactory;
import fr.osallek.eu4saveeditor.controller.pane.CustomPropertySheet;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ButtonItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.CheckComboBoxItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableCheckComboBoxItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableColorPickerItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableComboBoxItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableDatePickerItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSliderIntItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSliderItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSpinnerItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableTextItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.ComboBoxItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.HBoxItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.PropertySheetItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.SelectableGridViewItem;
import fr.osallek.eu4saveeditor.controller.propertyeditor.item.TextItem;
import org.controlsfx.property.editor.PropertyEditor;

public class CustomPropertyEditorFactory extends CustomDefaultPropertyEditorFactory {

    @Override
    public PropertyEditor<?> call(CustomPropertySheet.Item item) {
        Class<?> type = item.getType();

        if (type == TextItem.class) {
            return CustomEditors.createTextEditor((TextItem) item);
        }

        if (type == ClearableTextItem.class) {
            return CustomEditors.createClearableLabeledTextEditor((ClearableTextItem) item);
        }

        if (type == ComboBoxItem.class) {
            return CustomEditors.createCustomChoiceEditor((ComboBoxItem<?>) item);
        }

        if (type == ClearableDatePickerItem.class) {
            return CustomEditors.createClearableDatePickerEditor((ClearableDatePickerItem) item);
        }

        if (type == ClearableComboBoxItem.class) {
            return CustomEditors.createClearableComboBoxEditor((ClearableComboBoxItem<?>) item);
        }

        if (type == CheckComboBoxItem.class) {
            return CustomEditors.createCheckComboBoxEditor((CheckComboBoxItem<?>) item);
        }

        if (type == ClearableCheckComboBoxItem.class) {
            return CustomEditors.createClearableCheckComboBoxEditor((ClearableCheckComboBoxItem<?>) item);
        }

        if (type == ClearableSpinnerItem.class) {
            return CustomEditors.createClearableSpinnerEditor((ClearableSpinnerItem<?>) item);
        }

        if (type == ClearableSliderItem.class) {
            return CustomEditors.createClearableSliderEditor((ClearableSliderItem) item);
        }

        if (type == ClearableSliderIntItem.class) {
            return CustomEditors.createClearableSliderEditor((ClearableSliderIntItem) item);
        }

        if (type == SelectableGridViewItem.class) {
            return CustomEditors.createSelectableGridViewEditor((SelectableGridViewItem<?>) item);
        }

        if (type == HBoxItem.class) {
            return CustomEditors.createHBox((HBoxItem<?>) item);
        }

        if (type == ButtonItem.class) {
            return CustomEditors.createButton((ButtonItem) item);
        }

        if (type == PropertySheetItem.class) {
            return CustomEditors.createPropertySheet((PropertySheetItem) item);
        }

        if (type == ClearableColorPickerItem.class) {
            return CustomEditors.createColorPickerSheet((ClearableColorPickerItem) item);
        }

        return super.call(item);
    }
}
