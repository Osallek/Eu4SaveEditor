package com.osallek.eu4saveeditor.controller.propertyeditor;

import com.osallek.eu4saveeditor.controller.propertyeditor.item.ButtonItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.CheckComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableCheckComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSliderItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableSpinnerItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableTextItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.HBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.SelectableGridViewItem;
import org.controlsfx.control.PropertySheet;
import org.controlsfx.property.editor.DefaultPropertyEditorFactory;
import org.controlsfx.property.editor.PropertyEditor;

public class CustomPropertyEditorFactory extends DefaultPropertyEditorFactory {

    @Override
    public PropertyEditor<?> call(PropertySheet.Item item) {
        Class<?> type = item.getType();

        if (type == ClearableTextItem.class) {
            return CustomEditors.createClearableLabeledTextEditor((ClearableTextItem) item);
        }

        if (type == ComboBoxItem.class) {
            return CustomEditors.createCustomChoiceEditor((ComboBoxItem<?>) item);
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

        if (type == SelectableGridViewItem.class) {
            return CustomEditors.createSelectableGridViewEditor((SelectableGridViewItem<?>) item);
        }

        if (type == HBoxItem.class) {
            return CustomEditors.createHBox((HBoxItem<?>) item);
        }

        if (type == ButtonItem.class) {
            return CustomEditors.createButton((ButtonItem) item);
        }

        return super.call(item);
    }
}
