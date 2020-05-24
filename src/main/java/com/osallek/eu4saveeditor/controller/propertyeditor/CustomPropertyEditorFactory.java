package com.osallek.eu4saveeditor.controller.propertyeditor;

import com.osallek.eu4saveeditor.controller.propertyeditor.item.CheckComboBoxItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ClearableTextItem;
import com.osallek.eu4saveeditor.controller.propertyeditor.item.ComboBoxItem;
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

        if (type == CheckComboBoxItem.class) {
            return CustomEditors.createCheckComboBoxEditor((CheckComboBoxItem<?>) item);
        }

        return super.call(item);
    }
}
