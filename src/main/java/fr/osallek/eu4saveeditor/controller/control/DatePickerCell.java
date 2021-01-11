package fr.osallek.eu4saveeditor.controller.control;

import javafx.event.Event;
import javafx.geometry.Pos;
import javafx.scene.control.ContentDisplay;
import javafx.scene.control.DateCell;
import javafx.scene.control.DatePicker;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TablePosition;
import javafx.scene.control.TableView;
import javafx.util.Callback;
import javafx.util.converter.LocalDateStringConverter;
import org.apache.commons.lang3.BooleanUtils;

import java.time.LocalDate;
import java.time.chrono.Chronology;
import java.time.format.FormatStyle;
import java.util.Locale;
import java.util.Objects;

public class DatePickerCell<S> extends TableCell<S, LocalDate> {

    public static <S> Callback<TableColumn<S, LocalDate>, TableCell<S, LocalDate>> forTableColumn() {
        return forTableColumn(null, null);
    }

    public static <S> Callback<TableColumn<S, LocalDate>, TableCell<S, LocalDate>> forTableColumn(LocalDate startDate, LocalDate endDate) {
        return list -> new DatePickerCell<>(startDate, endDate);
    }

    private final LocalDate startDate;

    private final LocalDate endDate;

    private DatePicker datePicker;

    public DatePickerCell() {
        this(null, null);
    }

    public DatePickerCell(LocalDate startDate, LocalDate endDate) {
        this.startDate = startDate;
        this.endDate = endDate;
    }

    @Override
    public void updateItem(LocalDate item, boolean empty) {
        super.updateItem(item, empty);

        if (empty) {
            setText(null);
            setGraphic(null);
        } else {
            if (isEditing()) {
                setText(null);
                setGraphic(this.datePicker);
                setContentDisplay(ContentDisplay.GRAPHIC_ONLY);
            } else {
                if (this.datePicker == null) {
                    setText(new LocalDateStringConverter(FormatStyle.SHORT, null, Chronology.ofLocale(Locale.getDefault(Locale.Category.FORMAT)))
                                    .toString(item));
                } else {
                    setText(this.datePicker.getConverter().toString(item));
                }

                setGraphic(null);
                setContentDisplay(ContentDisplay.TEXT_ONLY);
            }
        }
    }

    private void createDatePicker() {
        if (this.datePicker == null) {
            this.datePicker = new DatePicker(getItem());
            this.datePicker.setEditable(true);
            this.datePicker.setShowWeekNumbers(false);
            this.datePicker.setDayCellFactory(d -> new DateCell() {
                @Override
                public void updateItem(LocalDate item, boolean empty) {
                    super.updateItem(item, empty);
                    setDisable((endDate != null && item.isAfter(endDate)) || (startDate != null && item.isBefore(startDate)));
                }
            });
            this.datePicker.valueProperty().addListener((observable, oldValue, newValue) -> {
                if (!Objects.equals(oldValue, newValue)
                    && ((endDate != null && newValue.isAfter(endDate)) || (startDate != null && newValue.isBefore(startDate)))) {
                    this.datePicker.setValue(oldValue);
                }
            });
            this.datePicker.focusedProperty().addListener((observable, oldValue, newValue) -> {
                if (BooleanUtils.isTrue(oldValue) && BooleanUtils.isFalse(newValue) && this.datePicker.isEditable()) {
                    this.datePicker.setValue(this.datePicker.getConverter().fromString(this.datePicker.getEditor().getText()));

                    final TableView<S> table = getTableView();
                    if (table != null) {
                        Event.fireEvent(getTableColumn(),
                                        new TableColumn.CellEditEvent<>(
                                                table,
                                                new TablePosition<>(table, getTableRow().getIndex(), getTableColumn()),
                                                TableColumn.editCommitEvent(),
                                                this.datePicker.getValue()
                                        ));
                    }

                    updateItem(this.datePicker.getValue(), this.datePicker.getValue() == null);
                }
            });
        }

        setAlignment(Pos.CENTER);
    }

    @Override
    public void startEdit() {
        if (!isEmpty()) {
            super.startEdit();
            createDatePicker();
            setText(this.datePicker.getConverter().toString(getItem()));
            setGraphic(this.datePicker);
            setContentDisplay(ContentDisplay.GRAPHIC_ONLY);
        }
    }

    @Override
    public void cancelEdit() {
        super.cancelEdit();
        setContentDisplay(ContentDisplay.TEXT_ONLY);
        setText(this.datePicker.getConverter().toString(getItem()));
        setGraphic(null);
    }

    public DatePicker getDatePicker() {
        return datePicker;
    }

    public void setDatePicker(DatePicker datePicker) {
        this.datePicker = datePicker;
    }
}
