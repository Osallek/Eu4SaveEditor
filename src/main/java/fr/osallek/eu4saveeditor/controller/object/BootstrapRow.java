package fr.osallek.eu4saveeditor.controller.object;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javafx.scene.layout.GridPane;

public class BootstrapRow {

    private final List<BootstrapColumn> columns = new ArrayList<>();

    private final boolean center;

    public BootstrapRow(boolean center) {
        this.center = center;
    }

    /**
     * Add a resizeable bootstrap column object
     *
     * @param column the object to be added
     */
    public void addColumn(BootstrapColumn column) {
        if (column == null) {
            return;
        }

        this.columns.add(column);
    }

    /**
     * Get all columns in the row
     *
     * @return an unmodifiable view of the columns in this row.
     */
    public List<BootstrapColumn> getColumns() {
        return Collections.unmodifiableList(this.columns);
    }

    public int calculateRowPositions(int lastGridPaneRow, Breakpoint currentWindowSize) {
        int inputRow = lastGridPaneRow;
        if (this.getColumns().isEmpty()) {
            return 0;
        }

        int currentGridPaneColumn = 0; //start in the first column
        for (BootstrapColumn column : this.getColumns()) {
            int contentWidth = column.getColumnWidth(currentWindowSize);

            if (currentGridPaneColumn + contentWidth > 12) {
                lastGridPaneRow++;
                currentGridPaneColumn = 0;
            }

            if (this.center) {
                currentGridPaneColumn = (12 - contentWidth) / 2;
            }

            GridPane.setConstraints(column.getContent(), currentGridPaneColumn, lastGridPaneRow, contentWidth, 1);

            currentGridPaneColumn += contentWidth;
        }

        return lastGridPaneRow - inputRow + 1;
    }

}
