package fr.osallek.eu4saveeditor.controller.object;

import java.util.Arrays;
import javafx.scene.Node;

public class BootstrapColumn {

    private final Node content;

    int[] columnWidths;

    public BootstrapColumn(Node content, int[] columnWidths) {
        this.content = content;
        this.columnWidths = columnWidths;

        if (this.columnWidths.length < 5) {
            this.columnWidths = Arrays.copyOf(this.columnWidths, 5);
            Arrays.fill(this.columnWidths, columnWidths.length, 5, -1);
        }
    }

    /**
     * Iterate through breakpoints, beginning at the specified bp, travelling down. Return first valid bp value. If none are valid, return 1
     *
     * @param breakPoint the breakpoint at which to determine the column width
     *
     * @return the requested width at that breakpoint, or based on a lower breakpoint if the specified bp has not been set.
     */
    public int getColumnWidth(Breakpoint breakPoint) {
        //Iterate through breakpoints, beginning at the specified bp, travelling down. Return first valid bp value.
        for (int i = breakPoint.getValue(); i >= 0; i--) {
            if (isValid(this.columnWidths[i])) {
                return this.columnWidths[i];
            }
        }

        //If none are valid, return 1
        return 1;
    }

    /**
     * Get the node in this column
     *
     * @return the content.
     */
    public Node getContent() {
        return content;
    }

    /**
     * Whether a value is between 1 and 12 (i.e. a valid column width)
     *
     * @param value the value being tested
     *
     * @return whether the value is a valid column width
     */
    private boolean isValid(int value) {
        return value > 0 && value <= 12;
    }
}
