package fr.osallek.eu4saveeditor.controller.control;

import fr.osallek.eu4parser.model.game.ImperialReform;
import javafx.collections.ListChangeListener;

public class ImperialReformListChangeListener implements ListChangeListener<ImperialReform> {

    private boolean sorting = false;

    @Override
    public void onChanged(Change<? extends ImperialReform> c) {
        c.next();

        if (!this.sorting && (c.wasAdded() || c.wasRemoved()) && c.getList().size() > 1) {
            this.sorting = true;
            c.getList().sorted();
            this.sorting = false;
        }
    }
}
