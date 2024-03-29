package fr.osallek.eu4saveeditor.controller.mapview;

import fr.osallek.eu4parser.model.save.province.SaveProvince;
import fr.osallek.eu4saveeditor.controller.pane.CustomPropertySheet;
import java.util.List;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;

public abstract class AbstractMapView {

    protected final MapViewType type;

    protected final MapViewContainer mapViewContainer;

    protected final Property<Boolean> selected;

    public AbstractMapView(MapViewContainer mapViewContainer, MapViewType type) {
        this.type = type;
        this.mapViewContainer = mapViewContainer;
        this.selected = new SimpleBooleanProperty(false);
        this.selected.addListener((observable, oldValue, newValue) -> {
            if (Boolean.FALSE.equals(oldValue) && Boolean.TRUE.equals(newValue)) {
                onSelected();
            }
        });
    }

    public MapViewType getType() {
        return this.type;
    }

    public void setSelected(boolean selected) {
        this.selected.setValue(selected);
    }

    public abstract void draw();

    public abstract void onProvinceSelected(SaveProvince province);

    public abstract List<CustomPropertySheet> removeSheets();

    public abstract String updateTitle(SaveProvince selectedProvince);

    public abstract void onSelected();
}
