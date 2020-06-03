package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.eu4parser.model.game.Culture;
import com.osallek.eu4parser.model.game.Religion;
import com.osallek.eu4parser.model.game.TradeGood;
import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4parser.model.save.province.SaveProvince;
import javafx.collections.ObservableList;
import javafx.scene.canvas.Canvas;
import javafx.scene.control.Label;
import javafx.scene.control.ToggleButton;
import javafx.scene.layout.VBox;
import org.controlsfx.control.SegmentedButton;

import java.util.Map;
import java.util.Objects;

public abstract class AbstractMapView {

    protected final SaveProvince[][] provincesMap;

    protected final Map<Integer, DrawableProvince> drawableProvinces;

    protected final Canvas canvas;

    protected final Save save;

    protected final MapViewType type;

    protected final VBox editPane;

    protected final ObservableList<Country> playableCountries;

    protected final ObservableList<Culture> cultures;

    protected final ObservableList<Religion> religions;

    protected final ObservableList<TradeGood> tradeGoods;

    protected final Label titleLabel;

    protected final SegmentedButton tabsSegmentedButton;

    protected final ToggleButton saveButton;

    protected boolean selected;

    public AbstractMapView(SaveProvince[][] provincesMap, Map<Integer, DrawableProvince> drawableProvinces, Canvas canvas, VBox editPane, Save save, MapViewType type,
                           ObservableList<Country> playableCountries, ObservableList<Culture> cultures,
                           ObservableList<Religion> religions, ObservableList<TradeGood> tradeGoods) {
        this.provincesMap = provincesMap;
        this.drawableProvinces = drawableProvinces;
        this.canvas = canvas;
        this.editPane = editPane;
        this.save = save;
        this.type = type;
        this.playableCountries = playableCountries;
        this.cultures = cultures;
        this.religions = religions;
        this.tradeGoods = tradeGoods;

        this.titleLabel = new Label();
        this.titleLabel.setVisible(false);

        this.saveButton = new ToggleButton(save.getGame().getLocalisation("SM_GAME"));
        this.saveButton.selectedProperty().addListener((observable, oldValue, newValue) -> {
            if (Boolean.FALSE.equals(oldValue) && Boolean.TRUE.equals(newValue)) {
                selectSaveButton();
            }
        });
        this.saveButton.setSelected(true);
        this.saveButton.disableProperty().bind(this.saveButton.selectedProperty());

        this.tabsSegmentedButton = new SegmentedButton(this.saveButton);
        this.tabsSegmentedButton.getStyleClass().add(SegmentedButton.STYLE_CLASS_DARK);
        this.tabsSegmentedButton.setMaxWidth(Double.MAX_VALUE);
        this.tabsSegmentedButton.getStylesheets()
                                .add(getClass().getClassLoader().getResource("styles/style.css").toExternalForm());
    }

    public abstract void draw();

    public abstract void onProvinceSelected(SaveProvince province);

    public abstract void removeSheets();

    protected abstract void updateTitle();

    protected void selectSaveButton() {
        removeSheets();
        updateTitle();
    }

    public boolean isSelected() {
        return selected;
    }

    public void setSelected(boolean selected) {
        this.selected = selected;
    }

    protected void clearTabsSegmentedButton() {
        this.tabsSegmentedButton.getButtons().subList(1, this.tabsSegmentedButton.getButtons().size()).clear();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof AbstractMapView)) {
            return false;
        }
        AbstractMapView that = (AbstractMapView) o;
        return type == that.type;
    }

    @Override
    public int hashCode() {
        return Objects.hash(type);
    }
}
