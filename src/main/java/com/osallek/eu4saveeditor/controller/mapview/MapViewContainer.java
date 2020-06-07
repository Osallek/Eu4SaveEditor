package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.eu4parser.model.game.Culture;
import com.osallek.eu4parser.model.game.Religion;
import com.osallek.eu4parser.model.game.TradeGood;
import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4parser.model.save.province.SaveProvince;
import javafx.beans.property.ReadOnlyBooleanProperty;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.canvas.Canvas;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ToggleButton;
import javafx.scene.layout.VBox;
import org.controlsfx.control.PropertySheet;
import org.controlsfx.control.SegmentedButton;

import java.util.Collections;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;

public class MapViewContainer {

    private final SaveProvince[][] provincesMap;

    private final Map<Integer, DrawableProvince> drawableProvinces;

    private final Canvas canvas;

    private final Save save;

    private final VBox editPane;

    private final Map<MapViewType, AbstractMapView> mapViews;

    private AbstractMapView selectedMapView;

    private SaveProvince selectedProvince;

    private final ObservableList<Country> playableCountries;

    private final ObservableList<Culture> cultures;

    private final ObservableList<Religion> religions;

    private final ObservableList<TradeGood> tradeGoods;

    private final ObservableList<SaveProvince> cities;

    private final Label titleLabel;

    private final SegmentedButton tabsSegmentedButton;

    private final ToggleButton saveButton;

    private final SavePropertySheet saveSheet;

    private final Button submitButton;

    public MapViewContainer(SaveProvince[][] provincesMap, Map<Integer, DrawableProvince> drawableProvinces,
                            Canvas canvas, VBox editPane, Save save, ObservableList<Country> playableCountries,
                            ObservableList<Culture> cultures, ObservableList<Religion> religions,
                            ObservableList<TradeGood> tradeGoods, ObservableList<SaveProvince> cities) {
        this.provincesMap = provincesMap;
        this.drawableProvinces = drawableProvinces;
        this.canvas = canvas;
        this.editPane = editPane;
        this.save = save;
        this.cities = cities;
        this.saveSheet = new SavePropertySheet(this.save, this.cities);
        this.mapViews = new EnumMap<>(MapViewType.class);
        this.playableCountries = playableCountries;
        this.cultures = cultures;
        this.religions = religions;
        this.tradeGoods = tradeGoods;

        this.titleLabel = new Label();

        this.submitButton = new Button(save.getGame().getLocalisation("DONE"));

        this.saveButton = new ToggleButton(save.getGame().getLocalisation("SM_GAME"));
        this.saveButton.selectedProperty().addListener((observable, oldValue, newValue) -> {
            if (Boolean.FALSE.equals(oldValue) && Boolean.TRUE.equals(newValue)) {
                selectSaveButton();
            }
        });
        this.saveButton.disableProperty().bind(this.saveButton.selectedProperty());

        this.tabsSegmentedButton = new SegmentedButton();
        this.tabsSegmentedButton.getStyleClass().add(SegmentedButton.STYLE_CLASS_DARK);
        this.tabsSegmentedButton.setMaxWidth(Double.MAX_VALUE);
        this.tabsSegmentedButton.getStylesheets()
                                .add(getClass().getClassLoader().getResource("styles/style.css").toExternalForm());
        addTabsSegmentedButtons(this.saveButton);

        this.editPane.getChildren().clear();
        this.editPane.getChildren().add(this.tabsSegmentedButton);
        this.editPane.getChildren().add(this.titleLabel);
        this.editPane.getChildren().add(this.submitButton);

        this.saveButton.setSelected(true);
    }

    public void draw() {
        if (this.selectedMapView != null) {
            this.selectedMapView.draw();
        }
    }

    public void registerMapView(MapViewType mapViewType) {
        this.mapViews.put(mapViewType, mapViewType.getMapView(this));
    }

    public void selectMapView(MapViewType mapViewType) {
        if (this.mapViews.containsKey(mapViewType)) {
            if (this.selectedMapView != null) {
                this.selectedMapView.setSelected(false);
            }

            clearTabsSegmentedButton();
            this.selectedMapView = this.mapViews.get(mapViewType);
            this.selectedMapView.setSelected(true);
        }
    }

    public void onProvinceSelected(SaveProvince province) {
        this.selectedProvince = province;

        if (this.selectedMapView != null) {
            this.selectedMapView.onProvinceSelected(this.selectedProvince);
        }
    }

    public void updateTitle() {
        if (this.saveButton.isSelected()) {
            this.titleLabel.setText(this.save.getName());
        } else {
            this.titleLabel.setText(this.selectedMapView.updateTitle(this.selectedProvince));
        }
    }

    public void selectSaveButton() {
        removeSheets();
        updateTitle();
        this.saveSheet.update();
        addSheets(Collections.singletonList(this.saveSheet.getPropertySheet()));
        setSubmitButtonOnAction(event -> {
            this.saveSheet.validate(event);
            this.saveSheet.update();
            updateTitle();
        });
        bindSubmitButtonDisableProperty(this.saveSheet.getValidationSupport().invalidProperty());
    }

    public void addSheets(List<PropertySheet> sheets) {
        for (int i = 0; i < sheets.size(); i++) {
            if (!this.editPane.getChildren().contains(sheets.get(i))) {
                this.editPane.getChildren().add(i + 2, sheets.get(i)); //+2 for buttons and title
            }
        }
    }

    public void removeSaveSheet() {
        removeSheet(this.saveSheet.getPropertySheet());
    }

    public void removeSheet(PropertySheet propertySheet) {
        this.editPane.getChildren().remove(propertySheet);
    }

    public void removeSheets() {
        if (this.selectedMapView != null) {
            this.editPane.getChildren().removeAll(this.selectedMapView.removeSheets());
        }
    }

    public void setSubmitButtonOnAction(EventHandler<ActionEvent> value) {
        this.submitButton.setOnAction(value);
    }

    public void bindSubmitButtonDisableProperty(ReadOnlyBooleanProperty observable) {
        this.submitButton.disableProperty().unbind();
        this.submitButton.disableProperty().bind(observable);
    }

    public void clearTabsSegmentedButton() {
        this.tabsSegmentedButton.getButtons().subList(1, this.tabsSegmentedButton.getButtons().size()).clear();
    }

    public void addTabsSegmentedButtons(ToggleButton... buttons) {
        this.tabsSegmentedButton.getButtons().addAll(buttons);
        this.tabsSegmentedButton.getButtons()
                                .forEach(toggleButton -> toggleButton.prefWidthProperty()
                                                                     .bind(this.tabsSegmentedButton.widthProperty()
                                                                                                   .divide(this.tabsSegmentedButton
                                                                                                                   .getButtons()
                                                                                                                   .size())));
        this.tabsSegmentedButton.getButtons()
                                .forEach(button -> button.disableProperty().bind(button.selectedProperty()));
    }

    public SaveProvince[][] getProvincesMap() {
        return provincesMap;
    }

    public Map<Integer, DrawableProvince> getDrawableProvinces() {
        return drawableProvinces;
    }

    public Canvas getCanvas() {
        return canvas;
    }

    public Save getSave() {
        return save;
    }

    public VBox getEditPane() {
        return editPane;
    }

    public ObservableList<Country> getPlayableCountries() {
        return playableCountries;
    }

    public ObservableList<Culture> getCultures() {
        return cultures;
    }

    public ObservableList<Religion> getReligions() {
        return religions;
    }

    public ObservableList<TradeGood> getTradeGoods() {
        return tradeGoods;
    }

    public SegmentedButton getTabsSegmentedButton() {
        return tabsSegmentedButton;
    }

    public SaveProvince getSelectedProvince() {
        return selectedProvince;
    }
}
