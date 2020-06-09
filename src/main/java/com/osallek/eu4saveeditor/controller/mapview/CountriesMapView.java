package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.clausewitzparser.common.ClausewitzUtils;
import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4parser.model.save.province.SaveProvince;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.control.ToggleButton;
import javafx.scene.image.PixelWriter;
import javafx.scene.paint.Color;
import org.controlsfx.control.PropertySheet;

import java.util.Collections;

public class CountriesMapView extends AbstractMapView {

    private final ToggleButton countryButton;

    private final ToggleButton provinceButton;

    private final ProvincePropertySheet provinceSheet;

    public CountriesMapView(MapViewContainer mapViewContainer) {
        super(mapViewContainer, MapViewType.COUNTRIES_MAP_VIEW);
        this.provinceSheet = new ProvincePropertySheet(this.mapViewContainer.getSave(),
                                                       this.mapViewContainer.getPlayableCountries(),
                                                       this.mapViewContainer.getCultures(),
                                                       this.mapViewContainer.getReligions(),
                                                       this.mapViewContainer.getTradeGoods());
        this.provinceSheet.countryChangedProperty().addListener((observable, oldValue, newValue) -> {
            if (Boolean.FALSE.equals(oldValue) && Boolean.TRUE.equals(newValue)) {
                drawProvince(this.provinceSheet.getProvince().getId());
            }
        });

        this.countryButton = new ToggleButton(this.mapViewContainer.getSave()
                                                                   .getGame()
                                                                   .getLocalisation("TRIGGER_COUNTRY"));
        this.countryButton.selectedProperty().addListener((observable, oldValue, newValue) -> {
            if (Boolean.FALSE.equals(oldValue) && Boolean.TRUE.equals(newValue)) {
                selectCountryButton();
            }
        });
        this.countryButton.disableProperty().bind(this.countryButton.selectedProperty());

        this.provinceButton = new ToggleButton(this.mapViewContainer.getSave()
                                                                    .getGame()
                                                                    .getLocalisation("UNKNOWN_LOC"));
        this.provinceButton.selectedProperty().addListener((observable, oldValue, newValue) -> {
            if (Boolean.FALSE.equals(oldValue) && Boolean.TRUE.equals(newValue)) {
                selectProvinceButton();
            }
        });
        this.provinceButton.disableProperty().bind(this.provinceButton.selectedProperty());
    }

    @Override
    public MapViewType getType() {
        return this.type;
    }

    @Override
    public void setSelected(boolean selected) {
        this.selected.setValue(selected);
    }

    @Override
    public void draw() {
        for (DrawableProvince drawableProvince : this.mapViewContainer.getDrawableProvinces().values()) {
            if (drawableProvince != null) {
                drawProvince(drawableProvince.getProvince().getId());
            }
        }
    }

    @Override
    public void onProvinceSelected(SaveProvince province) {
        this.provinceSheet.update(province);

        if (Boolean.FALSE.equals(this.selected.getValue())) {
            this.mapViewContainer.addTabsSegmentedButtons(this.countryButton, this.provinceButton);
            this.mapViewContainer.bindSubmitButtonDisableProperty(this.provinceSheet.getValidationSupport()
                                                                                    .invalidProperty());

            if (this.countryButton.isSelected()) {
                selectCountryButton();
            } else if (this.provinceButton.isSelected()) {
                selectProvinceButton();
            }

        }
    }

    @Override
    public void drawProvince(int provinceId) {
        GraphicsContext graphicsContext = this.mapViewContainer.getCanvas().getGraphicsContext2D();
        Color color = getOwnerColor(this.mapViewContainer.getDrawableProvinces().get(provinceId).getProvince());
        graphicsContext.setFill(color);
        this.mapViewContainer.getDrawableProvinces().get(provinceId).getRectangles()
                             .forEach(rectangle -> graphicsContext.fillRect(rectangle.x, rectangle.y, rectangle
                                     .width, rectangle.height));

        PixelWriter pixelWriter = graphicsContext.getPixelWriter();
        this.mapViewContainer.getDrawableProvinces().get(provinceId).getBorders()
                             .forEach(point2D -> pixelWriter.setColor((int) point2D.getX(), (int) point2D.getY(), Color.BLACK));
    }

    @Override
    public PropertySheet[] removeSheets() {
        if (this.provinceSheet != null) {
            return new PropertySheet[] {this.provinceSheet.getPropertySheet()};
        }

        return new PropertySheet[] {};
    }

    @Override
    public String updateTitle(SaveProvince selectedProvince) {
        if (this.countryButton.isSelected()) {
            return selectedProvince.getCountry().getLocalizedName();
        } else if (this.provinceButton.isSelected()) {
            return getTitle(selectedProvince);
        }

        return null;
    }

    @Override
    public void onSelected() {
        this.mapViewContainer.addTabsSegmentedButtons(this.countryButton, this.provinceButton);
    }

    private void selectCountryButton() {
        this.mapViewContainer.removeSaveSheet();
        this.mapViewContainer.removeSheet(this.provinceSheet.getPropertySheet());
        this.mapViewContainer.updateTitle();
    }

    private void selectProvinceButton() {
        this.mapViewContainer.removeSaveSheet();
        this.mapViewContainer.addSheets(Collections.singletonList(this.provinceSheet.getPropertySheet()));
        this.mapViewContainer.updateTitle();
        this.mapViewContainer.setSubmitButtonOnAction(e -> {
            this.provinceSheet.validate(e);
            this.provinceSheet.update(this.provinceSheet.getProvince());
            this.mapViewContainer.updateTitle();
        });

        this.mapViewContainer.bindSubmitButtonDisableProperty(this.provinceSheet.getValidationSupport()
                                                                                .invalidProperty());
    }

    private String getTitle(SaveProvince saveProvince) {
        String title = ClausewitzUtils.removeQuotes(saveProvince.getName());

        if (saveProvince.getCountry() != null) {
            title += " (" + saveProvince.getCountry().getLocalizedName() + ")";
        }

        return title;
    }

    private Color getOwnerColor(SaveProvince province) {
        if (province.getCountry() != null) {
            return countryToMapColor(province.getCountry());
        } else {
            if (province.isOcean() || province.isLake()) {
                return Color.rgb(68, 107, 163);
            } else if (province.isImpassable()) {
                return Color.rgb(94, 94, 94);
            } else {
                return Color.rgb(148, 146, 149);
            }
        }
    }

    private Color countryToMapColor(Country country) {
        return Color.rgb(country.getColors().getCountryColor().getRed(),
                         country.getColors().getCountryColor().getGreen(),
                         country.getColors().getCountryColor().getBlue());
    }
}
