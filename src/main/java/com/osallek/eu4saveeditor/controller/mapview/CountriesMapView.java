package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.clausewitzparser.common.ClausewitzUtils;
import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4parser.model.save.province.SaveProvince;
import com.osallek.eu4saveeditor.controller.pane.CustomPropertySheet;
import javafx.beans.property.ReadOnlyBooleanWrapper;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.control.ToggleButton;
import javafx.scene.image.PixelWriter;
import javafx.scene.paint.Color;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class CountriesMapView extends AbstractMapView {

    private final ToggleButton countryButton;

    private final ToggleButton provinceButton;

    private final ProvincePropertySheet provinceSheet;

    private final CountryPropertySheet countrySheet;

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

        this.countrySheet = new CountryPropertySheet(this.mapViewContainer.getSave(),
                                                     this.mapViewContainer.getPlayableCountries(),
                                                     this.mapViewContainer.getCultures(),
                                                     this.mapViewContainer.getReligions());

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
                drawProvince(drawableProvince.getProvince() == null ? null : drawableProvince.getProvince().getId());
            }
        }
    }

    @Override
    public void onProvinceSelected(SaveProvince province) {
        this.provinceSheet.update(province);
        this.countrySheet.update(province.getOwner());

        if (Boolean.FALSE.equals(this.selected.getValue())) {
            this.mapViewContainer.addTabsSegmentedButtons(this.countryButton, this.provinceButton);

            if (this.countryButton.isSelected()) {
                selectCountryButton();
            } else if (this.provinceButton.isSelected()) {
                selectProvinceButton();
            }
        } else {
            this.provinceSheet.update(this.provinceSheet.getProvince());
            this.countrySheet.update(this.countrySheet.getCountry());
            this.mapViewContainer.updateTitle();

            if (this.countryButton.isSelected()) {
                if (this.countrySheet.getCountry() == null) {
                    this.mapViewContainer.bindSubmitButtonDisableProperty(new ReadOnlyBooleanWrapper(true));
                } else {
                    this.mapViewContainer.bindSubmitButtonDisableProperty(this.countrySheet.getValidationSupport().invalidProperty());
                }
            }
        }
    }

    @Override
    public void drawProvince(Integer provinceId) {
        GraphicsContext graphicsContext = this.mapViewContainer.getCanvas().getGraphicsContext2D();
        Color color = getOwnerColor(this.mapViewContainer.getDrawableProvinces().get(provinceId).getProvince());
        graphicsContext.setFill(color);
        this.mapViewContainer.getDrawableProvinces().get(provinceId).getRectangles()
                             .forEach(rectangle -> graphicsContext.fillRect(rectangle.x, rectangle.y, rectangle.width, rectangle.height));

        PixelWriter pixelWriter = graphicsContext.getPixelWriter();
        this.mapViewContainer.getDrawableProvinces().get(provinceId).getBorders()
                             .forEach(point2D -> pixelWriter.setColor((int) point2D.getX(), (int) point2D.getY(), Color.BLACK));
    }

    @Override
    public List<CustomPropertySheet> removeSheets() {
        List<CustomPropertySheet> sheets = new ArrayList<>();

        if (this.provinceSheet != null) {
            sheets.add(this.provinceSheet.getPropertySheet());
        }

        if (this.countrySheet != null) {
            sheets.add(this.countrySheet.getPropertySheet());
        }

        return sheets;
    }

    @Override
    public String updateTitle(SaveProvince selectedProvince) {
        if (this.countryButton.isSelected()) {
            return selectedProvince.getOwner() == null ? getTitle(selectedProvince) : selectedProvince.getOwner().getLocalizedName();
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
        this.mapViewContainer.addSheets(Collections.singletonList(this.countrySheet.getPropertySheet()));
        this.mapViewContainer.updateTitle();
        this.mapViewContainer.setSubmitButtonOnAction(e -> {
            this.countrySheet.validate(e);
            this.mapViewContainer.updateTitle();
        });

        if (this.countrySheet.getCountry() == null) {
            this.mapViewContainer.bindSubmitButtonDisableProperty(new ReadOnlyBooleanWrapper(true));
        } else {
            this.mapViewContainer.bindSubmitButtonDisableProperty(this.countrySheet.getValidationSupport().invalidProperty());
        }
    }

    private void selectProvinceButton() {
        this.mapViewContainer.removeSaveSheet();
        this.mapViewContainer.removeSheet(this.countrySheet.getPropertySheet());
        this.mapViewContainer.addSheets(Collections.singletonList(this.provinceSheet.getPropertySheet()));
        this.mapViewContainer.updateTitle();
        this.mapViewContainer.setSubmitButtonOnAction(e -> {
            this.provinceSheet.validate(e);
            this.provinceSheet.update(this.provinceSheet.getProvince());
            this.mapViewContainer.updateTitle();
        });

        this.mapViewContainer.bindSubmitButtonDisableProperty(this.provinceSheet.getValidationSupport().invalidProperty());
    }

    private String getTitle(SaveProvince saveProvince) {
        String title = ClausewitzUtils.removeQuotes(saveProvince.getName());

        if (saveProvince.getOwner() != null) {
            title += " (" + saveProvince.getController().getLocalizedName() + ")";
        }

        return title;
    }

    private Color getOwnerColor(SaveProvince province) {
        if (province == null) {
            return Color.BLACK;
        } else if (province.getOwner() != null) {
            return countryToMapColor(province.getOwner());
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
        return Color.rgb(country.getColors().getMapColor().getRed(),
                         country.getColors().getMapColor().getGreen(),
                         country.getColors().getMapColor().getBlue());
    }
}
