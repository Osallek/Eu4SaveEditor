package fr.osallek.eu4saveeditor.controller.mapview;

import fr.osallek.eu4parser.common.Eu4MapUtils;
import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4parser.model.save.province.SaveProvince;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import fr.osallek.eu4saveeditor.controller.converter.CountryStringConverter;
import fr.osallek.eu4saveeditor.controller.converter.ProvinceIdStringConverter;
import fr.osallek.eu4saveeditor.controller.pane.CustomPropertySheet;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import javafx.beans.property.ReadOnlyBooleanWrapper;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.control.ToggleButton;
import javafx.scene.paint.Color;
import org.springframework.context.MessageSource;

public class CountriesMapView extends AbstractMapView {

    private final Save save;

    private final ToggleButton countryButton;

    private final ToggleButton provinceButton;

    private final ProvincePropertySheet provinceSheet;

    private final CountryPropertySheet countrySheet;

    public CountriesMapView(MapViewContainer mapViewContainer, Save save, MessageSource messageSource) {
        super(mapViewContainer, MapViewType.COUNTRIES_MAP_VIEW);
        this.save = save;
        this.provinceSheet = new ProvincePropertySheet(messageSource, this.mapViewContainer.getSave(),
                                                       this.mapViewContainer.getPlayableCountries(),
                                                       this.mapViewContainer.getCultures(),
                                                       this.mapViewContainer.getPlayableReligions(),
                                                       this.mapViewContainer.getTradeGoods(),
                                                       this.mapViewContainer.getTradeNodes());
        this.provinceSheet.countryChangedProperty().addListener((observable, oldValue, newValue) -> {
            if (Boolean.FALSE.equals(oldValue) && Boolean.TRUE.equals(newValue)) {
                draw();
            }
        });

        this.countrySheet = new CountryPropertySheet(messageSource, this.mapViewContainer.getSave(),
                                                     this.mapViewContainer.getCountriesAlive(),
                                                     this.mapViewContainer.getCultures(),
                                                     this.mapViewContainer.getPlayableReligions());
        this.countrySheet.colorChangedProperty().addListener((observable, oldValue, newValue) -> {
            if (Boolean.FALSE.equals(oldValue) && Boolean.TRUE.equals(newValue)) {
                draw();
            }
        });

        this.countryButton = new ToggleButton(Eu4SaveEditorUtils.localize("TRIGGER_COUNTRY", this.mapViewContainer.getSave().getGame()));
        this.countryButton.selectedProperty().addListener((observable, oldValue, newValue) -> {
            if (Boolean.FALSE.equals(oldValue) && Boolean.TRUE.equals(newValue)) {
                selectCountryButton();
            }
        });
        this.countryButton.disableProperty().bind(this.countryButton.selectedProperty());

        this.provinceButton = new ToggleButton(Eu4SaveEditorUtils.localize("UNKNOWN_LOC", this.mapViewContainer.getSave().getGame()));
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
        GraphicsContext graphicsContext = this.mapViewContainer.getCanvas().getGraphicsContext2D();
        graphicsContext.drawImage(Eu4SaveEditorUtils.bufferedToView(Eu4MapUtils.generateMapPng(this.save.getGame(), province -> {
            Color color = getOwnerColor(this.save.getProvince(province.getId()));

            return new java.awt.Color((float) color.getRed(), (float) color.getGreen(), (float) color.getBlue(), (float) color.getOpacity());
        })).getImage(), 0, 0);
    }

    @Override
    public void onProvinceSelected(SaveProvince province) {
        if (!Objects.equals(this.provinceSheet.getProvince(), province)) {
            this.provinceSheet.update(province);
        }

        if (!Objects.equals(this.countrySheet.getCountry(), province.getOwner())) {
            this.countrySheet.update(province.getOwner());
        }

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
            return selectedProvince.getOwner() == null ? getTitle(selectedProvince)
                                                       :
                   (CountryStringConverter.INSTANCE.toString(selectedProvince.getOwner()) + " (" + selectedProvince.getOwner().getTag() + ")");
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
            this.countrySheet.validate();
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
            this.provinceSheet.validate();
            this.provinceSheet.update(this.provinceSheet.getProvince());
            this.mapViewContainer.updateTitle();
        });

        this.mapViewContainer.bindSubmitButtonDisableProperty(this.provinceSheet.getValidationSupport().invalidProperty());
    }

    private String getTitle(SaveProvince saveProvince) {
        String title = ProvinceIdStringConverter.INSTANCE.toString(saveProvince);

        if (saveProvince.getOwner() != null) {
            title += " - " + CountryStringConverter.INSTANCE.toString(saveProvince.getOwner());
        }

        return title;
    }

    private Color getOwnerColor(SaveProvince province) {
        if (province == null) {
            return Color.BLACK;
        } else if (province.getOwner() != null) {
            return Eu4SaveEditorUtils.countryToMapColor(province.getOwner());
        } else {
            if (province.isOcean() || province.isLake()) {
                return Color.rgb(Eu4MapUtils.OCEAN_COLOR.getRed(), Eu4MapUtils.OCEAN_COLOR.getGreen(), Eu4MapUtils.OCEAN_COLOR.getBlue());
            } else if (province.isImpassable()) {
                return Color.rgb(Eu4MapUtils.IMPASSABLE_COLOR.getRed(), Eu4MapUtils.IMPASSABLE_COLOR.getGreen(), Eu4MapUtils.IMPASSABLE_COLOR.getBlue());
            } else {
                return Color.rgb(Eu4MapUtils.EMPTY_COLOR.getRed(), Eu4MapUtils.EMPTY_COLOR.getGreen(), Eu4MapUtils.EMPTY_COLOR.getBlue());
            }
        }
    }
}
