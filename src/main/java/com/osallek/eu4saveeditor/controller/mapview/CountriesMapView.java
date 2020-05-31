package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.clausewitzparser.common.ClausewitzUtils;
import com.osallek.eu4parser.model.game.Culture;
import com.osallek.eu4parser.model.game.Religion;
import com.osallek.eu4parser.model.game.TradeGood;
import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4parser.model.save.province.SaveProvince;
import javafx.collections.ObservableList;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.control.Button;
import javafx.scene.control.ToggleButton;
import javafx.scene.image.PixelWriter;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;

public class CountriesMapView extends AbstractMapView {

    private SaveProvince selectedProvince;

    private final ToggleButton countryButton;

    private final ToggleButton provinceButton;

    private final ProvincePropertySheet provinceSheet;

    private final Button submitButton;

    public CountriesMapView(SaveProvince[][] provincesMap, Canvas canvas, VBox editPane, Save save,
                            ObservableList<Country> playableCountries, ObservableList<Culture> cultures,
                            ObservableList<Religion> religions, ObservableList<TradeGood> tradeGoods) {
        super(provincesMap, canvas, editPane, save, MapViewType.COUNTRIES_MAP_VIEW, playableCountries, cultures, religions, tradeGoods);
        this.provinceSheet = new ProvincePropertySheet(save, this.playableCountries, this.cultures, this.religions, this.tradeGoods);
        this.provinceSheet.countryChangedProperty().addListener((observable, oldValue, newValue) -> {
            if (Boolean.FALSE.equals(oldValue) && Boolean.TRUE.equals(newValue)) {
                drawProvince(this.provinceSheet.getProvince().getId());
            }
        });

        this.countryButton = new ToggleButton(save.getGame().getLocalisation("TRIGGER_COUNTRY"));
        this.countryButton.selectedProperty().addListener((observable, oldValue, newValue) -> {
            if (Boolean.FALSE.equals(oldValue) && Boolean.TRUE.equals(newValue)) {
                selectCountryButton();
            }
        });
        this.countryButton.disableProperty().bind(this.countryButton.selectedProperty());

        this.provinceButton = new ToggleButton(save.getGame().getLocalisation("ANY_ALL_PROVINCE"));
        this.provinceButton.selectedProperty().addListener((observable, oldValue, newValue) -> {
            if (Boolean.FALSE.equals(oldValue) && Boolean.TRUE.equals(newValue)
                && !this.editPane.getChildren().contains(this.provinceSheet.getPropertySheet())) {
                selectProvinceButton();
            }
        });
        this.provinceButton.disableProperty().bind(this.provinceButton.selectedProperty());

        this.submitButton = new Button(save.getGame().getLocalisation("DONE"));
        this.submitButton.setOnAction(e -> {
            this.provinceSheet.validate(e);
            this.provinceSheet.update(this.provinceSheet.getProvince());
            this.titleLabel.setText(getTitle(this.provinceSheet.getProvince()));
        });
        this.submitButton.disableProperty().bind(this.provinceSheet.getValidationSupport().invalidProperty());
    }

    @Override
    public void draw() {
        for (DrawableProvince drawableProvince : this.drawableProvinces) {
            if (drawableProvince != null) {
                drawProvince(drawableProvince.getProvince().getId());
            }
        }
    }

    @Override
    public void onProvinceSelected(SaveProvince province) {
        this.selectedProvince = province;
        this.provinceSheet.update(province);
        updateTitle();

        if (this.selected) {
            this.submitButton.disableProperty().bind(this.provinceSheet.getValidationSupport().invalidProperty());
        } else {
            this.editPane.getChildren().clear();
            clearTabsSegmentedButton();

            this.tabsSegmentedButton.getButtons().add(this.countryButton);
            this.tabsSegmentedButton.getButtons().add(this.provinceButton);

            this.tabsSegmentedButton.getButtons()
                                    .forEach(toggleButton ->
                                                     toggleButton.prefWidthProperty()
                                                                 .bind(this.tabsSegmentedButton.widthProperty()
                                                                                               .divide(this.tabsSegmentedButton
                                                                                                               .getButtons()
                                                                                                               .size())));

            this.editPane.getChildren().add(this.tabsSegmentedButton);

            this.titleLabel.setVisible(true);

            this.editPane.getChildren().add(this.titleLabel);

            if (this.saveButton.isSelected()) {
                selectSaveButton();
            } else if (this.countryButton.isSelected()) {
                selectCountryButton();
            } else if (this.provinceButton.isSelected()) {
                selectProvinceButton();
            }

            this.editPane.getChildren().add(this.submitButton);
        }
    }

    @Override
    public void removeSheets() {
        if (this.provinceSheet != null) {
            this.editPane.getChildren().remove(this.provinceSheet.getPropertySheet());
        }
    }

    @Override
    protected void updateTitle() {
        if (this.saveButton.isSelected()) {
            this.titleLabel.setText(this.save.getName());
        } else if (this.countryButton.isSelected()) {
            this.titleLabel.setText(this.selectedProvince.getCountry().getLocalizedName());
        } else if (this.provinceButton.isSelected()) {
            this.titleLabel.setText(getTitle(this.selectedProvince));
        }
    }

    private void selectCountryButton() {
        this.editPane.getChildren().remove(this.provinceSheet.getPropertySheet());
        updateTitle();
    }

    private void selectProvinceButton() {
        this.editPane.getChildren().add(2, this.provinceSheet.getPropertySheet());
        updateTitle();
    }

    private void drawProvince(int provinceId) {
        GraphicsContext graphicsContext = this.canvas.getGraphicsContext2D();
        Color color = getOwnerColor(this.drawableProvinces[provinceId].getProvince());
        graphicsContext.setFill(color);
        this.drawableProvinces[provinceId].getRectangles()
                                          .forEach(rectangle -> graphicsContext.fillRect(rectangle.getX(), rectangle.getY(), rectangle
                                                  .getWidth(), rectangle.getHeight()));

        PixelWriter pixelWriter = graphicsContext.getPixelWriter();
        this.drawableProvinces[provinceId].getBorders()
                                          .forEach(point2D -> pixelWriter.setColor((int) point2D.getX(), (int) point2D.getY(), Color.BLACK));
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
            if (province.isOcean()) {
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
