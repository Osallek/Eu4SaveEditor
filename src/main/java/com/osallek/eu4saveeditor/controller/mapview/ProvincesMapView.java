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
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.image.Image;
import javafx.scene.layout.VBox;

import java.time.Duration;
import java.time.Instant;

public class ProvincesMapView extends AbstractMapView {

    private final ProvincePropertySheet sheet;

    private final Label titleLabel;

    private final Button submitButton;

    public ProvincesMapView(SaveProvince[][] provincesMap, Canvas canvas, VBox editPane, Save save,
                            ObservableList<Country> playableCountries, ObservableList<Culture> cultures,
                            ObservableList<Religion> religions, ObservableList<TradeGood> tradeGoods) {
        super(provincesMap, canvas, editPane, save, MapViewType.PROVINCES_MAP_VIEW, playableCountries, cultures, religions, tradeGoods);
        this.sheet = new ProvincePropertySheet(save, this.editPane, this.playableCountries, this.cultures, this.religions, this.tradeGoods);
        this.titleLabel = new Label();
        this.titleLabel.setVisible(false);
        this.submitButton = new Button("Submit");
        this.submitButton.setOnAction(e -> {
            this.sheet.validate(e);
            this.sheet.update(this.sheet.getProvince());
            this.titleLabel.setText(getTitle(this.sheet.getProvince()));
        });
        this.submitButton.disableProperty().bind(this.sheet.getValidationSupport().invalidProperty());
    }

    @Override
    public void draw() {
        this.canvas.getGraphicsContext2D()
                   .drawImage(new Image(this.save.getGame().getProvincesImage().toURI().toString()), 0, 0);
    }

    @Override
    public void onProvinceSelected(SaveProvince province) {
        if (this.selected) {
            this.titleLabel.setText(getTitle(province));
            this.sheet.update(province);
            this.submitButton.disableProperty().bind(this.sheet.getValidationSupport().invalidProperty());
        } else {
            this.editPane.getChildren().clear();

            this.titleLabel.setText(getTitle(province));
            this.titleLabel.setVisible(true);

            this.editPane.getChildren().add(this.titleLabel);

            this.editPane.getChildren().add(this.sheet.update(province));

            this.editPane.getChildren().add(this.submitButton);
        }
    }

    private String getTitle(SaveProvince saveProvince) {
        String title = ClausewitzUtils.removeQuotes(saveProvince.getName());

        if (saveProvince.getCountry() != null) {
            title += " (" + saveProvince.getCountry().getLocalizedName() + ")";
        }

        return title;
    }
}
