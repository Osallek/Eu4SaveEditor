package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.eu4parser.model.game.Culture;
import com.osallek.eu4parser.model.game.Religion;
import com.osallek.eu4parser.model.game.TradeGood;
import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4parser.model.save.province.SaveProvince;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.canvas.Canvas;
import javafx.scene.image.Image;
import javafx.scene.layout.VBox;

public class ProvincesMapView extends AbstractMapView {

    private final ObservableList<Country> playableCountries;

    private final ObservableList<Culture> cultures;

    private final ObservableList<Religion> religions;

    private final ObservableList<TradeGood> tradeGoods;

    public ProvincesMapView(SaveProvince[][] provincesMap, Canvas canvas, Save save) {
        super(provincesMap, canvas, save, MapViewType.PROVINCES_MAP_VIEW);
        this.playableCountries = FXCollections.observableArrayList(provincesMap[0][0].getSave().getPlayableCountries());
        this.cultures = FXCollections.observableArrayList(provincesMap[0][0].getSave().getGame().getCultures());
        this.religions = FXCollections.observableArrayList(provincesMap[0][0].getSave().getGame().getReligions());
        this.tradeGoods = FXCollections.observableArrayList(provincesMap[0][0].getSave().getGame().getTradeGoods());
    }

    @Override
    public void draw() {
        this.canvas.getGraphicsContext2D()
                   .drawImage(new Image(this.save.getGame().getProvincesImage().toURI().toString()), 0, 0);
    }

    @Override
    public void onProvinceSelected(SaveProvince province, VBox editPane) {
        editPane.setVisible(true);
        editPane.setMinWidth(300);
        editPane.setPrefWidth(600);
        editPane.setMaxWidth(600);
        editPane.getChildren().clear();
        editPane.getChildren()
                .add(new ProvincePropertySheet(editPane, province, this.playableCountries, this.cultures, this.religions,
                                               this.tradeGoods));
    }
}
