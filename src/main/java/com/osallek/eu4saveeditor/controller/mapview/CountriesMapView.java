package com.osallek.eu4saveeditor.controller.mapview;

import com.osallek.clausewitzparser.common.ClausewitzUtils;
import com.osallek.eu4parser.model.save.Save;
import com.osallek.eu4parser.model.save.country.Country;
import com.osallek.eu4parser.model.save.province.Province;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;

public class CountriesMapView extends AbstractMapView {

    public CountriesMapView(Province[][] provincesMap, Canvas canvas, Save save) {
        super(provincesMap, canvas, save, MapViewType.COUNTRIES_MAP_VIEW);
    }

    @Override
    public void draw() {
        GraphicsContext gc = this.canvas.getGraphicsContext2D();
        for (int x = 0; x < this.provincesMap.length; x++) {
            for (int y = 0; y < this.provincesMap[x].length; y++) {
                Province province = this.provincesMap[x][y];
                int startY = y;
                while (y < this.provincesMap[x].length && this.provincesMap[x][y].equals(province)) {
                    y++;
                }

                gc.setFill(getOwnerColor(province));
                gc.fillRect(x, startY, 1, (double) y - startY);
            }
        }

        drawProvincesBorders();
    }

    @Override
    public void onProvinceSelected(Province province, VBox editPane) {
        editPane.setVisible(true);
        editPane.setMinWidth(300);
        editPane.setMaxWidth(600);
//        editPaneTitle.setText(ClausewitzUtils.removeQuotes(province.getName()) + " (" + ClausewitzUtils.removeQuotes(province.getOwner()) + ")");
    }

    private Color getOwnerColor(Province province) {
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
