package fr.osallek.eu4saveeditor.controller.mapview;

import java.util.function.Function;

public enum MapViewType {
    COUNTRIES_MAP_VIEW(CountriesMapView::new);

    private final Function<MapViewContainer, AbstractMapView> mapViewGetter;

    MapViewType(Function<MapViewContainer, AbstractMapView> mapViewGetter) {
        this.mapViewGetter = mapViewGetter;
    }

    public AbstractMapView getMapView(MapViewContainer mapViewContainer) {
        return this.mapViewGetter.apply(mapViewContainer);
    }
}
