package fr.osallek.eu4saveeditor.controller.control;

import fr.osallek.eu4parser.model.game.Game;
import fr.osallek.eu4parser.model.game.ImperialReform;
import fr.osallek.eu4saveeditor.controller.converter.ImperialReformStringCellFactory;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.Supplier;
import javafx.beans.binding.Bindings;
import javafx.collections.ObservableList;
import javafx.geometry.Orientation;
import javafx.scene.control.ListView;
import javafx.scene.input.KeyCombination;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;

public class ListSelectionViewImperialReform extends CustomListSelectionView<ImperialReform> {

    private final ObservableList<ImperialReform> choices;

    private final ObservableList<ImperialReform> selected;

    public ListSelectionViewImperialReform(ObservableList<ImperialReform> choices, ObservableList<ImperialReform> selected, Game game) {
        super(choices, selected, new ImperialReformStringCellFactory(game), 750, 600);
        this.choices = choices;
        this.selected = selected;
        setSourceItems(this.choices.sorted());
        setTargetItems(this.selected.sorted());

        this.getActions().setAll(new MoveToTarget(), new MoveToTargetAll(), new MoveToSource(), new MoveToSourceAll());
    }

    @Override
    public void onReset(Supplier<Collection<ImperialReform>> sourceSupplier, Supplier<Collection<ImperialReform>> targetSupplier) {
        this.choices.setAll(sourceSupplier.get());
        this.selected.setAll(targetSupplier.get());
    }

    private static Glyph getGlyph(FontAwesome.Glyph angleDoubleDown) {
        return new FontAwesome().create(angleDoubleDown);
    }

    /**
     * Action use to move the selected items from the source list view to the target list view.
     */
    public class MoveToTarget extends ListSelectionAction<ImperialReform> {

        public MoveToTarget() {
            super(getGlyph(FontAwesome.Glyph.ANGLE_RIGHT));
            getStyleClass().add("move-to-target-button");
            setAccelerator(KeyCombination.keyCombination("CTRL+RIGHT"));
            graphicProperty().bind(Bindings.createObjectBinding(() -> (getOrientation() == Orientation.HORIZONTAL ?
                                                                       getGlyph(FontAwesome.Glyph.ANGLE_RIGHT) :
                                                                       getGlyph(FontAwesome.Glyph.ANGLE_DOWN)), orientationProperty()));
        }

        @Override
        public void initialize(ListView<ImperialReform> sourceListView, ListView<ImperialReform> targetListView) {
            disabledProperty().bind(Bindings.isEmpty(sourceListView.getSelectionModel().getSelectedItems()));
            setEventHandler(ae -> moveToTarget(sourceListView));
        }
    }


    /**
     * Action use to move all the items from the source list view to the target list view.
     */
    public class MoveToTargetAll extends ListSelectionAction<ImperialReform> {

        public MoveToTargetAll() {
            super(getGlyph(FontAwesome.Glyph.ANGLE_DOUBLE_RIGHT));
            getStyleClass().add("move-to-target-all-button");
            graphicProperty().bind(Bindings.createObjectBinding(() -> (getOrientation() == Orientation.HORIZONTAL ?
                                                                       getGlyph(FontAwesome.Glyph.ANGLE_DOUBLE_RIGHT) :
                                                                       getGlyph(FontAwesome.Glyph.ANGLE_DOUBLE_DOWN)), orientationProperty()));
            setAccelerator(KeyCombination.keyCombination("CTRL+SHIFT+RIGHT"));
        }

        @Override
        public void initialize(ListView<ImperialReform> sourceListView, ListView<ImperialReform> targetListView) {
            disabledProperty().bind(Bindings.isEmpty(sourceListView.getItems()));
            setEventHandler(ae -> moveToTargetAll(sourceListView));
        }
    }


    /**
     * Action use to move the selected items from the target list view to the source list view.
     */
    public class MoveToSource extends ListSelectionAction<ImperialReform> {

        public MoveToSource() {
            super(getGlyph(FontAwesome.Glyph.ANGLE_LEFT));
            getStyleClass().add("move-to-source-button");
            graphicProperty().bind(Bindings.createObjectBinding(() -> (getOrientation() == Orientation.HORIZONTAL ?
                                                                       getGlyph(FontAwesome.Glyph.ANGLE_LEFT) :
                                                                       getGlyph(FontAwesome.Glyph.ANGLE_UP)), orientationProperty()));
            setAccelerator(KeyCombination.keyCombination("CTRL+LEFT"));
        }

        @Override
        public void initialize(ListView<ImperialReform> sourceListView, ListView<ImperialReform> targetListView) {
            disabledProperty().bind(Bindings.isEmpty(targetListView.getSelectionModel().getSelectedItems()));
            setEventHandler(ae -> moveToSource(targetListView));
        }
    }


    /**
     * Action use to all the items from the target list view to the source list view.
     */
    public class MoveToSourceAll extends ListSelectionAction<ImperialReform> {

        public MoveToSourceAll() {
            super(getGlyph(FontAwesome.Glyph.ANGLE_DOUBLE_LEFT));
            getStyleClass().add("move-to-source-all-button");
            graphicProperty().bind(Bindings.createObjectBinding(() -> (getOrientation() == Orientation.HORIZONTAL ?
                                                                       getGlyph(FontAwesome.Glyph.ANGLE_DOUBLE_LEFT) :
                                                                       getGlyph(FontAwesome.Glyph.ANGLE_DOUBLE_UP)), orientationProperty()));
            setAccelerator(KeyCombination.keyCombination("CTRL+SHIFT+LEFT"));
        }

        @Override
        public void initialize(ListView<ImperialReform> sourceListView, ListView<ImperialReform> targetListView) {
            disabledProperty().bind(Bindings.isEmpty(targetListView.getItems()));
            setEventHandler(ae -> moveToSourceAll(targetListView));
        }
    }

    public void moveToTarget(ListView<ImperialReform> sourceListView) {
        List<ImperialReform> toMove = new ArrayList<>(sourceListView.getSelectionModel().getSelectedItems());
        int index = getSourceItems().indexOf(toMove.get(0));
        toMove = new ArrayList<>(getSourceItems().subList(0, index + 1));

        move(choices, selected, toMove);
        sourceListView.getSelectionModel().clearSelection();
    }

    public void moveToTargetAll(ListView<ImperialReform> sourceListView) {
        move(choices, selected, new ArrayList<>(getSourceItems()));
        sourceListView.getSelectionModel().clearSelection();
    }

    public void moveToSource(ListView<ImperialReform> targetListView) {
        List<ImperialReform> toMove = new ArrayList<>(targetListView.getSelectionModel().getSelectedItems());
        int index = getTargetItems().indexOf(toMove.get(0));
        toMove = new ArrayList<>(getTargetItems().subList(index, selected.size()));

        move(selected, choices, toMove);
        targetListView.getSelectionModel().clearSelection();
    }

    public void moveToSourceAll(ListView<ImperialReform> targetListView) {
        move(selected, choices, new ArrayList<>(getTargetItems()));
        targetListView.getSelectionModel().clearSelection();
    }

    private void move(ObservableList<ImperialReform> source, ObservableList<ImperialReform> target, List<ImperialReform> items) {
        source.removeAll(items);
        target.addAll(items);
    }
}
