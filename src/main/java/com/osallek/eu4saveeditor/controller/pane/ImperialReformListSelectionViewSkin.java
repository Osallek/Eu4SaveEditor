package com.osallek.eu4saveeditor.controller.pane;

import com.osallek.eu4parser.model.game.ImperialReform;
import com.osallek.eu4saveeditor.controller.control.ListSelectionViewImperialReform;
import impl.org.controlsfx.skin.ListActionViewSkin;
import javafx.beans.InvalidationListener;
import javafx.beans.binding.Bindings;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.geometry.Orientation;
import javafx.geometry.Pos;
import javafx.geometry.Side;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ListView;
import javafx.scene.control.Separator;
import javafx.scene.control.SkinBase;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.RowConstraints;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import org.controlsfx.control.ListActionView;
import org.controlsfx.control.ListSelectionView;
import org.controlsfx.control.action.Action;
import org.controlsfx.control.action.ActionUtils;

import java.util.ArrayList;
import java.util.List;

import static java.util.Objects.requireNonNull;
import static java.util.stream.Collectors.toCollection;
import static javafx.scene.control.SelectionMode.MULTIPLE;
import static javafx.scene.input.MouseEvent.MOUSE_CLICKED;

public class ImperialReformListSelectionViewSkin extends SkinBase<ListSelectionView<ImperialReform>> {

    private GridPane gridPane;
    private ListView<ImperialReform> sourceListView;
    private ListView<ImperialReform> targetListView;
    private ListActionView<ImperialReform> sourceListActionView;
    private ListActionView<ImperialReform> targetListActionView;

    public ImperialReformListSelectionViewSkin(ListSelectionViewImperialReform view) {
        super(view);

        sourceListView = requireNonNull(createSourceListView(), "source list view can not be null");
        sourceListView.setId("source-list-view");
        sourceListActionView = createListActionView(sourceListView);
        Bindings.bindContent(sourceListActionView.getActions(), view.getSourceActions());
        Bindings.bindContentBidirectional(sourceListActionView.getItems(), view.getSourceItems());

        targetListView = requireNonNull(createTargetListView(), "target list view can not be null");
        targetListView.setId("target-list-view");
        targetListActionView = createListActionView(targetListView);
        Bindings.bindContent(targetListActionView.getActions(), view.getTargetActions());
        Bindings.bindContentBidirectional(targetListActionView.getItems(), view.getTargetItems());

        sourceListActionView.cellFactoryProperty().bind(view.cellFactoryProperty());
        targetListActionView.cellFactoryProperty().bind(view.cellFactoryProperty());

        gridPane = createGridPane();
        getChildren().add(gridPane);

        InvalidationListener updateListener = o -> updateView();

        view.sourceHeaderProperty().addListener(updateListener);
        view.sourceFooterProperty().addListener(updateListener);
        view.targetHeaderProperty().addListener(updateListener);
        view.targetFooterProperty().addListener(updateListener);
        view.getActions().addListener(updateListener);

        updateView();

        getSourceListView().addEventHandler(MOUSE_CLICKED, event -> {
            if (event.getButton() == MouseButton.PRIMARY && event.getClickCount() == 2) {
                view.moveToTarget(getSourceListView());
            }
        });

        getTargetListView().addEventHandler(MOUSE_CLICKED, event -> {
            if (event.getButton() == MouseButton.PRIMARY && event.getClickCount() == 2) {
                view.moveToSource(getTargetListView());
            }
        });

        view.orientationProperty().addListener(observable -> updateView());
    }

    private GridPane createGridPane() {
        GridPane gridPane = new GridPane();
        gridPane.getStyleClass().add("grid-pane");
        return gridPane;
    }

    // Constraints used when view's orientation is HORIZONTAL
    private void setHorizontalViewConstraints() {
        gridPane.getColumnConstraints().clear();
        gridPane.getRowConstraints().clear();

        ColumnConstraints col1 = new ColumnConstraints();

        col1.setFillWidth(true);
        col1.setHgrow(Priority.ALWAYS);
        col1.setMaxWidth(Double.MAX_VALUE);
        col1.setPrefWidth(200);

        ColumnConstraints col2 = new ColumnConstraints();
        col2.setFillWidth(true);
        col2.setHgrow(Priority.NEVER);

        ColumnConstraints col3 = new ColumnConstraints();
        col3.setFillWidth(true);
        col3.setHgrow(Priority.ALWAYS);
        col3.setMaxWidth(Double.MAX_VALUE);
        col3.setPrefWidth(200);

        gridPane.getColumnConstraints().addAll(col1, col2, col3);

        RowConstraints row1 = new RowConstraints();
        row1.setFillHeight(true);
        row1.setVgrow(Priority.NEVER);

        RowConstraints row2 = new RowConstraints();
        row2.setMaxHeight(Double.MAX_VALUE);
        row2.setPrefHeight(200);
        row2.setVgrow(Priority.ALWAYS);

        RowConstraints row3 = new RowConstraints();
        row3.setFillHeight(true);
        row3.setVgrow(Priority.NEVER);

        gridPane.getRowConstraints().addAll(row1, row2, row3);
    }

    // Constraints used when view's orientation is VERTICAL
    private void setVerticalViewConstraints() {
        gridPane.getColumnConstraints().clear();
        gridPane.getRowConstraints().clear();

        ColumnConstraints col1 = new ColumnConstraints();

        col1.setFillWidth(true);
        col1.setHgrow(Priority.ALWAYS);
        col1.setMaxWidth(Double.MAX_VALUE);
        col1.setPrefWidth(200);

        gridPane.getColumnConstraints().addAll(col1);

        RowConstraints row1 = new RowConstraints();
        row1.setFillHeight(true);
        row1.setVgrow(Priority.NEVER);

        RowConstraints row2 = new RowConstraints();
        row2.setMaxHeight(Double.MAX_VALUE);
        row2.setPrefHeight(200);
        row2.setVgrow(Priority.ALWAYS);

        RowConstraints row3 = new RowConstraints();
        row3.setFillHeight(true);
        row3.setVgrow(Priority.NEVER);

        RowConstraints row4 = new RowConstraints();
        row4.setFillHeight(true);
        row4.setVgrow(Priority.NEVER);

        RowConstraints row5 = new RowConstraints();
        row5.setFillHeight(true);
        row5.setVgrow(Priority.NEVER);

        RowConstraints row6 = new RowConstraints();
        row6.setMaxHeight(Double.MAX_VALUE);
        row6.setPrefHeight(200);
        row6.setVgrow(Priority.ALWAYS);

        RowConstraints row7 = new RowConstraints();
        row7.setFillHeight(true);
        row7.setVgrow(Priority.NEVER);

        gridPane.getRowConstraints().addAll(row1, row2, row3, row4, row5, row6, row7);
    }

    // Used when view's orientation is HORIZONTAL
    private VBox createVerticalButtonBox() {
        VBox box = new VBox(5);
        box.setFillWidth(true);
        box.getChildren().addAll(createButtonsFromActions());
        return box;
    }

    // Used when view's orientation is VERTICAL
    private HBox createHorizontalButtonBox() {
        HBox box = new HBox(5);
        box.setFillHeight(true);
        box.getChildren().addAll(createButtonsFromActions());
        return box;
    }

    private void updateView() {
        gridPane.getChildren().clear();

        Node sourceHeader = getSkinnable().getSourceHeader();
        Node targetHeader = getSkinnable().getTargetHeader();
        Node sourceFooter = getSkinnable().getSourceFooter();
        Node targetFooter = getSkinnable().getTargetFooter();

        StackPane stackPane = new StackPane();
        stackPane.setAlignment(Pos.CENTER);

        Orientation orientation = getSkinnable().getOrientation();

        if (orientation == Orientation.HORIZONTAL) {
            setHorizontalViewConstraints();

            if (sourceHeader != null) {
                gridPane.add(sourceHeader, 0, 0);
            }

            if (targetHeader != null) {
                gridPane.add(targetHeader, 2, 0);
            }

            if (sourceListActionView != null) {
                sourceListActionView.setSide(Side.LEFT);
                gridPane.add(sourceListActionView, 0, 1);
            }

            if (targetListActionView != null) {
                targetListActionView.setSide(Side.RIGHT);
                gridPane.add(targetListActionView, 2, 1);
            }

            if (sourceFooter != null) {
                gridPane.add(sourceFooter, 0, 2);
            }

            if (targetFooter != null) {
                gridPane.add(targetFooter, 2, 2);
            }

            stackPane.getChildren().add(createVerticalButtonBox());
            gridPane.add(stackPane, 1, 1);
        } else {
            setVerticalViewConstraints();

            if (sourceHeader != null) {
                gridPane.add(sourceHeader, 0, 0);
            }

            if (targetHeader != null) {
                gridPane.add(targetHeader, 0, 4);
            }

            if (sourceListActionView != null) {
                sourceListActionView.setSide(Side.RIGHT);
                gridPane.add(sourceListActionView, 0, 1);
            }

            if (targetListActionView != null) {
                targetListActionView.setSide(Side.RIGHT);
                gridPane.add(targetListActionView, 0, 5);
            }

            if (sourceFooter != null) {
                gridPane.add(sourceFooter, 0, 2);
            }

            if (targetFooter != null) {
                gridPane.add(targetFooter, 0, 6);
            }

            stackPane.getChildren().add(createHorizontalButtonBox());
            gridPane.add(stackPane, 0, 3);
        }
    }

    private void moveToTarget() {
        move(getSourceListView(), getTargetListView());
        getSourceListView().getSelectionModel().clearSelection();
    }

    private void moveToSource() {
        move(getTargetListView(), getSourceListView());
        getTargetListView().getSelectionModel().clearSelection();
    }

    private void move(ListView<ImperialReform> viewA, ListView<ImperialReform> viewB) {
        List<ImperialReform> selectedItems = new ArrayList<>(viewA.getSelectionModel()
                                                                  .getSelectedItems());
        move(viewA, viewB, selectedItems);
    }

    private void move(ListView<ImperialReform> viewA, ListView<ImperialReform> viewB, List<ImperialReform> items) {
        viewA.getItems().removeAll(items);
        viewB.getItems().addAll(items);
    }

    private ObservableList<Node> createButtonsFromActions() {
        return getSkinnable().getActions().stream()
                             .peek(this::initializeListSelectionAction)
                             .map(this::createActionNode)
                             .collect(toCollection(FXCollections::observableArrayList));
    }

    private void initializeListSelectionAction(Action action) {
        if (action instanceof ListSelectionView.ListSelectionAction) {
            ((ListSelectionView.ListSelectionAction<ImperialReform>) action).initialize(sourceListView, targetListView);
        }
    }

    private Node createActionNode(Action action) {
        if (action == ActionUtils.ACTION_SEPARATOR) {
            return new Separator();
        } else if (action == ActionUtils.ACTION_SPAN) {
            Pane span = new Pane();
            HBox.setHgrow(span, Priority.ALWAYS);
            VBox.setVgrow(span, Priority.ALWAYS);
            return span;
        }
        return createActionButton(action);
    }

    private Button createActionButton(Action action) {
        Button button = ActionUtils.createButton(action);
        button.setMaxWidth(Double.MAX_VALUE);
        if (action.getAccelerator() != null) {
            getSkinnable().getScene().getAccelerators().put(action.getAccelerator(), button::fire);
        }
        return button;
    }

    /**
     * Returns the source list view (shown on the left-hand side).
     *
     * @return the source list view
     */
    public final ListView<ImperialReform> getSourceListView() {
        return sourceListView;
    }

    /**
     * Returns the target list view (shown on the right-hand side).
     *
     * @return the target list view
     */
    public final ListView<ImperialReform> getTargetListView() {
        return targetListView;
    }

    /**
     * Creates the {@link ListView} instance used on the left-hand side as the source list. This method can be
     * overridden to provide a customized list view control.
     *
     * @return the source list view
     */
    protected ListView<ImperialReform> createSourceListView() {
        return createListView();
    }

    /**
     * Creates the {@link ListView} instance used on the right-hand side as the target list. This method can be
     * overridden to provide a customized list view control.
     *
     * @return the target list view
     */
    protected ListView<ImperialReform> createTargetListView() {
        return createListView();
    }

    private ListView<ImperialReform> createListView() {
        ListView<ImperialReform> view = new ListView<>();
        view.getSelectionModel().setSelectionMode(MULTIPLE);
        return view;
    }

    private ListActionView<ImperialReform> createListActionView(ListView<ImperialReform> listView) {
        ListActionView<ImperialReform> listActionView = new ListActionView<>();
        listActionView.setSkin(new ListActionViewSkin<ImperialReform>(listActionView) {
            @Override
            public ListView<ImperialReform> createListView() {
                return listView;
            }
        });
        return listActionView;
    }

}
