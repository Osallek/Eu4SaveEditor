package fr.osallek.eu4saveeditor.controller.pane;

import javafx.geometry.Bounds;
import javafx.geometry.Point2D;
import javafx.geometry.Pos;
import javafx.scene.Group;
import javafx.scene.Node;
import javafx.scene.control.ScrollPane;
import javafx.scene.layout.VBox;

/**
 * Thanks to: Daniel Hári https://stackoverflow.com/questions/39827911/javafx-8-scaling-zooming-scrollpane-relative-to-mouse-position#answer-44314455
 */
public class ZoomableScrollPane extends ScrollPane {

    private static final double MAX_ZOOM = 3;

    private double scaleValue = 1;

    private final Node target;

    private final Node zoomNode;

    public ZoomableScrollPane(Node target) {
        super();
        this.target = target;
        this.zoomNode = new Group(target);
        setContent(outerNode(this.zoomNode));

        setPannable(true);
        setFitToHeight(true);
        setFitToWidth(true);

        updateScale();
    }

    private Node outerNode(Node node) {
        Node outerNode = centeredNode(node);
        outerNode.setOnScroll(e -> {
            if (e.isControlDown()) {
                e.consume();

                if (this.scaleValue >= MAX_ZOOM && e.getTextDeltaY() > 0) { //Don't zoom in too much
                    return;
                }

                if (zoomNode.getBoundsInParent().getMinY() > 0 && e.getTextDeltaY() < 0) { //Don't zoom out too much
                    return;
                }

                onScroll(e.getTextDeltaY(), e.getX(), e.getY());
            }
        });

        return outerNode;
    }

    private Node centeredNode(Node node) {
        VBox vBox = new VBox(node);
        vBox.setAlignment(Pos.CENTER);
        return vBox;
    }

    private void updateScale() {
        this.target.setScaleX(this.scaleValue);
        this.target.setScaleY(this.scaleValue);
    }

    private void onScroll(double wheelDelta, double x, double y) {
        double zoomIntensity = 0.02;
        double zoomFactor = Math.exp(wheelDelta * zoomIntensity);
        zoomFactor = Math.min(zoomFactor, MAX_ZOOM / this.scaleValue);

        Bounds innerBounds = this.zoomNode.getLayoutBounds();
        Bounds viewportBounds = getViewportBounds();

        // calculate pixel offsets from [0, 1] range
        double valX = this.getHvalue() * (innerBounds.getWidth() - viewportBounds.getWidth());
        double valY = this.getVvalue() * (innerBounds.getHeight() - viewportBounds.getHeight());

        this.scaleValue = Math.min(this.scaleValue * zoomFactor, MAX_ZOOM); //For division precision
        updateScale();
        this.layout(); // refresh ScrollPane scroll positions & target bounds

        // convert target coordinates to zoomTarget coordinates
        Point2D posInZoomTarget = this.target.parentToLocal(this.zoomNode.parentToLocal(x, y));

        // calculate adjustment of scroll position (pixels)
        Point2D adjustment = this.target.getLocalToParentTransform()
                                        .deltaTransform(posInZoomTarget.multiply(zoomFactor - 1));

        // convert back to [0, 1] range
        // (too large/small values are automatically corrected by ScrollPane)
        Bounds updatedInnerBounds = this.zoomNode.getBoundsInLocal();
        this.setHvalue((valX + adjustment.getX()) / (updatedInnerBounds.getWidth() - viewportBounds.getWidth()));
        this.setVvalue((valY + adjustment.getY()) / (updatedInnerBounds.getHeight() - viewportBounds.getHeight()));
    }
}
