module fr.osallek.eu4saveeditor {
    requires fr.osallek.eu4parser;
    requires fr.osallek.clausewitzparser;
    requires org.slf4j;
    requires java.desktop;
    requires javafx.base;
    requires javafx.controls;
    requires org.controlsfx.controls;
    requires javafx.swing;
    requires javafx.fxml;
    requires org.apache.commons.io;
    requires org.apache.logging.log4j;
    requires org.apache.logging.log4j.core;
    requires org.apache.logging.log4j.slf4j;

    exports fr.osallek.eu4saveeditor;
    exports fr.osallek.eu4saveeditor.common;
    exports fr.osallek.eu4saveeditor.controller;
    exports fr.osallek.eu4saveeditor.controller.control;
    exports fr.osallek.eu4saveeditor.controller.converter;
    exports fr.osallek.eu4saveeditor.controller.mapview;
    exports fr.osallek.eu4saveeditor.controller.object;
    exports fr.osallek.eu4saveeditor.controller.pane;
    exports fr.osallek.eu4saveeditor.controller.propertyeditor;
    exports fr.osallek.eu4saveeditor.controller.propertyeditor.item;
    exports fr.osallek.eu4saveeditor.controller.validator;
    exports fr.osallek.eu4saveeditor.i18n;
    exports fr.osallek.eu4saveeditor.imagereader;

    opens fr.osallek.eu4saveeditor.controller to javafx.fxml;
}
