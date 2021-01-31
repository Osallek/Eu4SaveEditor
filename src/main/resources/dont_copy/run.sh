#!/usr/bin/env bash
java -jar @project.build.finalName@.jar -Xmx2G --add-opens javafx.graphics/javafx.scene=org.controlsfx.controls --add-opens javafx.controls/javafx.scene.control.skin=org.controlsfx.controls --add-exports javafx.base/com.sun.javafx.event=org.controlsfx.controls --add-exports org.controlsfx.controls/impl.org.controlsfx.autocompletion=fr.osallek.eu4saveeditor
