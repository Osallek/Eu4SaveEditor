package fr.osallek.eu4saveeditor.controller.object;

import fr.osallek.eu4parser.model.game.GameModifier;
import fr.osallek.eu4parser.model.save.country.SaveModifier;
import fr.osallek.eu4saveeditor.common.Copy;

import java.time.LocalDate;
import java.util.Objects;

public class Modifier extends Copy<Modifier> {

    private GameModifier gameModifier;

    private LocalDate date;

    private boolean changed;

    public Modifier(GameModifier gameModifier, LocalDate date) {
        this.gameModifier = gameModifier;
        this.date = date;
        this.changed = true;
    }

    public Modifier(SaveModifier saveModifier) {
        this.gameModifier = saveModifier.getModifier();
        this.date = saveModifier.getDate();
    }

    public Modifier(Modifier other) {
        this.gameModifier = other.gameModifier;
        this.date = other.date;
        this.changed = other.changed;
    }

    @Override
    public Modifier copy() {
        return new Modifier(this);
    }

    public GameModifier getModifier() {
        return gameModifier;
    }

    public void setModifier(GameModifier modifier) {
        if (!this.gameModifier.equals(modifier)) {
            this.gameModifier = modifier;
            this.changed = true;
        }
    }

    public LocalDate getDate() {
        return date;
    }

    public void setDate(LocalDate date) {
        if (!this.date.equals(date)) {
            this.date = date;
            this.changed = true;
        }
    }

    public boolean isChanged() {
        return changed;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }

        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        Modifier that = (Modifier) o;
        return Objects.equals(gameModifier, that.gameModifier);
    }

    @Override
    public int hashCode() {
        return Objects.hash(gameModifier);
    }
}
