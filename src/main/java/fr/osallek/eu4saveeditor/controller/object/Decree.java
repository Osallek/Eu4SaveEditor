package fr.osallek.eu4saveeditor.controller.object;

import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4saveeditor.common.Copy;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import java.util.Objects;

public class Decree extends Copy<Decree> {

    private String decree;

    private String name;

    private final Save save;

    private boolean changed;

    public Decree(fr.osallek.eu4parser.model.game.Decree decree, Save save) {
        this.decree = decree.getName();
        this.name = Eu4SaveEditorUtils.localize(decree.getName(), save.getGame());
        this.changed = false;
        this.save = save;
    }

    public Decree(Save save) {
        this.changed = false;
        this.save = save;
    }

    public Decree(Decree other) {
        this.decree = other.decree;
        this.changed = other.changed;
        this.save = other.save;
        this.name = other.name;
    }

    @Override
    public Decree copy() {
        return new Decree(this);
    }

    public String getDecree() {
        return decree;
    }

    public void setDecree(fr.osallek.eu4parser.model.game.Decree decree) {
        if (!decree.getName().equals(this.decree)) {
            this.decree = decree.getName();
            this.name = Eu4SaveEditorUtils.localize(this.decree, this.save.getGame());
            this.changed = true;
        }
    }

    public boolean isChanged() {
        return changed;
    }

    @Override
    public String toString() {
        return this.name;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }

        if (!(o instanceof Decree that)) {
            return false;
        }

        return Objects.equals(decree, that.decree);
    }

    @Override
    public int hashCode() {
        return Objects.hash(decree);
    }
}
