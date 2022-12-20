package fr.osallek.eu4saveeditor.controller.object;

import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4saveeditor.common.Copy;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import java.util.Objects;

public class GoldenBull extends Copy<GoldenBull> {

    private String goldenBull;

    private String name;

    private Save save;

    private boolean changed;

    public GoldenBull(fr.osallek.eu4parser.model.game.GoldenBull goldenBull, Save save) {
        if (goldenBull != null) {
            this.goldenBull = goldenBull.getName();
            this.name = Eu4SaveEditorUtils.localize(goldenBull.getName(), save.getGame());
        }

        this.changed = false;
        this.save = save;
    }

    public GoldenBull(Save save) {
        this.changed = false;
        this.save = save;
    }

    public GoldenBull(GoldenBull other) {
        this.goldenBull = other.goldenBull;
        this.changed = other.changed;
        this.save = other.save;
        this.name = other.name;
    }

    @Override
    public GoldenBull copy() {
        return new GoldenBull(this);
    }

    public String getGoldenBull() {
        return goldenBull;
    }

    public void setGoldenBull(fr.osallek.eu4parser.model.game.GoldenBull goldenBull) {
        if (!goldenBull.getName().equals(this.goldenBull)) {
            this.goldenBull = goldenBull.getName();
            this.name = Eu4SaveEditorUtils.localize(this.goldenBull, this.save.getGame());
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

        if (!(o instanceof GoldenBull)) {
            return false;
        }

        GoldenBull that = (GoldenBull) o;
        return Objects.equals(goldenBull, that.goldenBull);
    }

    @Override
    public int hashCode() {
        return Objects.hash(goldenBull);
    }
}
