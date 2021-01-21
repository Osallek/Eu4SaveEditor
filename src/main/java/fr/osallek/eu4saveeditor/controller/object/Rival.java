package fr.osallek.eu4saveeditor.controller.object;

import fr.osallek.eu4parser.model.save.country.Country;

import java.time.LocalDate;
import java.util.Objects;

public class Rival {

    private final Country source;

    private Country target;

    private LocalDate date;

    private boolean changed;

    public Rival(Country source, Country target, LocalDate date) {
        this.source = source;
        this.target = target;
        this.date = date;
        this.changed = true;
    }

    public Rival(Country country, fr.osallek.eu4parser.model.save.country.Rival rival) {
        this.source = country;
        this.target = rival.getRival();
        this.date = rival.getDate();
    }

    public Rival(Rival other) {
        this.source = other.source;
        this.target = other.target;
        this.date = other.date;
        this.changed = other.changed;
    }

    public Country getSource() {
        return source;
    }

    public Country getTarget() {
        return target;
    }

    public void setTarget(Country target) {
        if (!this.target.equals(target)) {
            this.target = target;
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

        Rival that = (Rival) o;
        return Objects.equals(source, that.source) &&
               Objects.equals(target, that.target);
    }

    @Override
    public int hashCode() {
        return Objects.hash(source, target);
    }
}
