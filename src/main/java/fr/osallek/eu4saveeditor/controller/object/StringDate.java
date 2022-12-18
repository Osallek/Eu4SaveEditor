package fr.osallek.eu4saveeditor.controller.object;

import fr.osallek.eu4saveeditor.common.Copy;
import java.time.LocalDate;
import java.util.Map;
import java.util.Objects;

public class StringDate extends Copy<StringDate> {

    private String name;

    private LocalDate date;

    private boolean changed;

    public StringDate(String name, LocalDate date) {
        this.name = name;
        this.date = date;
        this.changed = true;
    }

    public StringDate(StringDate other) {
        this.name = other.name;
        this.date = other.date;
        this.changed = other.changed;
    }

    public StringDate(Map.Entry<String, LocalDate> entry) {
        this.name = entry.getKey();
        this.date = entry.getValue();
        this.changed = false;
    }

    @Override
    public StringDate copy() {
        return new StringDate(this);
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        if (!this.name.equals(name)) {
            this.name = name;
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

        StringDate that = (StringDate) o;
        return Objects.equals(name, that.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }
}
