package fr.osallek.eu4saveeditor.controller.object;

import fr.osallek.eu4parser.model.save.Save;
import fr.osallek.eu4parser.model.save.changeprices.ChangePrice;
import fr.osallek.eu4saveeditor.common.Copy;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import java.time.LocalDate;
import java.util.Objects;

public class PriceChange extends Copy<PriceChange> {

    private String name;

    private LocalDate expiryDate;

    private Integer value;

    private boolean changed;

    public PriceChange(ChangePrice changePrice, Save save) {
        this.name = Eu4SaveEditorUtils.localize(changePrice.getKey(), save.getGame());
        this.expiryDate = changePrice.getExpiryDate();
        this.value = changePrice.getValue();
        this.changed = true;
    }

    public PriceChange(String name, LocalDate expiryDate, Integer value) {
        this.name = name;
        this.expiryDate = expiryDate;
        this.value = value;
        this.changed = true;
    }

    public PriceChange(PriceChange other) {
        this.name = other.name;
        this.expiryDate = other.expiryDate;
        this.value = other.value;
        this.changed = other.changed;
    }

    @Override
    public PriceChange copy() {
        return new PriceChange(this);
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

    public LocalDate getExpiryDate() {
        return expiryDate;
    }

    public void setExpiryDate(LocalDate expiryDate) {
        if (!this.expiryDate.equals(expiryDate)) {
            this.expiryDate = expiryDate;
            this.changed = true;
        }
    }

    public Integer getValue() {
        return value;
    }

    public void setValue(Integer value) {
        if (!this.value.equals(value)) {
            this.value = value;
            this.changed = true;
        }
    }

    public boolean isChanged() {
        return changed;
    }

    public ChangePrice toChangePrice() {
        return new ChangePrice(this.name, this.value, this.expiryDate);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }

        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        PriceChange that = (PriceChange) o;
        return Objects.equals(name, that.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }
}
