package fr.osallek.eu4saveeditor.controller.object;

import fr.osallek.eu4parser.model.game.Policy;
import fr.osallek.eu4saveeditor.common.Copy;

import java.time.LocalDate;
import java.util.Objects;

public class ActivePolicy extends Copy<ActivePolicy> {

    private Policy policy;

    private LocalDate date;

    private boolean changed;

    public ActivePolicy(Policy policy, LocalDate date) {
        this.policy = policy;
        this.date = date;
        this.changed = true;
    }

    public ActivePolicy(fr.osallek.eu4parser.model.save.country.ActivePolicy activePolicy) {
        this.policy = activePolicy.getPolicy();
        this.date = activePolicy.getDate();
    }

    public ActivePolicy(ActivePolicy other) {
        this.policy = other.policy;
        this.date = other.date;
        this.changed = other.changed;
    }

    @Override
    public ActivePolicy copy() {
        return new ActivePolicy(this);
    }

    public Policy getPolicy() {
        return policy;
    }

    public void setPolicy(Policy policy) {
        if (!this.policy.equals(policy)) {
            this.policy = policy;
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

        ActivePolicy that = (ActivePolicy) o;
        return Objects.equals(policy, that.policy);
    }

    @Override
    public int hashCode() {
        return Objects.hash(policy);
    }
}
