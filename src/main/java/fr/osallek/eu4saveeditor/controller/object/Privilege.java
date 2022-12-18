package fr.osallek.eu4saveeditor.controller.object;

import fr.osallek.eu4parser.model.game.EstatePrivilege;
import fr.osallek.eu4parser.model.save.country.EstateInteraction;
import fr.osallek.eu4saveeditor.common.Copy;
import fr.osallek.eu4saveeditor.common.Eu4SaveEditorUtils;
import java.time.LocalDate;

public class Privilege extends Copy<Privilege> {

    private EstatePrivilege estatePrivilege;

    private LocalDate startDate;

    private boolean changed;

    public Privilege(EstatePrivilege privilege, LocalDate startDate) {
        this.estatePrivilege = privilege;
        this.startDate = startDate;
        this.changed = true;
    }

    public Privilege(EstateInteraction interaction) {
        this.estatePrivilege = interaction.getPrivilege();
        this.startDate = interaction.getDate();
        this.changed = false;
    }

    public Privilege(Privilege other) {
        this.estatePrivilege = other.estatePrivilege;
        this.startDate = other.startDate;
        this.changed = other.changed;
    }

    @Override
    public Privilege copy() {
        return new Privilege(this);
    }

    public EstatePrivilege getPrivilege() {
        return estatePrivilege;
    }

    public void setPrivilege(EstatePrivilege privilege) {
        if (!privilege.equals(this.estatePrivilege)) {
            this.estatePrivilege = privilege;
            this.changed = true;
        }
    }

    public LocalDate getStartDate() {
        return startDate;
    }

    public void setStartDate(LocalDate startDate) {
        if (!startDate.equals(this.startDate)) {
            this.startDate = startDate;
            this.changed = true;
        }
    }

    public boolean isChanged() {
        return changed;
    }

    @Override
    public String toString() {
        return Eu4SaveEditorUtils.localize(this.estatePrivilege.getName(), this.estatePrivilege.getGame());
    }
}
