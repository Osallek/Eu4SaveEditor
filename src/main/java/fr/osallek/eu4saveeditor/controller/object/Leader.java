package fr.osallek.eu4saveeditor.controller.object;

import fr.osallek.clausewitzparser.common.ClausewitzUtils;
import fr.osallek.eu4parser.model.game.LeaderPersonality;
import fr.osallek.eu4parser.model.save.country.Country;
import fr.osallek.eu4parser.model.save.country.LeaderType;
import fr.osallek.eu4saveeditor.common.Copy;

import java.time.LocalDate;

public class Leader extends Copy<Leader> {

    private final Country country;

    private final Integer id;

    private String name;

    private LeaderType type;

    private int manuever;

    private int fire;

    private int shock;

    private int siege;

    private LeaderPersonality personality;

    private LocalDate birthDate;

    private boolean changed;

    public Leader(fr.osallek.eu4parser.model.save.country.Leader leader) {
        this.country = leader.getCountry();
        this.id = leader.getId().getId();
        this.name = ClausewitzUtils.removeQuotes(leader.getName());
        this.type = leader.getType();
        this.manuever = leader.getManuever();
        this.fire = leader.getFire();
        this.shock = leader.getShock();
        this.siege = leader.getSiege();
        this.personality = leader.getPersonality();
        this.birthDate = leader.getBirthDate();
    }

    public Leader(Country country, String name, LeaderType type, int manuever, int fire, int shock, int siege, LeaderPersonality personality,
                  LocalDate birthDate) {
        this.country = country;
        this.id = null;
        this.name = name;
        this.type = type;
        this.manuever = manuever;
        this.fire = fire;
        this.shock = shock;
        this.siege = siege;
        this.personality = personality;
        this.birthDate = birthDate;
        this.changed = true;
    }

    public Leader(Leader other) {
        this.country = other.country;
        this.id = other.id;
        this.name = other.name;
        this.type = other.type;
        this.manuever = other.manuever;
        this.fire = other.fire;
        this.shock = other.shock;
        this.siege = other.siege;
        this.personality = other.personality;
        this.birthDate = other.birthDate;
        this.changed = other.changed;
    }

    @Override
    public Leader copy() {
        return new Leader(this);
    }

    public boolean personalityAllowed(LeaderPersonality leaderPersonality) {
        if (leaderPersonality.getAllow() == null) {
            return true;
        }

        if (leaderPersonality.getAllow().getCondition("is_admiral") != null) {
            if ("yes".equalsIgnoreCase(leaderPersonality.getAllow().getCondition("is_admiral")) && !LeaderType.ADMIRAL.equals(this.type)
                || "no".equalsIgnoreCase(leaderPersonality.getAllow().getCondition("is_admiral")) && LeaderType.ADMIRAL.equals(this.type)) {
                return false;
            }
        }

        return this.country == null || leaderPersonality.getAllow().apply(this.country, this.country);
    }

    public Integer getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        if (!name.equals(this.name)) {
            this.name = name;
            this.changed = true;
        }
    }

    public LeaderType getType() {
        return type;
    }

    public void setType(LeaderType type) {
        if (!type.equals(this.type)) {
            this.type = type;
            this.changed = true;
        }
    }

    public int getManuever() {
        return manuever;
    }

    public void setManuever(int manuever) {
        if (manuever != this.manuever) {
            this.manuever = manuever;
            this.changed = true;
        }
    }

    public int getFire() {
        return fire;
    }

    public void setFire(int fire) {
        if (fire != this.fire) {
            this.fire = fire;
            this.changed = true;
        }
    }

    public int getShock() {
        return shock;
    }

    public void setShock(int shock) {
        if (shock != this.shock) {
            this.shock = shock;
            this.changed = true;
        }
    }

    public int getSiege() {
        return siege;
    }

    public void setSiege(int siege) {
        if (siege != this.siege) {
            this.siege = siege;
            this.changed = true;
        }
    }

    public LeaderPersonality getPersonality() {
        return personality;
    }

    public void setPersonality(LeaderPersonality personality) {
        if (!personality.equals(this.personality)) {
            this.personality = personality;
            this.changed = true;
        }
    }

    public LocalDate getBirthDate() {
        return birthDate;
    }

    public void setBirthDate(LocalDate birthDate) {
        if (!birthDate.equals(this.birthDate)) {
            this.birthDate = birthDate;
            this.changed = true;
        }
    }

    public boolean isChanged() {
        return changed;
    }
}
