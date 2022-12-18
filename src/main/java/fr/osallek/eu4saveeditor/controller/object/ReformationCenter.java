package fr.osallek.eu4saveeditor.controller.object;

import fr.osallek.eu4parser.model.save.SaveReligion;
import fr.osallek.eu4parser.model.save.province.SaveProvince;
import fr.osallek.eu4saveeditor.common.Copy;
import java.util.Objects;

public class ReformationCenter extends Copy<ReformationCenter> {

    private final SaveReligion religion;

    private SaveProvince province;

    private boolean changed;

    public ReformationCenter(SaveReligion religion, SaveProvince province) {
        this.religion = religion;
        this.province = province;
        this.changed = true;
    }

    public ReformationCenter(fr.osallek.eu4parser.model.save.religion.ReformationCenter reformationCenter) {
        this.religion = reformationCenter.getReligion();
        this.province = reformationCenter.getProvince();
    }

    public ReformationCenter(ReformationCenter other) {
        this.religion = other.religion;
        this.province = other.province;
        this.changed = other.changed;
    }

    @Override
    public ReformationCenter copy() {
        return new ReformationCenter(this);
    }

    public SaveReligion getReligion() {
        return religion;
    }

    public SaveProvince getProvince() {
        return province;
    }

    public void setProvince(SaveProvince province) {
        if (!this.province.equals(province)) {
            this.province = province;
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

        ReformationCenter that = (ReformationCenter) o;
        return Objects.equals(religion, that.religion) &&
               Objects.equals(province, that.province);
    }

    @Override
    public int hashCode() {
        return Objects.hash(religion, province);
    }
}
