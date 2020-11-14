package com.osallek.eu4saveeditor.controller.object;

import com.osallek.eu4parser.model.game.SubjectType;
import com.osallek.eu4parser.model.save.country.Country;

import java.time.LocalDate;
import java.util.Objects;

public class CountrySubject {

    private final Country overlord;

    private Country subject;

    private SubjectType subjectType;

    private LocalDate startDate;

    public CountrySubject(Country subject) {
        this.subject = subject;
        this.overlord = this.subject.getOverlord();
        this.subjectType = this.subject.getSubjectType();
        this.startDate = this.subject.getSubjectStartDate();
    }

    public CountrySubject(Country overlord, Country subject, SubjectType subjectType, LocalDate startDate) {
        this.overlord = overlord;
        this.subject = subject;
        this.subjectType = subjectType;
        this.startDate = startDate;
    }

    public CountrySubject(CountrySubject other) {
        this.overlord = other.overlord;
        this.subject = other.subject;
        this.subjectType = other.subjectType;
        this.startDate = other.startDate;
    }

    public Country getOverlord() {
        return overlord;
    }

    public Country getSubject() {
        return subject;
    }

    public void setSubject(Country subject) {
        this.subject = subject;
    }

    public SubjectType getSubjectType() {
        return subjectType;
    }

    public void setSubjectType(SubjectType subjectType) {
        this.subjectType = subjectType;
    }

    public LocalDate getStartDate() {
        return startDate;
    }

    public void setStartDate(LocalDate startDate) {
        this.startDate = startDate;
    }

    public boolean changed() {
        return !this.overlord.equals(this.subject.getOverlord()) || !this.subjectType.equals(this.subject.getSubjectType())
               || !this.startDate.equals(this.subject.getSubjectStartDate());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }

        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        CountrySubject that = (CountrySubject) o;
        return Objects.equals(overlord, that.overlord) &&
               Objects.equals(subject, that.subject) &&
               Objects.equals(subjectType, that.subjectType) &&
               Objects.equals(startDate, that.startDate);
    }

    @Override
    public int hashCode() {
        return Objects.hash(overlord, subject, subjectType, startDate);
    }
}
