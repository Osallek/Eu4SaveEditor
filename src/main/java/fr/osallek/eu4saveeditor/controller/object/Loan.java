package fr.osallek.eu4saveeditor.controller.object;

import java.time.LocalDate;
import java.util.Objects;

public class Loan {

    private final Integer id;

    private int amount;

    private double interest;

    private LocalDate expiryDate;

    private boolean changed;

    public Loan(int amount, double interest, LocalDate expiryDate) {
        this.id = null;
        this.amount = amount;
        this.interest = interest;
        this.expiryDate = expiryDate;
        this.changed = true;
    }

    public Loan(fr.osallek.eu4parser.model.save.country.Loan loan) {
        this.id = loan.getId().getId();
        this.amount = loan.getAmount();
        this.interest = loan.getInterest();
        this.expiryDate = loan.getExpiryDate();
    }

    public Loan(Loan other) {
        this.id = other.id;
        this.amount = other.amount;
        this.interest = other.interest;
        this.expiryDate = other.expiryDate;
        this.changed = other.changed;
    }

    public Integer getId() {
        return id;
    }

    public int getAmount() {
        return amount;
    }

    public void setAmount(int amount) {
        if (amount != this.amount) {
            this.amount = amount;
            this.changed = true;
        }
    }

    public double getInterest() {
        return interest;
    }

    public void setInterest(double interest) {
        if (interest != this.interest) {
            this.interest = interest;
            this.changed = true;
        }
    }

    public LocalDate getExpiryDate() {
        return expiryDate;
    }

    public void setExpiryDate(LocalDate expiryDate) {
        if (!expiryDate.equals(this.expiryDate)) {
            this.expiryDate = expiryDate;
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
        Loan loan = (Loan) o;
        return amount == loan.amount &&
               Double.compare(loan.interest, interest) == 0 &&
               Objects.equals(expiryDate, loan.expiryDate);
    }

    @Override
    public int hashCode() {
        return Objects.hash(amount, interest, expiryDate);
    }
}
