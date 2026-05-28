package biblioteca;

/** Entidad de dominio — representa un socio de la biblioteca. */
public class Member {

    private final String id;
    private final String name;
    private boolean hasDebt;
    private int loanCount;

    public Member(String id, String name) {
        this.id = id;
        this.name = name;
        this.hasDebt = false;
        this.loanCount = 0;
    }

    public String getId()    { return id; }
    public String getName()  { return name; }
    public boolean hasDebt() { return hasDebt; }
    public int loanCount()   { return loanCount; }

    public void setHasDebt(boolean hasDebt) { this.hasDebt = hasDebt; }
    public void incrementLoans() { loanCount++; }
    public void decrementLoans() { if (loanCount > 0) loanCount--; }

    @Override
    public String toString() { return name + " (" + id + ")"; }
}
