package biblioteca;

/**
 * Aspecto 2 — Límite de préstamos simultáneos (advice tipo around).
 *
 * Envuelve Library.lendBook(). Si el socio ya tiene MAX_LOANS libros
 * activos, NO llama proceed() → la operación no se ejecuta.
 * Si el límite no se alcanzó, llama proceed() → la operación continúa normalmente.
 *
 * 'around' es el único advice que puede decidir si ejecutar o no el método original.
 */
public aspect LoanLimitAspect {

    private static final int MAX_LOANS = 3;

    pointcut lending(Member m, Book b) :
        execution(void Library.lendBook(Member, Book)) && args(m, b);

    void around(Member m, Book b) : lending(m, b) {
        if (m.loanCount() >= MAX_LOANS) {
            System.out.println("[LÍMITE] Préstamo bloqueado — "
                    + m.getName() + " ya tiene " + MAX_LOANS + " libros activos."
                    + " Debe devolver uno antes de pedir otro.");
            return;          // NO llama proceed() → lendBook() no se ejecuta
        }
        proceed(m, b);       // lendBook() se ejecuta normalmente
    }
}
