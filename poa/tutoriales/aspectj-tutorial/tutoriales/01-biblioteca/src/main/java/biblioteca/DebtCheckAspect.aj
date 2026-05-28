package biblioteca;

/**
 * Aspecto 1 — Validación de deudas (advice tipo before).
 *
 * Intercepta Library.lendBook() ANTES de que se ejecute.
 * Si el socio tiene deuda pendiente, lanza una excepción y
 * detiene el préstamo — sin tocar Library.java ni Member.java.
 */
public aspect DebtCheckAspect {

    // Captura m y b de los argumentos para usarlos en el advice
    pointcut lending(Member m, Book b) :
        execution(void Library.lendBook(Member, Book)) && args(m, b);

    before(Member m, Book b) : lending(m, b) {
        if (m.hasDebt()) {
            System.out.println("[DEUDA] Préstamo bloqueado — "
                    + m.getName() + " tiene deudas pendientes.");
            throw new IllegalStateException(
                    "El socio " + m.getName() + " tiene deudas y no puede tomar prestados nuevos libros.");
        }
    }
}
