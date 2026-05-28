package biblioteca;

/**
 * Lógica de negocio pura — gestiona préstamos y devoluciones.
 *
 * NO contiene validación de deudas, límites de préstamo ni logging.
 * Esas responsabilidades transversales las manejan los tres aspectos:
 *   DebtCheckAspect   → valida deudas antes de prestar (before)
 *   LoanLimitAspect   → bloquea si el socio ya tiene 3 libros (around)
 *   ActivityLogAspect → registra operaciones exitosas (after returning)
 */
public class Library {

    public void lendBook(Member member, Book book) {
        if (!book.isAvailable()) {
            System.out.println("[BIBLIOTECA] El libro " + book + " no está disponible.");
            return;
        }
        book.setAvailable(false);
        member.incrementLoans();
        System.out.println("[BIBLIOTECA] Préstamo exitoso: " + book.getTitle()
                + " → " + member.getName());
    }

    public void returnBook(Member member, Book book) {
        book.setAvailable(true);
        member.decrementLoans();
        System.out.println("[BIBLIOTECA] Devolución registrada: " + book.getTitle()
                + " devuelto por " + member.getName());
    }
}
