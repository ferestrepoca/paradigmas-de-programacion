package biblioteca;

/**
 * Aspecto 3 — Log de actividad (advice tipo after returning).
 *
 * Registra SOLO las operaciones que terminaron con éxito.
 * 'after returning' no se dispara si el método lanzó excepción
 * (p.ej. cuando DebtCheckAspect bloqueó la operación).
 *
 * En un sistema real, aquí iría la escritura en base de datos de auditoría.
 */
public aspect ActivityLogAspect {

    pointcut lendOp(Member m, Book b) :
        execution(void Library.lendBook(Member, Book)) && args(m, b);

    pointcut returnOp(Member m, Book b) :
        execution(void Library.returnBook(Member, Book)) && args(m, b);

    after(Member m, Book b) returning() : lendOp(m, b) {
        System.out.println("[LOG] PRESTAMO — socio: " + m.getName()
                + " | libro: " + b.getTitle()
                + " | libros activos: " + m.loanCount());
    }

    after(Member m, Book b) returning() : returnOp(m, b) {
        System.out.println("[LOG] DEVOLUCION — socio: " + m.getName()
                + " | libro: " + b.getTitle()
                + " | libros activos: " + m.loanCount());
    }
}
