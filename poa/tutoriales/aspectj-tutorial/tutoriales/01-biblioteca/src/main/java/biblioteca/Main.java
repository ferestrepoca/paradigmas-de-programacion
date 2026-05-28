package biblioteca;

/**
 * Tutorial 1 — Sistema de Biblioteca con 3 aspectos coordinados.
 *
 * Ejecutar:
 *   mvn clean compile exec:java
 *
 * Aspectos activos:
 *   DebtCheckAspect   → before: bloquea si el socio tiene deuda
 *   LoanLimitAspect   → around: bloquea si ya tiene MAX_LOANS=3 libros
 *   ActivityLogAspect → after returning: registra solo las operaciones exitosas
 *
 * Library.java NO sabe que estos aspectos existen.
 */
public class Main {

    public static void main(String[] args) {
        Library library = new Library();

        Book b1 = new Book("978-0-13-468599-1", "Clean Code");
        Book b2 = new Book("978-0-13-235088-4", "The Pragmatic Programmer");
        Book b3 = new Book("978-0-13-110362-7", "The C Programming Language");
        Book b4 = new Book("978-0-20-163361-5", "Design Patterns");

        Member alice = new Member("M001", "Alice");
        Member bob   = new Member("M002", "Bob");
        bob.setHasDebt(true);   // Bob tiene deuda pendiente

        // ── Préstamos normales de Alice ───────────────────────────────
        System.out.println("=== Préstamos normales (Alice) ===");
        library.lendBook(alice, b1);
        library.lendBook(alice, b2);
        library.lendBook(alice, b3);

        // ── LoanLimitAspect bloquea el 4.° préstamo ──────────────────
        System.out.println("\n=== Alice ya tiene 3 — LoanLimitAspect actúa ===");
        library.lendBook(alice, b4);

        // ── DebtCheckAspect bloquea a Bob ────────────────────────────
        System.out.println("\n=== Bob tiene deuda — DebtCheckAspect actúa ===");
        try {
            library.lendBook(bob, b4);
        } catch (IllegalStateException e) {
            System.out.println("[INFO] " + e.getMessage());
        }

        // ── Devolución y ActivityLogAspect registra ───────────────────
        System.out.println("\n=== Alice devuelve un libro ===");
        library.returnBook(alice, b1);

        // ── Alice puede pedir de nuevo ────────────────────────────────
        System.out.println("\n=== Alice puede pedir otro libro ahora ===");
        library.lendBook(alice, b4);
    }
}
