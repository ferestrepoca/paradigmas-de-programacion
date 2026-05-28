package cache;

/**
 * Tutorial 3 — Sistema de Caché con around advice.
 *
 * Ejecutar:
 *   mvn clean compile exec:java
 *
 * Aspecto activo:
 *   CacheAspect → around: intercepta getPrice() y evita consultas repetidas a BD
 *                 after:  invalida el caché al actualizar un precio
 *
 * ProductCatalog.java NO sabe que existe el caché.
 */
public class Main {

    public static void main(String[] args) throws InterruptedException {
        ProductCatalog catalog = new ProductCatalog();

        // ── Primera consulta por producto → va a la BD ────────────────
        System.out.println("=== Primera consulta (sin caché) ===");
        double p1 = catalog.getPrice("P001");
        System.out.println("  Precio P001: $" + p1);

        // ── Segunda y tercera consulta → CACHE HIT ────────────────────
        System.out.println("\n=== Segunda y tercera consulta (caché activo) ===");
        System.out.println("  Precio P001: $" + catalog.getPrice("P001"));
        System.out.println("  Precio P001: $" + catalog.getPrice("P001"));

        // ── Otra consulta diferente → va a la BD ─────────────────────
        System.out.println("\n=== Consulta de otro producto ===");
        System.out.println("  Precio P002: $" + catalog.getPrice("P002"));
        System.out.println("  Precio P002: $" + catalog.getPrice("P002"));   // HIT

        // ── Actualización invalida el caché ───────────────────────────
        System.out.println("\n=== Actualización de precio (invalida caché) ===");
        catalog.updatePrice("P001", 34.99);

        // ── Consulta post-actualización → vuelve a la BD ──────────────
        System.out.println("\n=== Consulta después de actualizar ===");
        System.out.println("  Precio P001: $" + catalog.getPrice("P001"));   // BD
        System.out.println("  Precio P001: $" + catalog.getPrice("P001"));   // HIT

        // ── Resumen ────────────────────────────────────────────────────
        System.out.println("\n=== Resumen ===");
        System.out.println("  Consultas reales a BD: " + catalog.getQueryCount());
        System.out.println("  Llamadas totales a getPrice(): 7");
        System.out.println("  Ahorro = " + (7 - catalog.getQueryCount()) + " consultas evitadas por caché");
    }
}
