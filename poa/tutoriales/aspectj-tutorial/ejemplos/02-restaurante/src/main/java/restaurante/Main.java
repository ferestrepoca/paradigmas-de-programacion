package restaurante;

/**
 * Ejemplo 2 — Wildcards en pointcuts.
 *
 * Ejecutar:
 *   mvn clean compile exec:java
 *
 * Aspecto activo:
 *   OrdenAspect → before/after sobre preparar*(..) y * Restaurante.*(..);
 *
 * Observar cómo todos los métodos prepararX disparan el before/after de cocina,
 * pero cobrar() solo dispara la regla de caja (con !cocina()).
 */
public class Main {

    public static void main(String[] args) {
        Restaurante rest = new Restaurante();

        System.out.println("=== Mesa 1 — menú completo ===\n");
        rest.prepararEntrada("Bruschetta");
        System.out.println();
        rest.prepararPrincipal("Pasta carbonara");
        System.out.println();
        rest.prepararPostre("Tiramisú");
        System.out.println();
        rest.cobrar(45.50);
    }
}
