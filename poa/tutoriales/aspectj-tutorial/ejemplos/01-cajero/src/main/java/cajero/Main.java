package cajero;

/**
 * Ejemplo 1 — before / after
 *
 * Ejecutar:
 *   mvn clean compile exec:java
 *
 * Aspecto activo:
 *   AuditoriaAspect → before/after intercepta depositar() y retirar()
 *
 * CajeroAutomatico.java NO contiene ningún log. El aspecto añade
 * auditoría completa sin modificar la clase de negocio.
 */
public class Main {

    public static void main(String[] args) {
        CajeroAutomatico cajero = new CajeroAutomatico(500.0);

        System.out.println("=== Depósito normal ===");
        cajero.depositar(200.0);

        System.out.println("=== Retiro normal ===");
        cajero.retirar(150.0);

        System.out.println("=== Retiro con fondos insuficientes ===");
        try {
            cajero.retirar(2000.0);
        } catch (IllegalStateException e) {
            System.out.println("  [EXCEPCIÓN] " + e.getMessage() + "\n");
        }

        System.out.println("Saldo final: $" + cajero.getSaldo());
    }
}
