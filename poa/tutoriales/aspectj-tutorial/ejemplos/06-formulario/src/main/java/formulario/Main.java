package formulario;

/**
 * Ejemplo 6 — args() + after() throwing
 *
 * Ejecutar:
 *   mvn clean compile exec:java
 *
 * Aspecto activo:
 *   ValidacionAspect → before(nombre, email, edad): valida parámetros con args()
 *                      after() throwing(Exception ex): centraliza log de errores
 *
 * Observar:
 *   - Registro válido     → valida y registra normalmente
 *   - Email sin @         → before lanza excepción, after throwing la captura
 *   - Edad negativa       → idem
 *   - Eliminar inexistente → after throwing captura la excepción de negocio
 */
public class Main {

    public static void main(String[] args) {
        RegistroUsuarios reg = new RegistroUsuarios();

        System.out.println("=== Registro válido ===");
        reg.registrar("Ana García", "ana@mail.com", 30);

        System.out.println("\n=== Email sin @ ===");
        try {
            reg.registrar("Bob", "bob-sin-arroba.com", 25);
        } catch (IllegalArgumentException e) {
            System.out.println("  Capturado en Main: " + e.getMessage());
        }

        System.out.println("\n=== Edad negativa ===");
        try {
            reg.registrar("Carlos", "carlos@mail.com", -5);
        } catch (IllegalArgumentException e) {
            System.out.println("  Capturado en Main: " + e.getMessage());
        }

        System.out.println("\n=== Registro adicional ===");
        reg.registrar("Diana", "diana@mail.com", 22);

        System.out.println("\n=== Eliminar usuario inexistente ===");
        try {
            reg.eliminar("Nadie");
        } catch (IllegalArgumentException e) {
            System.out.println("  Capturado en Main: " + e.getMessage());
        }

        System.out.println("\n=== Eliminar usuario existente ===");
        reg.eliminar("Ana García");

        System.out.println("\nTotal usuarios registrados: " + reg.total());
    }
}
