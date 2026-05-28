package procesador;

/**
 * Ejemplo 7 — around() + proceed()
 *
 * Ejecutar:
 *   mvn clean compile exec:java
 *
 * Aspecto activo:
 *   ResultadoCacheAspect → around: HIT retorna sin proceed() / MISS llama proceed()
 *                          after:  invalida la entrada del caché
 *
 * Observar:
 *   1ª llamada a "Análisis"     → MISS: llama proceed() (150 ms)
 *   2ª y 3ª llamada a "Análisis" → HIT: no llama proceed() (instantáneo)
 *   invalidarTarea("Análisis")  → elimina del caché
 *   4ª llamada a "Análisis"     → MISS de nuevo (fue invalidado)
 */
public class Main {

    public static void main(String[] args) throws InterruptedException {
        ProcesadorTareas proc = new ProcesadorTareas();

        System.out.println("=== Primera ejecución (MISS → proceed()) ===");
        String r1 = proc.ejecutarTarea("Análisis de ventas");
        System.out.println("  Resultado: " + r1);

        System.out.println("\n=== Segunda y tercera (HIT → sin proceed()) ===");
        System.out.println("  Resultado: " + proc.ejecutarTarea("Análisis de ventas"));
        System.out.println("  Resultado: " + proc.ejecutarTarea("Análisis de ventas"));

        System.out.println("\n=== Otra tarea (MISS) ===");
        System.out.println("  Resultado: " + proc.ejecutarTarea("Reporte mensual"));
        System.out.println("  Resultado: " + proc.ejecutarTarea("Reporte mensual"));

        System.out.println("\n=== Invalidación del caché ===");
        proc.invalidarTarea("Análisis de ventas");

        System.out.println("\n=== Post-invalidación (MISS de nuevo) ===");
        System.out.println("  Resultado: " + proc.ejecutarTarea("Análisis de ventas"));

        System.out.println("\n=== Resumen ===");
        System.out.println("  Llamadas totales a ejecutarTarea : 6");
        System.out.println("  Ejecuciones reales (proceed())   : " + proc.getEjecuciones());
        System.out.println("  Ejecuciones ahorradas por caché  : " + (6 - proc.getEjecuciones()));
    }
}
