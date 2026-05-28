package termostato;

/**
 * Ejemplo 5 — declare parents
 *
 * Ejecutar:
 *   mvn clean compile exec:java
 *
 * Aspecto activo:
 *   DiagnosticoAspect → declare parents: Termostato implements Reportable
 *                        ITD: Termostato.generarReporte()
 *
 * Observar: el cast (Reportable) termostato funciona aunque
 * Termostato.java no declare 'implements Reportable'.
 * El aspecto lo hace en tiempo de compilación.
 */
public class Main {

    public static void main(String[] args) {
        Termostato t = new Termostato(20);

        System.out.println("=== Estado inicial ===");
        // El cast funciona gracias a declare parents
        ((Reportable) t).generarReporte();

        System.out.println("\n=== Ajuste de temperatura ===");
        t.ajustar(28);

        System.out.println("\n=== Reporte post-ajuste ===");
        ((Reportable) t).generarReporte();

        System.out.println("\n=== Volviendo a temperatura normal ===");
        t.ajustar(21);
        ((Reportable) t).generarReporte();
    }
}
