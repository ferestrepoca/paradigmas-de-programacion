package agenda;

/**
 * Ejemplo 4 — Inter-Type Declarations (ITD)
 *
 * Ejecutar:
 *   mvn clean compile exec:java
 *
 * Aspecto activo:
 *   PrioridadAspect → inyecta campo 'prioridad' y métodos get/set en Tarea
 *                     before: muestra prioridad antes de ejecutar
 *
 * Observar: tarea.setPrioridad() y tarea.getPrioridad() funcionan
 * aunque Tarea.java no los declare. Son inyectados por el aspecto.
 */
public class Main {

    public static void main(String[] args) {
        Tarea t1 = new Tarea("Enviar informe mensual");
        Tarea t2 = new Tarea("Reunión con cliente");
        Tarea t3 = new Tarea("Actualizar documentación");

        // setPrioridad fue inyectado por PrioridadAspect (ITD)
        t1.setPrioridad(1);   // crítica
        t2.setPrioridad(3);   // media
        // t3 queda con prioridad por defecto: 5

        System.out.println("=== Ejecutando agenda ===\n");
        t1.ejecutar();
        System.out.println();
        t2.ejecutar();
        System.out.println();
        t3.ejecutar();

        System.out.println("\n=== Prioridades finales ===");
        System.out.println("  " + t1.getDescripcion() + " → " + t1.getPrioridad());
        System.out.println("  " + t2.getDescripcion() + " → " + t2.getPrioridad());
        System.out.println("  " + t3.getDescripcion() + " → " + t3.getPrioridad());
    }
}
