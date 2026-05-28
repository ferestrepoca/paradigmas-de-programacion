package agenda;

/**
 * Ejemplo 4 — Inter-Type Declarations (ITD).
 *
 * Clase de tarea simple. NO tiene campo de prioridad
 * ni los métodos getPrioridad / setPrioridad.
 *
 * PrioridadAspect.aj los inyecta desde afuera usando ITD.
 * Después del weaving, Tarea se comporta como si siempre
 * hubiera tenido el campo y los métodos, sin que esta
 * clase haya sido modificada.
 */
public class Tarea {

    private String descripcion;

    public Tarea(String descripcion) {
        this.descripcion = descripcion;
    }

    public String getDescripcion() {
        return descripcion;
    }

    public void ejecutar() {
        System.out.println("  Ejecutando: " + descripcion);
    }
}
