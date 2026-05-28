package agenda;

/**
 * Aspecto que demuestra Inter-Type Declarations (ITD).
 *
 * ITD permite agregar campos y métodos a clases existentes
 * desde un aspecto, sin modificar el archivo fuente de la clase.
 *
 * Aquí se inyecta en Tarea:
 *   - int prioridad          → nuevo campo con valor por defecto 5
 *   - setPrioridad(int p)    → nuevo método setter con validación
 *   - getPrioridad()         → nuevo método getter
 *
 * Después del weaving, cualquier código puede llamar
 * tarea.getPrioridad() o tarea.setPrioridad(3) aunque Tarea.java
 * no los declare.
 */
public aspect PrioridadAspect {

    // ITD: campo inyectado en Tarea con valor por defecto 5
    int Tarea.prioridad = 5;

    // ITD: setter inyectado en Tarea
    public void Tarea.setPrioridad(int p) {
        this.prioridad = (p >= 1 && p <= 10) ? p : 5;
    }

    // ITD: getter inyectado en Tarea
    public int Tarea.getPrioridad() {
        return this.prioridad;
    }

    // Pointcut: antes de ejecutar cualquier tarea
    pointcut ejecutando() :
        execution(void Tarea.ejecutar());

    // Muestra la prioridad antes de ejecutar (accede al campo ITD)
    before() : ejecutando() {
        Tarea t = (Tarea) thisJoinPoint.getTarget();
        System.out.printf("  [ITD] %-30s prioridad: %d%n",
                t.getDescripcion(), t.getPrioridad());
    }
}
