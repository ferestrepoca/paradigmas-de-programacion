package procesador;

/**
 * Ejemplo 7 — around + proceed().
 *
 * Simula el procesamiento costoso de tareas (150 ms por ejecución).
 * ResultadoCacheAspect.aj intercepta ejecutarTarea() con around:
 *   - CACHE HIT → retorna sin llamar proceed() (evita la espera)
 *   - MISS      → llama proceed(), guarda el resultado, retorna
 *
 * ejecutarTarea() no sabe que existe el caché.
 */
public class ProcesadorTareas {

    private int ejecutions = 0;

    public String ejecutarTarea(String nombre) {
        ejecutions++;
        System.out.println("  [CPU]  procesando \"" + nombre
                + "\"  (#" + ejecutions + ")");
        try {
            Thread.sleep(150);   // simula trabajo costoso
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
        return "resultado:" + nombre.toLowerCase().replace(" ", "-");
    }

    public void invalidarTarea(String nombre) {
        System.out.println("  [INV]  invalidando caché de \"" + nombre + "\"");
    }

    public int getEjecuciones() {
        return ejecutions;
    }
}
