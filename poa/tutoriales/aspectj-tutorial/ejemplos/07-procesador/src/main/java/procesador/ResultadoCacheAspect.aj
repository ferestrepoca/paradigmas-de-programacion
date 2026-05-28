package procesador;

import java.util.HashMap;
import java.util.Map;

/**
 * Aspecto de caché — demuestra around() y proceed().
 *
 * around() es el único advice que puede:
 *   1. Decidir si llama o no al método original (proceed()).
 *   2. Modificar el valor de retorno.
 *   3. Ejecutar código antes Y después en un solo advice.
 *
 * Patrón:
 *   CACHE HIT  → retorna sin llamar proceed() (el método original no se ejecuta)
 *   CACHE MISS → llama proceed(), guarda el resultado, retorna el valor
 *
 * invalidarTarea() se intercepta con after() para limpiar la entrada del caché.
 */
public aspect ResultadoCacheAspect {

    private final Map<String, String> cache = new HashMap<>();

    // Pointcut para el método costoso, capturando el nombre de la tarea
    pointcut ejecutando(String nombre) :
        execution(String ProcesadorTareas.ejecutarTarea(String)) && args(nombre);

    // Pointcut para la invalidación
    pointcut invalidando(String nombre) :
        execution(void ProcesadorTareas.invalidarTarea(String)) && args(nombre);

    // around: decide si ejecutar o devolver desde caché
    String around(String nombre) : ejecutando(nombre) {
        if (cache.containsKey(nombre)) {
            System.out.println("  [HIT]  \"" + nombre + "\" → " + cache.get(nombre)
                    + "  (proceed() NO llamado)");
            return cache.get(nombre);      // retorna SIN llamar el método original
        }

        String resultado = proceed(nombre); // llama ejecutarTarea() en la BD/CPU
        cache.put(nombre, resultado);
        System.out.println("  [SET]  \"" + nombre + "\" guardado en caché");
        return resultado;
    }

    // after: elimina la entrada del caché cuando se invalida
    after(String nombre) : invalidando(nombre) {
        if (cache.remove(nombre) != null) {
            System.out.println("  [DEL]  \"" + nombre + "\" eliminado del caché");
        }
    }
}
