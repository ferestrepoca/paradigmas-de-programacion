package cache;

import java.util.HashMap;
import java.util.Map;

/**
 * Aspecto de caché (advice tipo around + after).
 *
 * Patrón de funcionamiento:
 *   1. Consulta: around intercepta getPrice(id)
 *      → si id está en caché → retorna el valor SIN llamar proceed() (evita la BD)
 *      → si no está          → llama proceed() (va a la BD), guarda en caché y retorna
 *
 *   2. Actualización: after intercepta updatePrice(id, ...)
 *      → invalida la entrada en caché para que la próxima consulta sea fresca
 *
 * 'around' es el único advice que puede decidir si ejecutar o no el método original
 * y también puede modificar el valor de retorno.
 */
public aspect CacheAspect {

    // Estado del aspecto — el caché es un HashMap por instancia del aspecto (singleton)
    private final Map<String, Double> cache = new HashMap<>();

    // Pointcut para consultas: captura el argumento 'id'
    pointcut priceQuery(String id) :
        execution(double ProductCatalog.getPrice(String)) && args(id);

    // Pointcut para actualizaciones: captura solo el productId (ignora el precio con *)
    pointcut priceUpdate(String id) :
        execution(void ProductCatalog.updatePrice(String, double)) && args(id, *);

    // around: intercepta consultas y gestiona el caché
    double around(String id) : priceQuery(id) {
        if (cache.containsKey(id)) {
            System.out.println("  [CACHE HIT]  " + id + " = $" + cache.get(id)
                    + "  (no se consultó la BD)");
            return cache.get(id);    // retorna inmediatamente sin llamar proceed()
        }

        double result = proceed(id); // consulta real a la BD
        cache.put(id, result);
        System.out.println("  [CACHE SET]  " + id + " = $" + result + "  (guardado en caché)");
        return result;
    }

    // after: invalida el caché cuando el precio cambia
    after(String id) : priceUpdate(id) {
        if (cache.remove(id) != null) {
            System.out.println("  [CACHE INV]  " + id + " invalidado del caché");
        }
    }
}
