package cache;

import java.util.HashMap;
import java.util.Map;

/**
 * Simula un catálogo de productos con consultas "costosas" a base de datos.
 *
 * NO contiene ninguna lógica de caché. CacheAspect.aj intercepta getPrice()
 * de forma transparente y evita las consultas repetidas.
 *
 * queryCount permite verificar cuántas veces se llegó realmente a la BD.
 */
public class ProductCatalog {

    // Simula la base de datos
    private static final Map<String, Double> DB = new HashMap<>();
    static {
        DB.put("P001", 29.99);
        DB.put("P002", 149.99);
        DB.put("P003", 9.99);
        DB.put("P004", 59.99);
    }

    private int queryCount = 0;

    /**
     * Consulta "cara" — simula latencia de BD con Thread.sleep(80ms).
     * CacheAspect evita que este método se llame cuando ya tiene la respuesta.
     */
    public double getPrice(String productId) {
        queryCount++;
        System.out.println("  [BD] Consulta real #" + queryCount + " → " + productId);
        try {
            Thread.sleep(80);   // simula latencia de red/BD
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
        if (!DB.containsKey(productId)) {
            throw new IllegalArgumentException("Producto no encontrado: " + productId);
        }
        return DB.get(productId);
    }

    /**
     * Actualiza el precio. CacheAspect invalida la entrada del caché
     * para que la próxima consulta vaya a la BD con el valor actualizado.
     */
    public void updatePrice(String productId, double newPrice) {
        DB.put(productId, newPrice);
        System.out.println("  [BD] Precio actualizado: " + productId + " = $" + newPrice);
    }

    public int getQueryCount() { return queryCount; }
}
