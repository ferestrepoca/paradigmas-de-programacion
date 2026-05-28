package restaurante;

/**
 * Aspecto de control de órdenes — demuestra wildcards en pointcuts.
 *
 * Wildcards usados:
 *   preparar*  → coincide con prepararEntrada, prepararPrincipal, prepararPostre
 *   *          → cualquier tipo de retorno o cualquier nombre de método
 *   (..)       → cualquier número y tipo de parámetros
 *   !          → negación lógica
 *
 * Clave: cocina() captura SOLO los métodos que empiezan por "preparar"
 *        sin listarlos uno a uno. Si en el futuro se agrega
 *        prepararBebida(), el aspecto la captura automáticamente.
 */
public aspect OrdenAspect {

    // Wildcard preparar* → todos los métodos de cocina
    pointcut cocina() :
        call(void Restaurante.preparar*(..));

    // * → cualquier método de Restaurante con cualquier retorno
    pointcut cualquiera() :
        call(* Restaurante.*(..));

    before() : cocina() {
        System.out.println("[COCINA]  iniciando: "
                + thisJoinPoint.getSignature().getName());
    }

    after() : cocina() {
        System.out.println("[COCINA]  listo: "
                + thisJoinPoint.getSignature().getName());
    }

    // cobrar() es el único que no matchea preparar*, por eso aplica !cocina()
    after() : cualquiera() && !cocina() {
        System.out.println("[CAJA]    operación de caja registrada\n");
    }
}
