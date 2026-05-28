package cajero;

import java.util.Arrays;

/**
 * Aspecto de auditoría (advice tipo before + after).
 *
 * Intercepta todas las operaciones de CajeroAutomatico excepto getSaldo(),
 * imprimiendo nombre del método y argumentos antes de la llamada
 * y confirmando el resultado después.
 *
 * CajeroAutomatico.java NO sabe que este aspecto existe.
 */
public aspect AuditoriaAspect {

    // Pointcut: cualquier ejecución en CajeroAutomatico, salvo el getter
    pointcut operaciones() :
        execution(* CajeroAutomatico.*(..))
        && !execution(* CajeroAutomatico.getSaldo());

    // before: se ejecuta justo antes del método
    before() : operaciones() {
        System.out.println("[ANTES]  "
                + thisJoinPoint.getSignature().getName()
                + "  args: " + Arrays.toString(thisJoinPoint.getArgs()));
    }

    // after: se ejecuta siempre al terminar, tanto en éxito como en excepción
    after() : operaciones() {
        System.out.println("[DESPUÉS] "
                + thisJoinPoint.getSignature().getName()
                + " completado\n");
    }
}
