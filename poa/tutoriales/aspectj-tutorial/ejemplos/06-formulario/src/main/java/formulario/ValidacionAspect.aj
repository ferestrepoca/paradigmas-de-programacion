package formulario;

/**
 * Aspecto de validación — demuestra args() y after() throwing.
 *
 * args(nombre, email, edad):
 *   Captura los valores reales de los parámetros del método en el advice.
 *   Permite leer/validar los argumentos sin reflexión.
 *
 * after() throwing(Exception ex):
 *   Se activa SOLO cuando el método termina lanzando una excepción.
 *   Permite centralizar el manejo de errores de toda la capa de registro
 *   sin duplicar bloques try/catch.
 *
 * RegistroUsuarios.java no contiene ninguna validación.
 */
public aspect ValidacionAspect {

    // Pointcut que captura los 3 parámetros de registrar()
    pointcut registrando(String nombre, String email, int edad) :
        execution(void RegistroUsuarios.registrar(String, String, int))
        && args(nombre, email, edad);

    // before con args: valida antes de ejecutar el método
    before(String nombre, String email, int edad) : registrando(nombre, email, edad) {
        if (nombre == null || nombre.isBlank())
            throw new IllegalArgumentException("Nombre no puede estar vacío");
        if (email == null || !email.contains("@"))
            throw new IllegalArgumentException("Email inválido: " + email);
        if (edad < 0 || edad > 120)
            throw new IllegalArgumentException("Edad inválida: " + edad);
        System.out.println("  [VALIDACIÓN OK] " + nombre + " / " + email + " / " + edad);
    }

    // after() throwing: centraliza el log de cualquier error en RegistroUsuarios
    after() throwing(Exception ex) :
            execution(* RegistroUsuarios.*(..)) {
        System.err.println("  [ERROR] "
                + thisJoinPoint.getSignature().getName()
                + " → " + ex.getMessage());
    }
}
