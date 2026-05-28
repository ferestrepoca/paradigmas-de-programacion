package trafico;

/**
 * Aspecto que demuestra la diferencia entre call() y execution().
 *
 * call()      → se activa en el CALLER (quien invoca el método).
 *               Aquí: Main.java cuando escribe moto.arrancar().
 *               Con herencia: call(* Moto.arrancar()) matchea
 *               la llamada estática basada en el tipo declarado.
 *
 * execution() → se activa al ENTRAR al cuerpo del método.
 *               Aquí: dentro de Vehiculo.arrancar().
 *               Con herencia: execution(* Vehiculo.arrancar())
 *               se dispara aunque la instancia sea Moto, porque
 *               el cuerpo vive en Vehiculo.
 *
 * Resultado al llamar moto.arrancar():
 *   [CALL]  antes de entrar al cuerpo (en Main.java)
 *   [EXEC]  al inicio del cuerpo (en Vehiculo.arrancar())
 *             → Moto arrancando motor...
 *   [EXEC]  al salir del cuerpo
 *
 * Para frenar() solo actúa la regla de execution (Vehiculo):
 *   call(* Moto.frenar()) no se declara, así que no hay CALL.
 */
public aspect SensorTrafico {

    // call: sensor en el sitio de invocación (Main.java)
    before() : call(* Moto.arrancar()) {
        System.out.println("[CALL]  sensor activado en el caller "
                + "— tipo estático: Moto");
    }

    // execution: sensor dentro del cuerpo heredado de Vehiculo
    before() : execution(* Vehiculo.arrancar()) {
        System.out.println("[EXEC]  entrando al cuerpo del método "
                + "— cuerpo en: Vehiculo");
    }

    after() : execution(* Vehiculo.arrancar()) {
        System.out.println("[EXEC]  saliendo del cuerpo del método");
    }

    // execution de frenar — solo afecta a Vehiculo.frenar()
    before() : execution(* Vehiculo.frenar()) {
        System.out.println("[EXEC]  frenado detectado");
    }
}
