package trafico;

/**
 * Ejemplo 3 — call() vs execution()
 *
 * Ejecutar:
 *   mvn clean compile exec:java
 *
 * Aspecto activo:
 *   SensorTrafico → call(* Moto.arrancar())          ← actúa en el caller
 *                   execution(* Vehiculo.arrancar())  ← actúa en el cuerpo
 *                   execution(* Vehiculo.frenar())    ← sin CALL equivalente
 *
 * Observar:
 *   moto.arrancar() dispara CALL + EXEC (2 sensores con 1 llamada)
 *   moto.frenar()   dispara solo EXEC (cuerpo en Vehiculo)
 */
public class Main {

    public static void main(String[] args) {
        Moto moto = new Moto();

        System.out.println("=== moto.arrancar() ===");
        moto.arrancar();

        System.out.println("\n=== moto.frenar() ===");
        moto.frenar();

        System.out.println("\n--- Resumen ---");
        System.out.println("arrancar → call + execution (2 sensores, 1 llamada)");
        System.out.println("frenar   → solo execution (sin call declarado)");
    }
}
