package trafico;

/**
 * Clase base que representa un vehículo genérico.
 * arrancar() y frenar() son heredados por Moto.
 *
 * Clave para entender call vs execution:
 *   El CUERPO de arrancar() vive aquí en Vehiculo.
 *   Moto hereda el método pero NO lo sobreescribe.
 *
 *   → call(* Moto.arrancar()) → sensor en el CALLER (Main.java)
 *   → execution(* Vehiculo.arrancar()) → sensor DENTRO del cuerpo
 *   Ambos se disparan con una sola llamada: moto.arrancar()
 */
public class Vehiculo {

    protected String tipo;

    public Vehiculo(String tipo) {
        this.tipo = tipo;
    }

    public void arrancar() {
        System.out.println("  " + tipo + " arrancando motor...");
    }

    public void frenar() {
        System.out.println("  " + tipo + " aplicando frenos.");
    }
}
