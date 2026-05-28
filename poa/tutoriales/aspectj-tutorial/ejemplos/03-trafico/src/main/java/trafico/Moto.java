package trafico;

/**
 * Subclase que hereda arrancar() y frenar() de Vehiculo sin sobreescribirlos.
 *
 * Esto es crucial: cuando se llama moto.arrancar(), el cuerpo
 * del método que se ejecuta es el de Vehiculo, no el de Moto.
 *
 * Por eso execution(* Vehiculo.arrancar()) se dispara aunque
 * la referencia sea de tipo Moto.
 */
public class Moto extends Vehiculo {

    public Moto() {
        super("Moto");
    }
}
