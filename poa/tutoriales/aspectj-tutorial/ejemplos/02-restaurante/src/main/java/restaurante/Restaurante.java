package restaurante;

/**
 * Ejemplo 2 — Wildcards en pointcuts.
 *
 * Simula las operaciones de cocina y caja de un restaurante.
 * El aspecto OrdenAspect usa el wildcard preparar* para interceptar
 * todos los métodos de cocina sin nombrarlos individualmente.
 */
public class Restaurante {

    public void prepararEntrada(String plato) {
        System.out.println("  Cocina: preparando entrada — " + plato);
    }

    public void prepararPrincipal(String plato) {
        System.out.println("  Cocina: preparando plato principal — " + plato);
    }

    public void prepararPostre(String postre) {
        System.out.println("  Cocina: preparando postre — " + postre);
    }

    public void cobrar(double total) {
        System.out.println("  Caja: total a cobrar = $" + total);
    }
}
