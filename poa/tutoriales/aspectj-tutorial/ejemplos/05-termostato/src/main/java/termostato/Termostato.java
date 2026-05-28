package termostato;

/**
 * Ejemplo 5 — declare parents.
 *
 * Termostato controla la temperatura del hogar.
 * No implementa ninguna interfaz de diagnóstico.
 *
 * DiagnosticoAspect usa 'declare parents' para hacer que Termostato
 * implemente Reportable, e ITD para proveer el cuerpo del método.
 * Esto permite tratar un Termostato como Reportable en Main.java
 * sin modificar esta clase.
 */
public class Termostato {

    private int temperatura;
    private boolean calefaccionActiva;

    public Termostato(int temperaturaInicial) {
        this.temperatura = temperaturaInicial;
        this.calefaccionActiva = temperaturaInicial > 25;
    }

    public void ajustar(int nuevaTemp) {
        this.temperatura = nuevaTemp;
        this.calefaccionActiva = nuevaTemp > 25;
        System.out.println("  Temperatura ajustada a: " + temperatura + "°C"
                + (calefaccionActiva ? "  [CALEFACCIÓN ACTIVA]" : "  [NORMAL]"));
    }

    public int getTemperatura() {
        return temperatura;
    }

    public boolean isCalefaccionActiva() {
        return calefaccionActiva;
    }
}
