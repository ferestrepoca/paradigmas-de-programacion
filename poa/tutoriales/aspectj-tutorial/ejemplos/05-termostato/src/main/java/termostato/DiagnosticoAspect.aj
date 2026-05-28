package termostato;

/**
 * Aspecto que demuestra declare parents + ITD.
 *
 * declare parents: hace que Termostato implemente Reportable
 *                  en tiempo de compilación, sin modificar Termostato.java.
 *
 * ITD: provee el cuerpo del método generarReporte() que exige la interfaz.
 *      El método accede a los campos de Termostato normalmente.
 *
 * Resultado: en Main.java se puede hacer:
 *   Reportable r = (Reportable) termostato;
 *   r.generarReporte();
 * sin que Termostato.java declare 'implements Reportable'.
 */
public aspect DiagnosticoAspect {

    // declare parents: Termostato pasa a implementar Reportable
    declare parents: Termostato implements Reportable;

    // ITD: implementación del método exigido por la interfaz
    public void Termostato.generarReporte() {
        System.out.println("  ┌─ Diagnóstico Termostato ─────────────┐");
        System.out.printf ("  │  Temperatura actual : %3d°C           │%n", this.temperatura);
        System.out.printf ("  │  Calefacción        : %-16s│%n",
                this.calefaccionActiva ? "ACTIVA" : "INACTIVA");
        System.out.printf ("  │  Estado             : %-16s│%n",
                this.temperatura > 25 ? "⚠ Por encima de 25°C" : "✓ Normal");
        System.out.println("  └──────────────────────────────────────┘");
    }
}
