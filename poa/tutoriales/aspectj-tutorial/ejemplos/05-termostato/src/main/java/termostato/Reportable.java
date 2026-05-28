package termostato;

/**
 * Interfaz que define la capacidad de generar reportes de diagnóstico.
 * Termostato NO la implementa en su código fuente.
 * DiagnosticoAspect usa 'declare parents' para que Termostato la implemente.
 */
public interface Reportable {
    void generarReporte();
}
