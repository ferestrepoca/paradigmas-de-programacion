package calificaciones;

/**
 * Tutorial 2 — Sistema de Calificaciones con validación y alertas.
 *
 * Ejecutar:
 *   mvn clean compile exec:java
 *
 * Aspectos activos:
 *   GradeValidationAspect → before + args: valida rango 0.0–5.0
 *   AcademicRiskAspect    → after returning: alerta promedio < 3.0
 */
public class Main {

    public static void main(String[] args) {
        GradeBook gradeBook = new GradeBook();

        Student ana    = new Student("S001", "Ana García");
        Student carlos = new Student("S002", "Carlos López");
        Student diana  = new Student("S003", "Diana Torres");

        // ── Notas de Ana (buen promedio) ─────────────────────────────
        System.out.println("=== Notas de Ana ===");
        gradeBook.recordGrade(ana, "Programación",   4.5);
        gradeBook.recordGrade(ana, "Matemáticas",    3.8);
        gradeBook.recordGrade(ana, "Algoritmos",     4.2);

        // ── Notas de Carlos (riesgo académico) ───────────────────────
        System.out.println("\n=== Notas de Carlos ===");
        gradeBook.recordGrade(carlos, "Programación", 2.5);
        gradeBook.recordGrade(carlos, "Matemáticas",  1.8);
        gradeBook.recordGrade(carlos, "Algoritmos",   2.0);

        // ── Notas de Diana (promedio límite) ─────────────────────────
        System.out.println("\n=== Notas de Diana ===");
        gradeBook.recordGrade(diana, "Programación", 3.2);
        gradeBook.recordGrade(diana, "Matemáticas",  3.0);
        gradeBook.recordGrade(diana, "Algoritmos",   3.1);

        // ── Cálculo de promedios — AcademicRiskAspect actúa ──────────
        System.out.println("\n=== Cálculo de promedios ===");
        gradeBook.computeAverage(ana,    "2024-1");
        gradeBook.computeAverage(carlos, "2024-1");
        gradeBook.computeAverage(diana,  "2024-1");

        // ── Nota inválida — GradeValidationAspect actúa ──────────────
        System.out.println("\n=== Intento de registrar nota inválida ===");
        try {
            gradeBook.recordGrade(ana, "Física", 6.5);
        } catch (IllegalArgumentException e) {
            System.out.println("[ERROR] " + e.getMessage());
        }

        try {
            gradeBook.recordGrade(carlos, "Física", -1.0);
        } catch (IllegalArgumentException e) {
            System.out.println("[ERROR] " + e.getMessage());
        }
    }
}
