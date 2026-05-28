package calificaciones;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Lógica de negocio — registra notas y calcula promedios.
 *
 * NO contiene validación de rango de notas ni alertas de riesgo académico.
 * Esas responsabilidades las manejan los aspectos:
 *   GradeValidationAspect → before: valida que la nota esté entre 0.0 y 5.0
 *   AcademicRiskAspect    → after returning: alerta si el promedio < 3.0
 */
public class GradeBook {

    private final Map<String, List<Double>> grades = new HashMap<>();

    public void recordGrade(Student student, String subject, double grade) {
        grades.computeIfAbsent(student.getId(), k -> new ArrayList<>()).add(grade);
        System.out.printf("[LIBRO] Nota registrada — %s | %s: %.1f%n",
                student.getName(), subject, grade);
    }

    public double computeAverage(Student student, String semester) {
        List<Double> studentGrades =
                grades.getOrDefault(student.getId(), Collections.emptyList());
        if (studentGrades.isEmpty()) return 0.0;
        double avg = studentGrades.stream()
                .mapToDouble(Double::doubleValue)
                .average()
                .orElse(0.0);
        System.out.printf("[LIBRO] Promedio calculado — %s (%s): %.2f%n",
                student.getName(), semester, avg);
        return avg;
    }
}
