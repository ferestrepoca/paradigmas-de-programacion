package calificaciones;

/**
 * Aspecto 2 — Alerta de riesgo académico (advice tipo after returning).
 *
 * Se dispara SÓLO si computeAverage() terminó sin excepción y captura
 * el valor de retorno (double avg) usando 'returning(avg)'.
 *
 * Si el promedio es menor a 3.0, imprime una alerta.
 * GradeBook.java no contiene ninguna lógica de alerta.
 */
public aspect AcademicRiskAspect {

    pointcut averageComputed(Student s) :
        execution(double GradeBook.computeAverage(Student, String))
        && args(s, *);

    after(Student s) returning(double avg) : averageComputed(s) {
        if (avg < 3.0) {
            System.out.printf("[ALERTA] %s está en RIESGO ACADÉMICO. Promedio: %.2f%n",
                    s.getName(), avg);
        } else if (avg < 3.5) {
            System.out.printf("[INFO]   %s promedio aceptable: %.2f%n", s.getName(), avg);
        } else {
            System.out.printf("[INFO]   %s promedio sobresaliente: %.2f%n", s.getName(), avg);
        }
    }
}
