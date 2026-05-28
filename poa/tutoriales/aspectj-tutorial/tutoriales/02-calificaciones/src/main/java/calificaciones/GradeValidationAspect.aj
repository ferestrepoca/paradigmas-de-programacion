package calificaciones;

/**
 * Aspecto 1 — Validación de rango de notas (advice tipo before + args).
 *
 * Intercepta GradeBook.recordGrade() antes de registrar.
 * Captura los tres argumentos con args(s, subject, grade) y valida
 * que la nota esté dentro del rango permitido 0.0 – 5.0.
 *
 * Si la nota es inválida, lanza IllegalArgumentException y el registro
 * no ocurre. GradeBook.java no contiene ninguna validación.
 */
public aspect GradeValidationAspect {

    pointcut gradeRecording(Student s, String subject, double grade) :
        execution(void GradeBook.recordGrade(Student, String, double))
        && args(s, subject, grade);

    before(Student s, String subject, double grade)
            : gradeRecording(s, subject, grade) {
        if (grade < 0.0 || grade > 5.0) {
            System.out.printf("[VALIDACION] Nota rechazada — %.1f fuera del rango [0.0, 5.0]%n", grade);
            throw new IllegalArgumentException(
                    "Nota inválida: " + grade + " para " + s.getName()
                    + " en " + subject + ". Rango permitido: 0.0 – 5.0");
        }
    }
}
