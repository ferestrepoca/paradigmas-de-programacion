package cajero;

/**
 * Ejemplo 1 — before / after
 *
 * Cajero automático que gestiona saldo.
 * No contiene ninguna lógica de auditoría ni log.
 * AuditoriaAspect.aj intercepta todas las operaciones de forma transparente.
 */
public class CajeroAutomatico {

    private double saldo;

    public CajeroAutomatico(double saldoInicial) {
        this.saldo = saldoInicial;
    }

    public void depositar(double monto) {
        if (monto <= 0) throw new IllegalArgumentException("El monto debe ser positivo");
        saldo += monto;
        System.out.println("  Depósito realizado. Saldo actual: $" + saldo);
    }

    public void retirar(double monto) {
        if (monto <= 0) throw new IllegalArgumentException("El monto debe ser positivo");
        if (monto > saldo) throw new IllegalStateException("Fondos insuficientes");
        saldo -= monto;
        System.out.println("  Retiro realizado. Saldo actual: $" + saldo);
    }

    public double getSaldo() {
        return saldo;
    }
}
