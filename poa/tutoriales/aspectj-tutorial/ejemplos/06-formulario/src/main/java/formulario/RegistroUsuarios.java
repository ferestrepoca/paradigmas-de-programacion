package formulario;

import java.util.ArrayList;
import java.util.List;

/**
 * Ejemplo 6 — args() + after() throwing.
 *
 * Registro de usuarios sin ninguna validación interna.
 * ValidacionAspect.aj usa args() para capturar los parámetros
 * del método y validarlos antes de que se ejecute.
 * También captura cualquier excepción lanzada con after() throwing.
 */
public class RegistroUsuarios {

    private final List<String> usuarios = new ArrayList<>();

    public void registrar(String nombre, String email, int edad) {
        usuarios.add(nombre);
        System.out.println("  ✓ Usuario registrado: " + nombre
                + " <" + email + ">  edad: " + edad);
    }

    public void eliminar(String nombre) {
        if (!usuarios.remove(nombre)) {
            throw new IllegalArgumentException("Usuario no encontrado: " + nombre);
        }
        System.out.println("  ✓ Usuario eliminado: " + nombre);
    }

    public int total() {
        return usuarios.size();
    }
}
