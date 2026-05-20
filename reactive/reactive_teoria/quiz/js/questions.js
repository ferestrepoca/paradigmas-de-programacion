var questions = [
    { q: "1. ¿Cuál es la definición fundamental de la programación reactiva según la presentación?",
      choices: [
        "A) Un paradigma basado en el almacenamiento secuencial de datos en bases de datos relacionales.",
        "B) Un modelo de ejecución imperativo donde se consulta de forma constante si el estado ha cambiado.",
        "C) Un paradigma de programación asíncrona orientado al flujo de datos y a la propagación del cambio.",
        "D) Una extensión de la programación estructurada orientada exclusivamente al paralelismo de hardware."
      ], answer: 2
    },
    { q: "2. En el contexto de los componentes clave, ¿cuál es la responsabilidad principal del Subscriber (Suscriptor)?",
      choices: [
        "A) Definir cómo se debe transformar o mutar un stream de datos.",
        "B) Emitir la secuencia de eventos de manera continua a lo largo del tiempo.",
        "C) Almacenar temporalmente los errores del sistema en un búfer de memoria.",
        "D) Suscribirse al Observable, escuchar los eventos y reaccionar ante ellos."
      ], answer: 3
    },
    { q: "3. ¿En qué consiste la filosofía o estrategia de propagación denominada Push (Empuje)?",
      choices: [
        "A) El suscriptor es quien solicita explícitamente los datos a la fuente cuando los necesita.",
        "B) La fuente envía activamente los eventos a los suscriptores tan pronto como están disponibles.",
        "C) El consumidor bloquea el hilo de ejecución hasta que la fuente se destruye automáticamente.",
        "D) Una notificación que obliga al cliente a borrar toda su caché antes de consultar de nuevo."
      ], answer: 1
    },
    { q: "4. Según el Manifiesto Reactivo, ¿qué característica permite que una aplicación se adapte dinámicamente a cambios en la carga de trabajo, escalando sus recursos?",
      choices: ["A) Resiliente","B) Elástica","C) Responsiva","D) Orientada a Mensajes"], answer: 1
    },
    { q: "5. ¿Cuál es el propósito principal del operador de control avanzado debounce?",
      choices: [
        "A) Esperar a que el flujo deje de emitir eventos durante un tiempo determinado antes de actuar.",
        "B) Limitar la frecuencia de eventos a exactamente uno por cada intervalo fijo, ignorando los intermedios.",
        "C) Cancelar la petición asíncrona anterior y lanzar una nueva inmediatamente al recibir cualquier evento.",
        "D) Multiplicar el número de eventos emitidos por el Observable para duplicar el procesamiento."
      ], answer: 0
    },
    { q: "6. ¿Qué problema crítico define el concepto de Backpressure (Contrapresión)?",
      choices: [
        "A) Cuando el consumidor procesa los datos más rápido de lo que el productor puede emitirlos.",
        "B) Cuando un error asíncrono no es capturado y destruye toda la base de datos.",
        "C) Cuando el productor emite datos más rápido de lo que el consumidor puede procesarlos.",
        "D) Cuando el canal de comunicación por red se cierra abruptamente por problemas de hardware."
      ], answer: 2
    },
    { q: "7. ¿Cómo soluciona la estrategia Drop el problema de la contrapresión?",
      choices: [
        "A) Almacena de forma indefinida todos los eventos en la memoria RAM hasta que se desborde.",
        "B) Envía una señal al productor para que reduzca activamente su velocidad de emisión.",
        "C) Duplica de inmediato los hilos de ejecución asignados al servidor para absorber la carga.",
        "D) Descarta o ignora los nuevos eventos entrantes mientras el consumidor esté ocupado."
      ], answer: 3
    },
    { q: "8. De acuerdo con la presentación, ¿qué ventaja técnica clave ofrece la programación reactiva en sistemas de alta concurrencia?",
      choices: [
        "A) Permite realizar operaciones CRUD de manera estrictamente secuencial y síncrona.",
        "B) Al operar de forma no bloqueante y orientada a eventos, un número reducido de hilos puede manejar una gran cantidad de peticiones de forma eficiente.",
        "C) Garantiza que el código sea idéntico al paradigma imperativo, facilitando las pruebas tradicionales.",
        "D) Elimina por completo la necesidad de usar librerías externas o frameworks en el despliegue."
      ], answer: 1
    },
    { q: "9. ¿Cuál es una de las desventajas documentadas al implementar este paradigma?",
      choices: [
        "A) Un alto consumo innecesario de almacenamiento físico en discos de estado sólido.",
        "B) La imposibilidad absoluta de conectarse de forma nativa a dispositivos de Internet de las Cosas (IoT).",
        "C) Una severa limitación que impide el uso de funciones lambda o flecha en el código.",
        "D) Una curva de aprendizaje alta y un debugging (depuración) más complejo debido a la naturaleza asíncrona de los flujos."
      ], answer: 3
    },
    { q: "10. ¿Qué función cumple el operador onErrorResume visto en los ejemplos prácticos?",
      choices: [
        "A) Detener el servidor inmediatamente y generar un volcado completo de la memoria RAM.",
        "B) Forzar al cliente a reintentar la misma petición HTTP de forma infinita cada milisegundo.",
        "C) Definir una estrategia de recuperación retornando un flujo alternativo (como un flujo vacío o datos por defecto) si ocurre un fallo.",
        "D) Validar sintácticamente que los tipos de datos mapeados no contengan valores nulos."
      ], answer: 2
    }
];

if (typeof module !== 'undefined' && module.exports) module.exports = questions;
