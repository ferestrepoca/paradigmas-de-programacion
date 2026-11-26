# Presentación de Mercury

Presentación hecha con reveal.js y un tema propio.

## Archivos

| Archivo | Para qué sirve |
|---|---|
| `mercury.html` | La presentación real (aquí se trabaja) |
| `index.html` | Catálogo de componentes listos para copiar |
| `mercury-theme.css` | Colores, tipografía y estilos compartidos |
| `dist/`, `plugin/`, `css/` | Framework (no modificar) |

## Cómo verla

Desde esta carpeta (`reveal.js`):

```
npm install   # solo la primera vez
npm start
```

Se abre en `http://localhost:8000`: `/` muestra la plantilla (`index.html`) y `/mercury.html` la presentación.

Atajos útiles: `F` pantalla completa, `S` vista de orador, `Esc` vista general, `?` ayuda.

## Cómo añadir diapositivas

1. Abrir `index.html` y copiar el bloque `<section>` que necesites.
2. Pegarlo dentro de `<div class="slides">` en `mercury.html`.
3. Cambiar el contenido. No hace falta tocar el CSS.

## Reglas para trabajar en equipo

- Cada persona trabaja su propia sección delimitada con comentarios:
  `<!-- ===== SECCIÓN: Nombre (autor) ===== -->`. Así se reducen los conflictos al unir cambios.
- Una idea por diapositiva, máximo 6 líneas de texto.
- Si falta un estilo, agrégalo en `mercury-theme.css` y avisa al equipo.

## Paleta

| Color | Hex | Uso |
|---|---|---|
| Violeta | `#8b5cf6` | Principal, tarjetas |
| Cian | `#22d3ee` | Énfasis, subtítulos |
| Rosa | `#f472b6` | Palabras clave, viñetas |
| Ámbar | `#fbbf24` | Negritas, etiquetas |
| Verde | `#34d399` | Código, determinismo |
| Fondo | `#0f1022` | Fondo base |

Tipografías: **Space Grotesk** (texto) y **JetBrains Mono** (código).

## Componentes disponibles

- Título con degradado: `<h1 class="grad">`
- Etiqueta superior: `<span class="kicker">`
- Tarjetas: `.grid.cols-2` / `.grid.cols-3` con `.card.c-violet|c-cyan|c-pink|c-amber|c-green`
- Etiquetas: `.pill`
- Texto secundario: `.muted`
- Código: `<pre><code>` con spans `k` (clave), `t` (tipo), `p` (predicado), `m` (modo), `s` (literal), `c` (comentario)
- Aparición progresiva: clase `fragment`
