# 🔍 Tutorial AspectJ — Aspect-Oriented Programming

> **Course:** Programming Languages — Programming Paradigms
> **Universidad Nacional de Colombia**
> **Format:** Interactive tutorial + university presentation
> **Duration:** 35–40 minutes

---

## 👥 Integrantes del Grupo

| Nombre |
|--------|
| Adrian Yebid Rincon |
| Juan Esteban Ruiz Guasca |
| Yeiner Arwawingumu Zapata Vallejo |
| Cesar Felipe Pineda Ortiz |

---

## 📋 Table of Contents

1. [Introduction](#introduction)
2. [Objectives](#objectives)
3. [System Requirements](#system-requirements)
4. [Getting Started](#getting-started)
5. [Repository Structure](#repository-structure)
6. [Code Examples](#code-examples)
7. [Guided Tutorials](#guided-tutorials)
8. [How to Run](#how-to-run)
9. [References](#references)

---

## 📖 Introduction

**Aspect-Oriented Programming (AOP)** is a paradigm that complements OOP by separating **cross-cutting concerns** from core business logic.

Imagine 50 classes that all need logging, timing, authentication, and error handling. With pure OOP you repeat that code in all 50 classes. With AOP you define it **once in an Aspect** and it applies automatically wherever you need it.

**AspectJ** is the most complete and mature AOP implementation for Java — the industry standard and the foundation of Spring AOP.

---

## 🎯 Objectives

By the end of this tutorial, students will be able to:

- ✅ Explain what AOP is and what problem it solves
- ✅ Distinguish the 5 key concepts: Aspect, Join Point, Pointcut, Advice, Weaving
- ✅ Understand `before`, `after`, `after returning`, `after throwing`, and `around` advice
- ✅ Use `call()` vs `execution()` pointcut designators correctly
- ✅ Capture method arguments with `args()`
- ✅ Inject fields and methods into existing classes with Inter-Type Declarations (ITD)
- ✅ Make classes implement interfaces via aspects with `declare parents`
- ✅ Set up AspectJ with Maven and run real AspectJ projects

---

## 💻 System Requirements

| Tool | Minimum version | Notes |
|------|----------------|-------|
| Java JDK | 17 | LTS recommended |
| Maven | 3.6+ | Dependency management |
| IDE | Any | IntelliJ IDEA recommended |
| AspectJ | 1.9.21 | Downloaded automatically via Maven |
| Git | Any | To clone the repository |

---

## ⚙️ Getting Started

### 1. Verify Java and Maven

```bash
java -version   # should show 17.x.x or higher
mvn -version    # should show 3.x.x
```

### 2. Clone the repository

```bash
git clone https://github.com/adrianyebid/Tutorial-AspectJ.git
cd Tutorial-AspectJ
```

### 3. Open the interactive tutorial

Open `index.html` directly in any browser — no server needed.

### 4. Run a code example

```bash
cd ejemplos/01-cajero
mvn clean compile exec:java
```

### 5. IntelliJ IDEA tip

Open any `ejemplos/` or `tutoriales/` subfolder as a Maven project.
Install the **AspectJ Support** plugin (`Settings → Plugins → Marketplace`) for `.aj` file syntax highlighting.

---

## 📁 Repository Structure

```
aspectj-tutorial/
│
├── index.html                  ← Interactive tutorial (open in browser)
├── presentation.md             ← Marp slide deck
│
├── ejemplos/                   ← 7 standalone Maven projects (one concept each)
│   ├── 01-cajero/              ← before / after
│   ├── 02-restaurante/         ← wildcards * and ..
│   ├── 03-trafico/             ← call() vs execution()
│   ├── 04-agenda/              ← Inter-Type Declarations (ITD)
│   ├── 05-termostato/          ← declare parents
│   ├── 06-formulario/          ← args() + after throwing
│   └── 07-procesador/          ← around + proceed()
│
└── tutoriales/                 ← 3 complete systems (multiple aspects combined)
    ├── 01-biblioteca/          ← Library: DebtCheck + LoanLimit + ActivityLog
    ├── 02-calificaciones/      ← Grade book: GradeValidation + AcademicRisk
    └── 03-cache/               ← Product catalog: CacheAspect (around + after)
```

---

## 🧩 Code Examples

Each folder in `ejemplos/` is a self-contained Maven project focused on one AspectJ concept.

| # | Project | Concept | Aspect file |
|---|---------|---------|-------------|
| 01 | `cajero` | `before` / `after` — transparent auditing | `AuditoriaAspect.aj` |
| 02 | `restaurante` | Wildcards `preparar*` and `!` negation | `OrdenAspect.aj` |
| 03 | `trafico` | `call()` vs `execution()` with inheritance | `SensorTrafico.aj` |
| 04 | `agenda` | ITD — inject fields and methods | `PrioridadAspect.aj` |
| 05 | `termostato` | `declare parents` — implement interface via aspect | `DiagnosticoAspect.aj` |
| 06 | `formulario` | `args()` capture + `after() throwing` | `ValidacionAspect.aj` |
| 07 | `procesador` | `around` + `proceed()` — cache pattern | `ResultadoCacheAspect.aj` |

---

## 📚 Guided Tutorials

Each folder in `tutoriales/` is a realistic system that combines several AspectJ techniques.

### 01 — Library (`tutoriales/01-biblioteca/`)

A library loan system where three aspects work together on `Library.java` without modifying it:

- `DebtCheckAspect.aj` — `before + args`: blocks loans if member has outstanding debt
- `LoanLimitAspect.aj` — `around`: blocks if member already has ≥ 3 active loans
- `ActivityLogAspect.aj` — `after returning`: logs only successful operations

### 02 — Grade Book (`tutoriales/02-calificaciones/`)

Academic grade management with two validation layers:

- `GradeValidationAspect.aj` — `before + args(s, subject, grade)`: rejects grades outside 0.0–5.0
- `AcademicRiskAspect.aj` — `after returning(double avg)`: alerts when student average drops below 3.0

### 03 — Cache (`tutoriales/03-cache/`)

Product catalog with simulated 80 ms DB latency. `ProductCatalog.java` knows nothing about the cache:

- `CacheAspect.aj` — `around`: returns cached result without calling `proceed()` on hit
- `CacheAspect.aj` — `after`: invalidates cache entry when price is updated

---

## 🚀 How to Run

Every project follows the same command:

```bash
cd <project-folder>
mvn clean compile exec:java
```

Examples:

```bash
# Example — around advice + cache
cd ejemplos/07-procesador
mvn clean compile exec:java

# Tutorial — library system (3 aspects combined)
cd tutoriales/01-biblioteca
mvn clean compile exec:java

# Tutorial — cache system
cd tutoriales/03-cache
mvn clean compile exec:java
```

---

## 📖 References

1. Kiczales, G. et al. (1997). *Aspect-Oriented Programming*. ECOOP.
2. Kiczales, G. et al. (2001). *An Overview of AspectJ*. ECOOP.
3. Laddad, R. (2003). *AspectJ in Action*. Manning Publications.
4. Eclipse Foundation. *The AspectJ Programming Guide*. https://www.eclipse.org/aspectj/doc/released/progguide/
5. Spring Framework. *Aspect Oriented Programming with Spring*. https://docs.spring.io/spring-framework/reference/core/aop.html
6. Eclipse Foundation. *AspectJ 1.9.21 Release Notes*. https://www.eclipse.org/aspectj/

---

*Tutorial created for the Programming Languages course — Universidad Nacional de Colombia*
