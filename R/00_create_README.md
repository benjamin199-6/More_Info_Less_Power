Research Project Template
================

**Reproducible workflow for empirical research  
(LaTeX + Zotero + GitHub + Overleaf + R + OSF)**

------------------------------------------------------------------------

## 1. Purpose of this Repository

This repository provides a **reusable research project template**
designed to:

- avoid losing files and versions
- separate writing, references, code, and data cleanly
- ensure full reproducibility
- work across multiple machines
- integrate smoothly with GitHub, Overleaf, Zotero, R, and OSF
- allow returning to a project after months without confusion

**Core design principle**

> GitHub is the single source of truth.  
> All other tools read from or write to GitHub-managed files.

------------------------------------------------------------------------

## 2. Conceptual Workflow

### References

Zotero  
→ auto-export via Better BibTeX  
→ `paper/bibliography.bib`  
→ Git commit & push  
→ Overleaf compilation

### Analysis

R scripts  
→ tables & figures  
→ `paper/tables` and `paper/figures`  
→ included in LaTeX

------------------------------------------------------------------------

## 3. Repository Structure

research-project-template/ ├── paper/ │ ├── main.tex \# document
structure │ ├── preamble.tex \# packages, formatting, biblatex │ ├──
titlepage.tex \# authors and affiliations │ ├── bibliography.bib \#
Zotero auto-export (do not edit manually) │ ├── sections/ │ │ ├──
01_abstract.tex │ │ ├── 02_introduction.tex │ │ ├── 03_literature.tex │
│ ├── 04_data.tex │ │ ├── 05_empirical_strategy.tex │ │ └──
06_conclusion.tex │ ├── figures/ \# generated figures │ └── tables/ \#
generated tables │ ├── R/ \# R scripts ├── masterfile.R \# runs full
analysis pipeline │ ├── data/ │ ├── raw/ \# raw data (not committed) │
├── processed/ \# processed data (usually not committed) │ └── README.md
\# data documentation │ ├── OSF/ \# preregistration & OSF material │ ├──
.gitignore ├── research-project-template.Rproj └── README.md

------------------------------------------------------------------------

## 4. How This Repository Was Created

### Step 1 — Create GitHub repository

A new GitHub repository named `research-project-template` was created to
serve as a reusable template for future research projects.

### Step 2 — Clone locally

The repository was cloned to a fixed local directory to ensure stable
relative paths across machines.

### Step 3 — Create RStudio project

An `.Rproj` file was created inside the repository to support
reproducible R workflows and Git integration.

### Step 4 — Create LaTeX paper structure

A modular LaTeX setup was created locally:

- `main.tex` defines document structure only  
- content is split into `sections/*.tex`  
- formatting and bibliography are handled in `preamble.tex`

### Step 5 — Set up Zotero auto-export

Zotero (with Better BibTeX) was configured to auto-export references to:

Rules: - Zotero writes the file - the file is never edited manually -
citation keys are stable

### Step 6 — Commit to GitHub

All LaTeX files and the bibliography were committed, making GitHub the
single source of truth.

### Step 7 — Import into Overleaf

A new Overleaf project was created by importing the GitHub repository.  
Overleaf is used only as an editor/compiler, not as the canonical file
source.

### Step 8 — Add analysis and OSF scaffolding

Folders for R scripts, OSF preregistration, and data were added.  
Raw and processed data are excluded via `.gitignore`.

------------------------------------------------------------------------

## 5. Daily Workflow

### Writing text

- Edit locally or in Overleaf
- Structural changes are made locally and committed
- Overleaf pulls from GitHub

### Adding references

1.  Add reference in Zotero
2.  Zotero updates `paper/bibliography.bib`
3.  Commit and push the file
4.  Recompile in Overleaf

### Running analysis

- Execute `masterfile.R`
- Figures and tables are written to `paper/figures` and `paper/tables`
- Commit code, not raw data

------------------------------------------------------------------------

## 6. Design Philosophy

- Single source of truth (GitHub)
- Explicit interfaces between tools
- No hidden dependencies
- OSF- and journal-ready
- Easily cloneable for new projects

------------------------------------------------------------------------

## 7. Starting a New Paper

1.  Click **Use this template** on GitHub
2.  Rename the repository
3.  Update title, authors, and section files
4.  Start writing and analysing

------------------------------------------------------------------------

## 8. One-sentence Summary

**Zotero writes → GitHub stores → Overleaf reads → R computes**
rmarkdown::render(“README.Rmd”)
