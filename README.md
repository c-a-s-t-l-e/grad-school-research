# Grad School Research

---

This repo serves as a public container for my research at the [University of Tennessee's Center for Transportation Research](https://ctr.utk.edu/).

It is oranized as follows:

phd-research/
├── README.md                # Overview of your PhD research themes & goals
├── LICENSE                  # License for open source work
├── CITATION.cff             # General citation for your PhD repo
├── CODE_OF_CONDUCT.md       # Community guidelines
├── CONTRIBUTING.md          # How collaborators can contribute
│
├── docs/                    # Centralized documentation
│   ├── overview.md          # Narrative summary of research directions
│   ├── methods/             # General methodological notes
│   ├── publications/        # Preprints, published papers, drafts
│   ├── presentations/       # Slides, posters, talks
│   └── references.bib       # Shared bibliography
│
├── projects/                # Each sub-project under your PhD
│   ├── project1-topic-name/
│   │   ├── README.md        # Project-specific overview
│   │   ├── data/
│   │   │   ├── raw/
│   │   │   └── processed/
│   │   ├── notebooks/
│   │   ├── src/
│   │   ├── tests/
│   │   └── results/
│   ├── project2-topic-name/
│   └── project3-topic-name/
│
├── shared/                  # Cross-cutting resources
│   ├── data/                # Reusable datasets across projects
│   ├── code/                # General utility scripts
│   ├── notebooks/           # Cross-project exploratory notebooks
│   └── templates/           # Paper templates, plotting styles, configs
│
├── environment.yml          # Global environment (or lockfile)
├── Makefile                 # Automate workflows (tests, docs, builds)
└── .gitignore
