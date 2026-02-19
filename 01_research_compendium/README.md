# Markup languages and reproducible programming in statistics

Deliverable 1: Reproducible research compendium.

Place the full contents of your research compendium in this directory. See course manual for requirements.
This is the research compendium for my thesis project. Since the data is confidential, for this compendium I am using a data that mimics the format and structure of my data. It is an open access, synthetic data that can be downloaded at https://www.kaggle.com/datasets/olaniyanjulius/essay-grading-dataset. It is also available in the compendium, with the full structure of:
 
Research Report/
│
├── data/
│   └── data.xlsx
│
├── docs/
│   ├── Research Report.qmd
│   ├── Research Report.pdf
│   └── references.bib
│
├── scripts/
│   └── text-preprocessing.R
│
├── README.md
├── LICENSE.md
├── renv
└── .gitignore
 
To get the same results as me, open the 'research report.Rproj' file, run renv::restore(), and run Research Report.qmd under docs folder.