---
editor_options: 
  markdown: 
    wrap: 72
---

# R Project Template

A standardized template for organizing R data analysis projects with
built-in Git LFS support for large files.

## Repository Structure

```
Project/
├── 01_data/
│   └── 01_large_files/            # Large datasets tracked via Git LFS
├── 02_scripts/
│   └── 01_functions/              # Reusable R functions
├── 03_outputs/
│   └── 01_figures/                # Generated plots and visualizations
├── 04_notes/                      # Project documentation and guides
├── .gitignore                     # Files excluded from version control
├── .gitattributes                 # Git LFS configuration
└── README.md                      # This file
```

## Quick Start

### 1. Clone or Copy This Template

``` bash
# If using as a template for a new project
git clone <this-repo-url> <your-project-name>
cd <your-project-name>
```

### 2. Set Up Git LFS (One-Time Setup)

If you haven't installed Git LFS before:

``` bash
# Install Git LFS (Windows - included with Git for Windows)
git lfs version  # Verify installation

# Configure Git LFS on your machine (once per user)
git lfs install
```

**For detailed installation and troubleshooting**, see:
[`04_notes/git_lfs_setup.md`](04_notes/git_lfs_setup.md)

### 3. Start Working

-   Add data files to `01_data/` (large files go in
    `01_large_files/`)
-   Create analysis scripts in `02_scripts/`
-   Store custom functions in `02_scripts/01_functions/`
-   Generated outputs automatically go to `03_outputs/`

## Directory Descriptions

### `01_data/`

Store all input data files here: - **Raw data**: Original, unmodified
datasets - **Processed data**: Cleaned or transformed datasets -
**`01_large_files/`**: Large datasets automatically tracked via Git
LFS

### `02_scripts/`

Analysis and processing scripts: - **Main scripts**: Numbered workflows
(scriptXX-YY format) - **`01_functions/`**: Reusable functions
sourced by multiple scripts - Organize scripts by dependency order using
numbered prefixes

### `03_outputs/`

Generated results and visualizations: - **`01_figures/`**: Plots,
maps, and visualizations - **Tables**: Summary statistics and model
outputs - **Reports**: Rendered R Markdown documents

*Note: Output files are gitignored by default to ensure reproducibility*

### `04_notes/`

Project documentation and guides: - Analysis notes and decisions -
Methodology documentation - Setup guides 

## Git Configuration

### What's Tracked

-   All R scripts (`.R`, `.Rmd`)
-   Data files in `01_data/` (via Git LFS for large files)
-   Project configuration files (`.Rproj`, `.gitignore`,
    `.gitattributes`)
-   Documentation (`.md` files)

### What's Ignored

See `.gitignore` for complete list: - R session files (`.RData`,
`.Rhistory`, `.Rproj.user/`) - Generated outputs (`.png`, `.pdf`,
`.tiff`, etc.) - Temporary objects

### Git LFS

Files in `01_data/01_large_files/` are automatically tracked via Git
LFS: - Prevents repository bloat from large datasets - Maintains version
control for large files - Works transparently once configured

**See [`04_notes/git_lfs_setup.md`](04_notes/git_lfs_setup.md)
for complete setup instructions**

## Coding Conventions

This template follows standardized R coding practices for file naming,
folder organization, and R object naming.

### Naming Convention Overview

**Files and Folders:**

-   Scripts: `scriptXX-YY_descriptive_name.R` (e.g.,
    `script02-01_analysis_rf.R`)
-   Functions: `functionXX-YY_descriptive_name.R` (e.g.,
    `function01-01_helper_functions.R`)
-   Folders: `XX_descriptive_name/` (e.g., `01_data/`, `02_scripts/`)
-   All lowercase with underscores

**R Objects:**

-   Raw data: `data_*` (e.g., `data_habitat`)
-   Processed data: `df_*` (e.g., `df_habitat_clean`)
-   Parameters: `param_*` (e.g., `param_seed`)
-   Temporary: `temp_*` (e.g., `temp_filtered`)

**See
[`04_notes/naming_conventions.md`](04_notes/naming_conventions.md) for
complete details.**

## Workflow Recommendations

### Script Organization

-   **script00-XX**: Setup (packages, functions, configuration)
-   **script01-XX**: Data import and formatting
-   **script02-XX**: Analysis workflows
-   **script03-XX**: Visualization and reporting
-   **script10-XX**: Special analyses or extensions

### Master Control Script

Use `script00-00_user_interface.R` to orchestrate your workflow:

-   Source package loading script
-   Load data objects
-   Execute analysis scripts in dependency order
-   Provide workflow documentation

### Version Control

1.  **Commit often** with descriptive messages
2.  **Document changes** in script modification notes
3.  **Use branches** for experimental analyses

## Additional Documentation

-   [**Naming Conventions**](04_notes/naming_conventions.md): Complete
    guide to file, folder, and object naming
-   [**Git LFS Setup**](04_notes/git_lfs_setup.md): Installation,
    configuration, and troubleshooting

### External Resources

-   [R for Data Science](https://r4ds.had.co.nz/)
-   [Git LFS Documentation](https://git-lfs.github.com/)
-   [Happy Git with R](https://happygitwithr.com/) 
