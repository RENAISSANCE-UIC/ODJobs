## ODJobs: Automated Analysis of Microbial Growth Kinetics for Antimicrobial Susceptibility Testing

## Overview

ODJobs is a specialized R package designed for the automated processing and analysis 
of timestamped optical density data from 96-well plate readers, with particular 
focus on bacterial growth kinetics in antimicrobial susceptibility testing. Originally 
developed to support Minimum Inhibitory Concentration (MIC) determination studies 
involving nanomaterial antimicrobials, the package provides robust analytical workflows 
that are equally applicable to traditional small molecule antimicrobials.

To install:
The latest version can be found on [GitHub](https://github.com/CodeDepotIV/sTabl3R)](https://github.com/RENAISSANCE-UIC/ODJobs), and installed using `devtools`.

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("RENAISSANCE-UIC/ODJobs")
```

### Primary Applications
### Nanomaterial Antimicrobial Testing

- Correction methods to account for nanoparticle optical interference
- Time-matched nanoparticle control corrections handle dynamic particle behavior
- Contamination-aware algorithms detect problematic control wells
- Dual-correction workflows separate material effects from antimicrobial activity
- Temperature monitoring and stability assessment
- Flexible experimental layouts supporting various plate designs
- Support for multi-wavelength analysis 

### Traditional Antimicrobial MIC Determination

- Standard broth microdilution analysis with emphasis on quality control analytics
- Multiple correction algorithms for different experimental designs
- Automated detection and exclusion of contaminated wells

## Features

-   **Multi-format support**: Synergy HTX (Excel/CSV) and Cerillo Stratus (CSV/Excel) files
-   **Automatic plate reader detection**: Identifies file format automatically with manual override options
-   **Advanced correction methods**: Standard (contamination-safe), robust, traditional, and half-broth corrections
-   **Custom plate layouts**: Interactive Shiny interface or CSV import with flexible layout processing
-   **Multi-wavelength analysis**: Process data from one or two wavelengths simultaneously (Synergy HTX)
-   **Comprehensive visualization**: Growth curves, plate heatmaps, QC plots, and temperature profiles
-   **Data export**: Multiple formats optimized for downstream analysis and colleague sharing
-   **Quality control**: Automated diagnostics and contamination detection

## Dependencies

``` r
# Core packages (required)
library(tidyverse)  
library(lubridate)  
library(readxl)  
library(janitor)  
library(patchwork)    
library(writexl)    
library(shiny)    
library(readr)      
library(viridis)  
```

## Quick Start

### Example

``` r
library(ODJobs)

csv_path <- system.file("extdata", "plate_layout_20250915.csv", package = "ODJobs")
xlsx_path <- system.file("extdata", "Example_Data.xlsx", package = "ODJobs")

r1 <- analyze_growth_curves(
  file_path = xlsx_path,
  layout_csv = csv_path, 
  orientation = "vertical"
)

show_qc_plots(r1, wavelength = "600")
show_growth_curves(r1, wavelength = "600")
show_concentrations_panel(r1, wavelength = "600", max_time_hrs = 15)
show_plate_composite(r1, wavelength = "600")
show_plate_composite(r1, wavelength = "600", include_layout = F, max_time_hrs = 15)
```

### Basic Analysis With Custom Layout

``` r
library(ODJobs)

setup_workspace(working_directory = "<path_to_working_directory>")

layout_ev <- launch_plate_editor()

my_results <- analyze_growth_curves(
  file_path = "<path_to_file>",
  layout_csv = layout_ev,
  orientation = "vertical"
)

show_qc_plots(my_results)
show_growth_curves(my_results)
```

### Advanced Analysis Options

``` r
# Use specific correction methods
results_standard <- run_standard_analysis("data.xlsx")
results_robust <- run_robust_analysis("data.xlsx") 
results_traditional <- run_traditional_analysis("data.xlsx")

# Compare all methods
all_methods <- compare_correction_methods("data.xlsx")

# Custom parameters
results <- analyze_growth_curves("data.xlsx", 
                                correction_method = "standard",
                                max_time_hrs = 24,
                                orientation = "horizontal")
```

## Configuration

### Set Working Directory

``` r
# Default location  
setup_workspace()    

# Custom location  
setup_workspace("/path/to/your/project")    

# Different project name  
setup_workspace(project_subdir = "My_Growth_Curves")

# With enhanced options
setup_workspace(create = TRUE, 
                fallback_home_if_no_desktop = TRUE,
                verbose = TRUE)
```

## Examples

### Complete Workflow

``` r
# 1. Set up workspace  
setup_workspace()    

# 2. Run full analysis with summaries
results <- run_analysis("experiment_data.xlsx", export_summaries = TRUE)    

# 3. View results  
show_growth_curves(results)  
show_plate_endpoint(results)    
show_qc_plots(results)

# 4. Export data for colleagues
colleague_data <- get_colleague_format(results, export_to_csv = TRUE)    

# 5. Verify experimental layout
verify_experimental_layout(results$processed_data)
```

### Interactive Layout Design

``` r
# 1. Create custom layout interactively
layout_data <- launch_plate_editor(auto_capture = TRUE, return_data = TRUE)    

# 2. Run analysis with custom layout  
results <- analyze_growth_curves("data.xlsx", layout_csv = layout_data)   

# 3. Verify layout was applied correctly
verify_experimental_layout(results$processed_data)

# 4. Validate layout file
validate_layout_file("my_layout.csv")
```

### Multi-Wavelength Analysis

``` r
# Process multi-wavelength data  
multi_results <- analyze_growth_curves("multi_data.xlsx")    

# Access specific wavelengths  
show_growth_curves(multi_results, wavelength = "600")  
show_growth_curves(multi_results, wavelength = "420")    

# Export specific wavelength  
data_600nm <- get_colleague_format(multi_results, wavelength = "600")

# Show all wavelengths
show_growth_curves(multi_results, wavelength = "all")
```

## Data Structure and Object Access

### Single Wavelength Results Structure

``` r
results <- analyze_growth_curves("data.xlsx")

# Returns a structured S3 list object:  
results  
├── processed_data          # Raw processed data with layout
│   ├── metadata            # Experiment metadata from file
│   ├── data                # Raw data merged with layout
│   ├── layout              # Plate layout information
│   └── summary             # Basic data summary
├── corrected_results       # Corrected data and statistics
│   ├── raw_data            # Original uncorrected data
│   ├── corrected_data      # Fully corrected data
│   ├── replicate_stats     # Summary statistics by replicate
│   └── corrections         # Correction values applied
├── plots                   # All visualization objects
│   ├── growth_curves       # Main growth curve plot
│   ├── initial_plate       # Initial plate heatmap
│   ├── final_plate         # Final plate heatmap
│   ├── plate_composite     # Before/after comparison
│   ├── concentrations_panel # Concentration panel plot
│   └── qc_plots            # Quality control plots
├── correction_method       # Method used
├── threshold               # Whether thresholding was applied
├── max_time_hrs           # Time cutoff applied (if any)
└── wavelength              # Wavelength analyzed (if applicable)
```

### Multi-Wavelength Results Structure

``` r
multi_results <- analyze_growth_curves("multi_data.xlsx")  

# Returns a named list with one entry per wavelength:    
multi_results
├── wavelength_600          # 600nm results
│   ├── processed_data      # Same structure as single wavelength
│   ├── corrected_results   # Same structure as single wavelength
│   ├── plots               # Same structure as single wavelength
│   └── wavelength          # "600"
└── wavelength_420          # 420nm results (if present)
    └── [same structure]
```

## Correction Methods

### Standard Method (Recommended - Contamination-Safe)

-   **Contamination detection**: Automatically detects and excludes contaminated broth controls
-   **Time-matched NP correction**: Accounts for nanoparticle kinetics by concentration
-   **Robust statistics**: Uses median-based corrections for stability

``` r
results <- run_standard_analysis("data.xlsx")
# or
results <- analyze_growth_curves("data.xlsx", correction_method = "standard")
```

### Half-Broth Method (For Half-Broth Experiments)

-   **Two-stage processing**: Raw NP correction for samples, half-broth baseline for controls
-   **Contamination-aware**: Detects problematic half-broth wells
-   **Automatic detection**: Triggered when half-broth wells are present in layout

``` r
# Automatically used when half-broth wells detected in custom layout
results <- analyze_growth_curves("data.xlsx", layout_csv = "halfbroth_layout.csv")
```

### Robust Method

-   **Outlier detection**: Identifies and excludes problematic wells using MAD statistics
-   **Median-based corrections**: Less sensitive to extreme values
-   **Enhanced quality control**: Multiple validation steps

``` r
results <- run_robust_analysis("data.xlsx")
```

### Traditional Method

-   **Simple approach**: Single broth well correction
-   **Raw NP values**: Direct nanoparticle subtraction
-   **No thresholding**: Allows negative values

``` r
results <- run_traditional_analysis("data.xlsx")
```

## Interactive Layout Editor (Shiny App)

``` r
# Launch interactive editor with auto-capture
layout_data <- launch_plate_editor(auto_capture = TRUE, return_data = TRUE)   

# Manual mode (download CSV manually)
launch_plate_editor(auto_capture = FALSE)

# Use captured layout  
results <- analyze_growth_curves("data.xlsx", layout_csv = layout_data)
```

### Layout Editor Features

-   **96-well plate visualization**: Interactive plate with drag-and-drop
-   **Multiple sample types**: Experiment, NP Control, Positive Control, Broth, Half-Broth, Blank
-   **Concentration series**: Automatic numbering and validation
-   **Replicate management**: Define biological and technical replicates
-   **Auto-capture**: Automatically saves when you close the app
-   **CSV export/import**: Save and load custom layouts

## File Format Support

### Synergy HTX (Agilent BioTek)

-   **Excel files**: .xlsx, .xls with automatic metadata extraction
-   **CSV files**: Exported from BioTek Synergy software
-   **Multi-wavelength**: Automatic detection of 420nm, 600nm, and other wavelength sections
-   **Time formats**: Decimal hours and HH:MM formats supported

### Cerillo Stratus

-   **CSV/Excel formats**: Direct export from Cerillo software
-   **Single wavelength**: 600nm optical density
-   **Timestamp support**: Unix timestamps and formatted dates
-   **Automatic detection**: Pattern-based file format recognition

### Format Detection

``` r
# Check file format before processing
detected_type <- detect_plate_reader_with_preview("data.csv")

# Manual override if needed
results <- force_plate_reader_type("data.csv", "cerillo")
```

## Data Export and Sharing

### For Colleagues (Wide Format)

``` r
# Export current method results
colleague_data <- get_colleague_format(results, export_to_csv = TRUE)    

# Multi-wavelength export
all_wavelengths <- get_colleague_format(multi_results, wavelength = "all", export_to_csv = TRUE)

# Custom options
data_600 <- get_colleague_format(results, 
                                wavelength = "600",
                                include_untreated = TRUE,
                                export_to_csv = FALSE)
```

### Specific Data Extraction

``` r
# Extract specific concentration data  
conc_data <- get_concentration_data(results, "Conc_5", format = "wide")  

# Plot specific concentrations
plot_concentration(results, "Conc_5", style = "with_ribbons")
show_concentrations_panel(results, concentrations = c("Conc_1", "Conc_5", "Conc_10"))

# Get raw or corrected data
raw_data <- get_raw_data(results)    
corrected_data <- get_corrected_data(results)

# Multiple export formats
wide_data <- export_wide_format(results, data_type = "corrected")
untreated_data <- export_untreated_wide(results)
```

## Visualization

### Growth Curves

``` r
# Basic growth curves with error bars
show_growth_curves(results)    

# Time-limited view
show_growth_curves(results, max_time_hrs = 16)

# Specific concentrations panel
show_concentrations_panel(results)  
show_concentrations_panel(results, free_scale = TRUE)

# Individual concentration plots
plot_concentration(results, "Conc_10", style = "separated")
plot_concentration(results, "Conc_5", style = "with_ribbons")
```

### Plate Layouts and Heatmaps

``` r
# Final plate layout
show_plate_endpoint(results)    

# Composite comparison (raw vs corrected)
show_plate_composite(results)    
show_plate_composite(results, include_layout = TRUE)

# Initial conditions  
show_plate_initial_condition(results)
```

### Quality Control Visualizations

``` r
# Comprehensive QC plots  
show_qc_plots(results)    

# Temperature monitoring
show_temperature(results)
check_temperature(results)

# Layout verification  
verify_experimental_layout(results$processed_data)

# Data diagnostics
diagnose_data_issues(results$corrected_results)
```

### Temperature Monitoring

``` r
# Temperature profile plots
show_temperature(results)
plot_temperature(results, style = "detailed", qc_checks = TRUE)

# Temperature quality check
temp_stats <- check_temperature(results)

# Multi-wavelength temperature
show_temperature(multi_results, wavelength = "600")
```

## Additional Features

### Example Experimental Layout (96-wells)

```{=html}

     01  02  03  04  05  06  07  08  09  10  11  12
 A   B   S1  S2  S3  S4  S5  S6  S7  S8  S9  S10 B
 B   B   S1  S2  S3  S4  S5  S6  S7  S8  S9  S10 B  
 C   B   S1  S2  S3  S4  S5  S6  S7  S8  S9  S10 B
 D   B   N1  N2  N3  N4  N5  N6  N7  N8  N9  N10 B
 E   B   BL  BL  BL  BL  BL  BL  BL  BL  BL  BL  B
 F   B   BL  BL  BL  BL  BL  BL  BL  BL  BL  BL  B
 G   B   BL  BL  BL  BL  BL  BL  BL  BL  BL  BL  B
 H   B   U1  U2  U3  U4  U5  U6  U7  U8  U9  U10 B

Legend:
- S = Sample (A=Rep1, B=Rep2, C=Rep3)
- N = NP-only control
- U = Untreated control
- B = Broth control
- BL = Blank 
- Columns 2-11 = Concentrations 1-10
```

### Time Management (Thresholds)

``` r
# Apply time cutoffs
results <- analyze_growth_curves("data.xlsx", max_time_hrs = 24)

# Time-limited visualizations
show_growth_curves(results, max_time_hrs = 16)
show_plate_composite(results, max_time_hrs = 12)
```

### Custom Workflows

``` r
# Full workflow with custom parameters
results <- run_analysis("data.xlsx", 
                       layout_csv = "custom_layout.csv",
                       method = "standard", 
                       export_summaries = TRUE,
                       max_time_hrs = 20)

# Diagnostic workflow
quick_check(results)
diagnose_problematic_wells(results$corrected_results)

# Multi-method comparison
all_methods <- compare_correction_methods("data.xlsx", export_summaries = TRUE)
```

## Troubleshooting

### File Format Issues

``` r
# Check file format with preview
detect_plate_reader_with_preview("data.csv")    

# Force specific format if auto-detection fails
results <- force_plate_reader_type("data.csv", "cerillo")

# Test Cerillo compatibility
test_cerillo_compatibility("cerillo_file.csv", "layout.csv")
```

### Layout Problems

``` r
# Verify layout was applied correctly
verify_experimental_layout(results$processed_data)    

# Validate custom layout file before use
validate_layout_file("custom_layout.csv")
```

### Check replicate structure

``` r
check_replicate_structure(results)
```

### Data Quality Issues

\`\`\`r# Comprehensive diagnostics diagnose_data_issues(results\$corrected_results)

# Check for problematic wells

diagnose_problematic_wells(results\$corrected_results)

# Quick data overview

quick_check(results) explore_data(results\$corrected_results)

```         

### Common Solutions

-   Negative Values: Try correction_method = "traditional", threshold = FALSE
-   High Variability: Use correction_method = "robust"
-   Contaminated Broth: Use correction_method = "standard" (automatically detects contamination)
-   Missing NP Controls: Package handles this automatically, no NP correction applied
-   File Detection Issues: Use force_plate_reader_type() or check file format

### Support and Diagnostics

### For comprehensive analysis support:
```r# File format help
detect_plate_reader_with_preview("your_file.csv")

# Data structure exploration  
quick_check(results)
explore_data(results$corrected_results)

# Layout validation
verify_experimental_layout(results$processed_data)

# Quality control
show_qc_plots(results)
diagnose_data_issues(results$corrected_results)

# Temperature monitoring
check_temperature(results)
```
