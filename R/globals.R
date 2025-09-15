utils::globalVariables(c(
  "A01", "Category", "Concentration_Value", "H12", "across", "aes", "annotate",
  "any_of", "auc", "baseline", "bind_rows", "broth_correction_applied", "broth_od",
  "c_across", "case_when", "category", "clean_names", "col_number", "cols", "column",
  "combined", "complete", "conc_label", "conc_rank", "conc_rep", "concentration",
  "concentration_f", "concentration_value", "condition", "consistency_rating",
  "contamination_risk", "coord_fixed", "cor", "correction", "count", "cv",
  "cv_final_od", "data_source", "data_type", "data_type_f", "datetime", "desc",
  "display_label", "distinct", "dmy", "dmy_hms", "drift", "effect_category",
  "element_blank", "element_rect", "element_text", "everything", "expand_grid",
  "facet_wrap", "final_od", "geom_hline", "geom_line", "geom_point", "geom_ribbon",
  "geom_smooth", "geom_text", "geom_tile", "getSrcref", "ggplot", "ggsave", "group_by",
  "group_id", "growth_phase", "growth_trend", "head", "initial_od", "issue_type",
  "labeller", "labs", "left_join", "mad", "map", "map_lgl", "max_cv", "max_od",
  "max_od_phase", "max_temp", "max_tp", "mdy", "mdy_hms", "mean_blank_od",
  "mean_broth_od", "mean_cv", "mean_final_od", "mean_fold_change", "mean_halfbroth_od",
  "mean_inhibition_pct", "mean_od", "mean_od_phase", "mean_se", "mean_temp",
  "mean_total_growth", "median", "median_temp", "min_od", "min_temp", "mutate",
  "n_concentrations", "n_distinct", "n_negative", "n_replicates", "n_wells", "na.omit",
  "np_correction", "np_correction_applied", "od600", "od600_broth_corrected",
  "od600_final", "od600_raw", "od_range", "od_value", "parameter", "pivot_longer",
  "plot_annotation", "plot_layout", "position_jitter", "pull", "quantile", "read_csv",
  "read_excel", "replicate_cv", "replicate_id", "replicate_label", "replicate_name",
  "row_letter", "row_num", "row_number", "rowwise", "runApp", "sample_final_od",
  "sample_replicate", "sample_type", "scale_color_brewer", "scale_color_identity",
  "scale_color_manual", "scale_color_viridis_d", "scale_fill_manual",
  "scale_fill_viridis_c", "scale_fill_viridis_d", "scale_linetype_manual",
  "scale_shape_manual", "scale_x_continuous", "scale_y_continuous", "sd_auc",
  "sd_final_od", "sd_od", "sd_temp", "sd_total_growth", "se_final_od", "se_od",
  "se_od_raw", "seconds", "server", "shinyApp", "slice", "slice_head", "slice_max",
  "starts_with", "stat_summary", "str", "str_extract_all", "str_remove", "str_replace",
  "str_replace_all", "str_split", "str_starts", "str_to_title", "str_to_upper",
  "str_trunc", "temperature", "temperature_clean", "text_color", "text_label", "theme",
  "theme_minimal", "theme_void", "tibble", "time", "time_elapsed", "time_hrs",
  "time_hrs_clean", "time_hrs_consensus", "time_hrs_rounded", "time_point",
  "time_to_max_hrs", "total_growth", "ui", "ungroup", "unite", "untreated_control",
  "untreated_final_od", "value", "well", "well_id", "well_id_raw", "wells_included",
  "write_csv", "ymd_hms"
))

# package-private state (not exported)
.odjobs_state <- new.env(parent = emptyenv())
.odjobs_state$SOURCED_FROM_FUNCTION <- FALSE

# tiny getters/setters (not exported)
.set_sourced_from_function <- function(value) {
  .odjobs_state$SOURCED_FROM_FUNCTION <- isTRUE(value)
  invisible(.odjobs_state$SOURCED_FROM_FUNCTION)
}

.get_sourced_from_function <- function() {
  isTRUE(.odjobs_state$SOURCED_FROM_FUNCTION)
}

