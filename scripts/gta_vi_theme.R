# ── GTA VI Theme and Table Styling ──

highlight_color <- "#FF00C8"  # Hot pink
accent_color <- "#00CFFF"     # Neon blue
bg_color <- "#000000"         # Black

# Plot Theme
gta_vi_theme <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = bg_color, color = NA),
      panel.background = element_rect(fill = bg_color),
      legend.background = element_rect(fill = bg_color),
      plot.title = element_text(color = accent_color, face = "bold"),
      axis.title = element_text(color = accent_color),
      axis.text = element_text(color = accent_color),
      legend.text = element_text(color = accent_color),
      legend.title = element_text(color = accent_color),
      plot.caption = element_text(color = accent_color)
    )
}

# Table Styling
gta_vi_table_style <- function(kable_tbl, col1 = 1, col2 = 2) {
  kbl_data <- attr(kable_tbl, "kable_meta")$kable_input
  num_cols <- if (!is.null(kbl_data)) ncol(kbl_data) else 1
  
  k_tbl <- kable_tbl %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE, position = "center"
    ) %>%
    column_spec(col1, bold = TRUE, color = "white", background = highlight_color)
  
  if (!is.null(col2) && col2 <= num_cols) {
    k_tbl <- k_tbl %>%
      column_spec(col2, color = "black", background = accent_color)
  }
  
  return(k_tbl)
}
