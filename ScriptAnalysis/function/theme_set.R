
library(lubridate)
library(RColorBrewer)
library(paletteer)
library(scales)

# suppressWarnings(font_import(pattern = "times", prompt = F))

scientific_10 <- function(x) {
     ifelse(x == 0, 0, parse(text = gsub("[+]", "", gsub("e", "%*%10^", scales::scientific_format()(x)))))
}


log_fill <- trans_new(
     name = "log_fill",
     transform = function(x) sign(x) * log1p(abs(x)),
     inverse = function(x) sign(x) * (exp(abs(x)) - 1)
)

theme_set <- function() {
  theme_classic() +
    theme(
      plot.caption = element_text(
        face = "bold", size = 14, vjust = 0,
        hjust = 0
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(face = "bold", size = 14, hjust = 0),
      legend.text = element_text(face = "bold", size = 12),
      legend.title = element_text(face = "bold", size = 12),
      legend.box.background = element_rect(fill = "transparent", colour = "transparent"),
      legend.background = element_rect(fill = "transparent", colour = "transparent"),
      axis.title.x = element_text(face = "bold", size = 12, color = "black"),
      axis.title.y = element_text(face = "bold", size = 12, color = "black"),
      axis.text.x = element_text(size = 12, color = "black"),
      axis.text.y = element_text(size = 12, color = "black")
    )
}

theme_plot <- function() {
  theme_bw() +
    theme(
      plot.caption = element_text(
        face = "bold", size = 16, vjust = 0,
        hjust = 0
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(face = "bold", size = 18, hjust = 0),
      legend.text = element_text(face = "bold", size = 14),
      legend.title = element_text(face = "bold", size = 16),
      legend.box.background = element_rect(fill = "transparent", colour = "transparent"),
      legend.background = element_rect(fill = "transparent", colour = "transparent"),
      axis.title.x = element_text(face = "bold", size = 16, color = "black"),
      axis.title.y = element_text(face = "bold", size = 16, color = "black"),
      axis.text.x = element_text(size = 14, color = "black"),
      axis.text.y = element_text(size = 14, color = "black")
    )
}

func_rmse <-
  # actual_val is the actual valeu,
  # fit_val is the value fitted by model
  function(actual_val, fit_val) {
    sqrt(
      mean((as.numeric(fit_val) - as.numeric(actual_val))^2, na.rm = TRUE)
    )
  }

fill_color <- c("#E64B35FF", "#4DBBD5FF", "#3C5488FF", "#91D1C2FF", "#7E6148FF")
fill_color_disease <- paletteer_d("rcartocolor::Pastel")[1:10]
fill_color_continue <- paletteer_d("MoMAColors::ustwo")

index_labels <- c("SMAPE", "RMSE", "MASE", "R_Squared")
disease_groups <- c("Respiratory IDs",
                    "Vector-borne and zoonotic IDs",
                    "Gastrointestinal IDs",
                    "Sexually IDs",
                    "Other IDs")
models <- c("Neural Network", "ETS", "SARIMA", "TBATS", "Hybrid", "Bayesian structural")
models_label <- c("Neural Network", "ETS", "SARIMA", "TBATS", "Hybrid**", "Bayesian structural")

# Laplace smoothing

add_value <- 0.01

# left border
split_dates <- as.Date(c("2020/1/1", "2021/1/1", "2022/1/1", "2023/1/1"))
split_periods <- c("Pre-COVID-19",
                   "PHSMs",
                   "Wild & Delta variant",
                   "Omicron variant",
                   "Endemic")
back_color <- c('white',"grey", "white", "grey", "white")
names(back_color) <- split_periods

# max process: 20
max_proces <- 20
