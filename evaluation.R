rm(list = ls())
library(tidyverse)
library(ggbump)

# Reading the case study data


df_evaluations <- readxl::read_xlsx(path = "notasMP.xlsx", sheet = "evaluaciones")
df_evaluations <- df_evaluations %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))

df_preferences <- readxl::read_xlsx(path = "notasMP.xlsx", sheet = "importancia")

ncrit <- nrow(df_preferences)

# Functions for computing weights

roc_weights <- function(ncrit){

  m <- matrix(
    rep(1 / 1:ncrit, ncrit),
    nrow = ncrit,
    byrow = T
  )

  m[lower.tri(m)] <- 0.

  apply(m, MARGIN = 1, sum)/ncrit

}


rs_weights <- function(ncrit){

  sapply(1:ncrit, function(i) (2*(ncrit + 1 - i))/(ncrit*(ncrit + 1)))
}

rr_weights <- function(ncrit){

  sapply(1:ncrit, function(i) (1/i)/sum(1/(1:ncrit)))
}

# Computing students' global evaluation

eval_mat <- df_evaluations %>%
  select(-c(Estudiante, NF)) %>%
  as.matrix()

roc_ws <- roc_weights(ncrit)[df_preferences$Importancia]
rs_ws <- rs_weights(ncrit)[df_preferences$Importancia]
rr_ws <- rr_weights(ncrit)[df_preferences$Importancia]

w_list <- list(
  ROC = roc_ws,
  RS = rs_ws,
  RR = rr_ws
)

global_grades <- sapply(w_list, function(ws) eval_mat %*% ws)

df_evaluations <- df_evaluations %>%
  bind_cols(global_grades)

# Comparison analysis

df_final <- df_evaluations %>%
  mutate(
    NF.Rank = rank(-NF),
    ROC.Rank = rank(-ROC),
    RS.Rank = rank(-RS),
    RR.Rank = rank(-RR)
  ) %>%
  select(
    Estudiante,
    NF,
    NF.Rank,
    ROC,
    ROC.Rank,
    RS,
    RS.Rank,
    RR,
    RR.Rank
  )


write_csv(df_final, file = "final_grades.csv")


# Stats

# Rank order correlation (Kendall's tau)

corr_results <- sapply(df_final %>% select(ROC.Rank, RS.Rank, RR.Rank),
                       function(rr) cor(rr, df_final$NF.Rank, method = "kendall"))

avg_ord_diff <- sapply(df_final %>% select(ROC.Rank, RS.Rank, RR.Rank),
                       function(rr) mean(abs(rr - df_final$NF.Rank)))

eucl_dist <- sapply(w_list, function(ws) sqrt(sum((ws - df_preferences$Pesos)^2)))


df_stats <- tibble(
  Method = names(eucl_dist),
  Correlation = corr_results,
  AvgOrdDiff = avg_ord_diff,
  EuclDistWeights = eucl_dist
)


# Plots


step = 0.2
df_plot_ <- lapply(split(df_final, df_final$NF.Rank), function(grp_df){
  grp_df$NF.Rank = grp_df$NF.Rank #+ step*c(0:(nrow(grp_df)-1))
  grp_df
})

df_plot_ <- bind_rows(df_plot_)

df_plot <- df_plot_ %>% filter(NF.Rank < 60) %>%
  select(Estudiante, ends_with(".Rank")) %>%
  rename(
    "Tradicional" = NF.Rank,
    "ROC" = ROC.Rank,
    "RS" = RS.Rank,
    "RR" = RR.Rank
  ) %>% pivot_longer(
    cols = -Estudiante,
    names_to = "Método",
    values_to = "Posición"
  ) %>%
  mutate(
    `Método` = factor(`Método`, levels=c("Tradicional", "ROC", "RS", "RR")),
    Estudiante = factor(Estudiante),
    PosRank = floor(`Posición`)
  )




p_rank_comp <- df_plot %>%
  ggplot(aes(x = as.numeric(`Método`), y = `Posición`, color = Estudiante)) +
  geom_bump(linewidth = 1, alpha=0.8) +
  geom_point(aes(fill = Estudiante), size = 7, shape=19, alpha=0.8) + # fill="white", stroke=2) +
  #geom_text(data = df_plot %>% filter(`Método` == "Tradicional"),
  #          aes(x = as.numeric(`Método`) - 0.1, label = Estudiante),
  #            size = 2.5, hjust = 1) +
  #geom_text(data = df_plot %>% filter(`Método` == "RR"),
  #            aes(x = as.numeric(`Método`) + 0.1, label = Estudiante),
  #          size = 2.5, hjust = 0) +
  geom_text(aes(label = PosRank), color="black",
            size = 3) +
  scale_x_continuous(breaks = 1:4,
                     labels = c("Enfoque\nTradicional", "Método\nROC", "Método\nRS", "Método\nRR"),
                     limits = c(0.8,4.2),
                     position = "top") +
  xlab("") +
  ylab("") +
  scale_y_reverse() +
  theme_void() +
  theme(legend.position = "none",
        axis.text.y=element_blank(),
        axis.text.x = element_text(face = "bold")
        )

print(p_rank_comp)

ggsave("p_rank_comp.pdf", plot = p_rank_comp, width = 8, height = 11)


df_weights <- df_preferences %>%
  mutate(
    `Método ROC` = roc_ws,
    `Método RS` = rs_ws,
    `Método RR` = rr_ws
  ) %>%
  select(`Evaluación parcial`, Pesos, `Método ROC`, `Método RS`, `Método RR`) %>%
  rename(
    "Enf. tradicional" = Pesos
  ) %>%
  pivot_longer(cols = -c(`Evaluación parcial`), names_to = "Método", values_to = "Peso") %>%
  mutate(`Método` = factor(`Método`, levels=c("Enf. tradicional", "Método ROC", "Método RS", "Método RR")))

p_weight_comp <- df_weights %>%
  ggplot(aes(x=`Evaluación parcial`, y=Peso, group=`Método`, color=`Método`))+
  geom_line() +
  geom_point(size=2) +
  theme_minimal() +
  ggtitle("Distribución de los pesos") +
  theme(legend.position = c(0., 0.25))


ggsave("p_weights.pdf", plot = p_weight_comp, width = 4, height = 3)
