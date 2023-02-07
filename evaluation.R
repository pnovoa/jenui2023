rm(list = ls())
library(tidyverse)
library(ggbump)


# Reading the case study data

df_evaluations <- readxl::read_xlsx(path = "notasMP.xlsx", sheet = "evaluaciones")
df_evaluations <- df_evaluations %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))

df_preferences <- readxl::read_xlsx(path = "notasMP.xlsx", sheet = "importancia") %>%
  mutate(
    `Evaluación parcial` = factor(`Evaluación parcial`)
  )

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

ew_weights <- function(ncrit){
  rep(1/ncrit, ncrit)
}

set_fixed_weights <- function(ori_ws, pos_new, val_new){

  new_ws <- rep(-1, length(ori_ws) + length(pos_new))

  new_ws[pos_new] <- val_new

  new_ws[new_ws == -1] <- ori_ws

  new_ws

}


# Computing students' global evaluation

eval_mat <- df_evaluations %>%
  select(-c(Estudiante, NF)) %>%
  as.matrix()

rownames(eval_mat) <- df_evaluations$Estudiante


over_var_w <- 1 - df_preferences  %>%
  filter(Tipo == "Fijo") %>%
  group_by(Tipo) %>%
  summarise(
    totalw = sum(Pesos)
  ) %>% select(totalw) %>% as.numeric()

ncrit_free <- nrow(df_preferences)

fixed_importance <- df_preferences  %>%
  filter(Tipo != "Fijo") %>%
  select(Importancia) %>%
  mutate(Importancia = rank(Importancia))

ncrit_fixed <- nrow(fixed_importance)


eqw_ws_fixed <- ew_weights(ncrit_fixed) * over_var_w
roc_ws_fixed <- roc_weights(ncrit_fixed)[fixed_importance$Importancia] * over_var_w
rs_ws_fixed <- rs_weights(ncrit_fixed)[fixed_importance$Importancia] * over_var_w
rr_ws_fixed <- rr_weights(ncrit_fixed)[fixed_importance$Importancia] * over_var_w

w_list_fixed <- list(
  EW.Fixed = eqw_ws_fixed,
  ROC.Fixed = roc_ws_fixed,
  RS.Fixed = rs_ws_fixed,
  RR.Fixed = rr_ws_fixed
)

new_pos_vals <- df_preferences  %>%
  filter(Tipo == "Fijo") %>%
  mutate(Index = as.numeric(`Evaluación parcial`)) %>%
  select(Index, Pesos)

w_list_fixed <- lapply(w_list_fixed, function(ws) set_fixed_weights(ws, new_pos_vals$Index, new_pos_vals$Pesos))

eqw_ws_free <- ew_weights(ncrit_free)
roc_ws_free <- roc_weights(ncrit_free)[df_preferences$Importancia]
rs_ws_free <- rs_weights(ncrit_free)[df_preferences$Importancia]
rr_ws_free <- rr_weights(ncrit_free)[df_preferences$Importancia]

w_list_free <- list(
  EW.Free = eqw_ws_free,
  ROC.Free = roc_ws_free,
  RS.Free = rs_ws_free,
  RR.Free = rr_ws_free
)

w_list_all <- append(list(Tradicional = df_preferences$Pesos), w_list_fixed)
w_list_all <- append(w_list_all, w_list_free)
names(w_list_all) <- paste0("Grade.", names(w_list_all))

students_over_3_5 <- eval_mat[,ncrit_free] >= 3.5
students_under_3_5 <- eval_mat[,ncrit_free] < 3.5

global_grades <- sapply(w_list_all, function(ws){

  m1 <- eval_mat[students_over_3_5, ] %*% ws

  m2 <- eval_mat[students_under_3_5, ncrit_free] * ws[ncrit_free]

  m2 <- as.matrix(m2)

  mr <- rbind(m1, m2)

  mr

} )

students_names <- c(rownames(eval_mat[students_over_3_5,]), rownames(eval_mat[students_under_3_5,]))

df_global_grades <- as_tibble(global_grades) %>% mutate(Estudiante = students_names)

df_evaluations <- df_evaluations %>%
  inner_join(
    df_global_grades,
    by=c("Estudiante")
  )

method_levels <- c("Tradicional",
                   "EW\n(F)",
                   "ROC\n(F)",
                   "RS\n(F)",
                   "RR\n(F)",
                   "EW\n(L)",
                   "ROC\n(L)",
                   "RS\n(L)",
                   "RR\n(L)")

df_plot_grades <- df_evaluations %>%
  select(Estudiante, starts_with("Grade.")) %>%
  pivot_longer(cols = -Estudiante, names_to = "Método", values_to = "Nota") %>%
  mutate(
    `Método` = str_replace(`Método`, "Grade.", ""),
    `Método` = str_replace(`Método`, "\\.", "\n"),
    `Método` = str_replace(`Método`, "Fixed", "(F)"),
    `Método` = str_replace(`Método`, "Free", "(L)")
  ) %>%
  mutate(
    `Método` = factor(`Método`, levels=method_levels)
  )

df_plot <- df_plot_grades %>%
  group_by(`Método`) %>%
  summarise(
    Suspenso = sum(Nota < 5)/n(),
    Aprobado = sum(Nota >= 5 & Nota < 7)/n(),
    Notable = sum(Nota >= 7 & Nota < 9)/n(),
    Sobresaliente = sum(Nota >= 9)/n()
  )



df_bar_plot <- df_plot %>%
  pivot_longer(
    cols = -`Método`,
    names_to = "Grupo",
    values_to = "Porcentaje"
  ) %>%
  mutate(Grupo = factor(Grupo, levels=rev(c("Suspenso", "Aprobado", "Notable", "Sobresaliente")))) %>%
  arrange(`Método`, Grupo)

cbPalette <- c("#d7191c", "#fdae61", "#abd9e9", "#2c7bb6")

bar_plot <- df_bar_plot %>% ggplot(aes(x=Método, y=Porcentaje, fill=Grupo)) +
  geom_bar(stat = "identity", width = 0.4) +
  scale_fill_manual(values = cbPalette) +
  scale_y_continuous(labels = scales::percent) +
  #theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
  theme_minimal()

ggsave(filename = "bar_plot.pdf", plot = bar_plot, width = 8, height = 2.5)

df_plot_grades %>% ggplot(aes(x=`Método`, y=Nota)) + geom_boxplot()



# # Comparison analysis
#
# df_final <- df_evaluations %>%
#   mutate(
#     NF.Rank = rank(-NF),
#     ROC.Rank = rank(-ROC),
#     RS.Rank = rank(-RS),
#     RR.Rank = rank(-RR)
#   ) %>%
#   select(
#     Estudiante,
#     NF,
#     NF.Rank,
#     ROC,
#     ROC.Rank,
#     RS,
#     RS.Rank,
#     RR,
#     RR.Rank
#   )
#
#
# write_csv(df_final, file = "final_grades.csv")
#
#
# # Stats
#
# # Rank order correlation (Kendall's tau)
#
# corr_results <- sapply(df_final %>% select(ROC.Rank, RS.Rank, RR.Rank),
#                        function(rr) cor(rr, df_final$NF.Rank, method = "kendall"))
#
# avg_ord_diff <- sapply(df_final %>% select(ROC.Rank, RS.Rank, RR.Rank),
#                        function(rr) mean(abs(rr - df_final$NF.Rank)))
#
# eucl_dist <- sapply(w_list, function(ws) sqrt(sum((ws - df_preferences$Pesos)^2)))
#
#
# df_stats <- tibble(
#   `Método` = names(eucl_dist),
#   `Correlación` = corr_results,
#   `Diferencia promedio del orden`  = avg_ord_diff,
#   `Distancia entre pesos` = eucl_dist
# )
#
#
# # Plots
#
#
# step = 0.2
# df_plot_ <- lapply(split(df_final, df_final$NF.Rank), function(grp_df){
#   grp_df$NF.Rank = grp_df$NF.Rank #+ step*c(0:(nrow(grp_df)-1))
#   grp_df
# })
#
# df_plot_ <- bind_rows(df_plot_)
#
# df_plot <- df_plot_ %>% filter(NF.Rank < 60) %>%
#   select(Estudiante, ends_with(".Rank")) %>%
#   rename(
#     "Tradicional" = NF.Rank,
#     "ROC" = ROC.Rank,
#     "RS" = RS.Rank,
#     "RR" = RR.Rank
#   ) %>% pivot_longer(
#     cols = -Estudiante,
#     names_to = "Método",
#     values_to = "Posición"
#   ) %>%
#   mutate(
#     `Método` = factor(`Método`, levels=c("Tradicional", "ROC", "RS", "RR")),
#     Estudiante = factor(Estudiante),
#     PosRank = floor(`Posición`)
#   )
#
#
#
#
# p_rank_comp <- df_plot %>%
#   ggplot(aes(x = as.numeric(`Método`), y = `Posición`, color = Estudiante)) +
#   geom_bump(linewidth = 1, alpha=0.8) +
#   geom_point(aes(fill = Estudiante), size = 7, shape=19, alpha=0.8) + # fill="white", stroke=2) +
#   #geom_text(data = df_plot %>% filter(`Método` == "Tradicional"),
#   #          aes(x = as.numeric(`Método`) - 0.1, label = Estudiante),
#   #            size = 2.5, hjust = 1) +
#   #geom_text(data = df_plot %>% filter(`Método` == "RR"),
#   #            aes(x = as.numeric(`Método`) + 0.1, label = Estudiante),
#   #          size = 2.5, hjust = 0) +
#   geom_text(aes(label = PosRank), color="black",
#             size = 3) +
#   scale_x_continuous(breaks = 1:4,
#                      labels = c("Enfoque\nTradicional", "Método\nROC", "Método\nRS", "Método\nRR"),
#                      limits = c(0.8,4.2),
#                      position = "top") +
#   xlab("") +
#   ylab("") +
#   scale_y_reverse() +
#   theme_void() +
#   theme(legend.position = "none",
#         axis.text.y=element_blank(),
#         axis.text.x = element_text(face = "bold")
#         )
#
# print(p_rank_comp)
#
# ggsave("p_rank_comp.pdf", plot = p_rank_comp, width = 8, height = 11)
#
# df_weights_ <- df_preferences %>%
#   mutate(
#     `Método ROC` = roc_ws,
#     `Método RS` = rs_ws,
#     `Método RR` = rr_ws
#   ) %>%
#   select(`Evaluación parcial`, Pesos, `Método ROC`, `Método RS`, `Método RR`)
#
#
# library(xtable)
#
# xtable(df_weights_ %>% select(-Pesos), type = "latex", )
#
#
# df_weights <- df_weights_ %>%
#   rename(
#     "Enfoque tradicional" = Pesos
#   ) %>%
#   pivot_longer(cols = -c(`Evaluación parcial`), names_to = "Método", values_to = "Peso") %>%
#   mutate(`Método` = factor(`Método`, levels=c("Enfoque tradicional", "Método ROC", "Método RS", "Método RR")))
#
# p_weight_comp <- df_weights %>%
#   ggplot(aes(x=`Evaluación parcial`, y=Peso, group=`Método`, color=`Método`))+
#   geom_line(aes(linetype=`Método`)) +
#   geom_point(size=2) +
#   theme_minimal() +
#   #ggtitle("Distribución de los pesos") +
#   theme(legend.position = c(0.25, 0.7), legend.title = element_blank())
#
#
# ggsave("p_weights.pdf", plot = p_weight_comp, width = 3.5, height = 2.5)
#
#
#
# df_plot_stats <- df_stats %>%
#   pivot_longer(cols = -c(`Método`), names_to = "Medida", values_to = "Valor") %>%
#   mutate(`Método` = factor(`Método`, levels=c("ROC", "RS", "RR"))) %>%
#   mutate(
#     Medida = ifelse(Medida == "Correlación", "a) Correlación",
#                     ifelse(Medida == "Distancia entre pesos", "c) Distancia entre pesos",
#                            "b) Diferencia promedio del orden"
#                     )
#                     )
#   )
#
#
# p_stats <- df_plot_stats %>%
#   ggplot(aes(x=`Método`, y=Valor, fill=`Método`)) +
#   geom_bar(stat = "identity", width=0.3, alpha=0.9) +
#   geom_text(aes(label=round(Valor, 3)), size=3, hjust = 1.2, angle = 90, fontface="bold") +
#   theme_minimal() + ylab("") +
#   facet_wrap(~Medida, nrow = 3, scales = "free") +
#   #ggtitle("Distribución de los pesos") +
#   theme(legend.position = "none", axis.text.y= element_blank())
#
# ggsave(filename = "p_medidas.pdf", plot = p_stats, width = 3, height = 5)
