
modelo_diamonds <- function(){

  # Pacotes ------------------------------------------------------------------

  library(ggplot2)
  library(tidymodels)
  library(tidyverse)
  library(vip)
  library(jsonlite)

  # Dados -------------------------------------------------------------------
  data("diamonds")

  # EAD ---------------------------------------------------------------------
  # glimpse(diamonds)
  # skim(diamonds)
  # GGally::ggpairs(diamonds)
  # qplot(carat, price, data = diamonds)
  # qplot(x, price, data = diamonds)

  # base treino e teste -----------------------------------------------------



  ###################################################################################
  ########################   IMPORTANTE    #########################################
  #
  # Athos: eu vi que aplicando o log na variável resposta, o rmse caiu muito,
  # mas estava dando erro na hora de gerar a coluna com o predito.
  #
  # Então, eu criei uma coluna price_log como o log(price) e chamei de diamonds2, ok?
  #
  ###################################################################################



  diamonds2<-diamonds %>%
    mutate(price_log=log(price))


  set.seed(1)
  diamonds_initial_split <- diamonds2 %>% initial_split(8/10)

  diamonds_train <- training(diamonds_initial_split)
  diamonds_test <- testing(diamonds_initial_split)

  # data prep --------------------------------------------------
  # [opcional] Experimente criar step_log(), step_interact(), etc
  # para ver se melhora o modelo.


  diamonds_recipe <- recipe(price_log ~ ., data = diamonds_train) %>%     # price_log = log(price) é a minha variável resposta
    step_rm(price) %>%
    step_log(carat,depth,table,x,y,z,offset=0.001) %>%                    # apliquei log nas outras variaveis (melhorou um pouco)
    step_normalize(all_predictors(), -all_nominal()) %>%
    step_novel(all_nominal()) %>%
    step_dummy(all_nominal()) %>%
    step_zv(all_predictors())


  prep(diamonds_recipe)
  juice(prep(diamonds_recipe))


  # definicao do modelo -----------------------------------------------------
  # Defina um modelo de regressão linear usando glmnet e
  # prepare para tunar o hiperparâmetro penalty.
  # Deixe o mixture fixo em 1.
  # use as funções decision_tree(), tune(), set_engine() e set_mode().



  diamonds_model <- linear_reg(
    penalty = tune(), # lambda,
    mixture = 1 # LASSO
  ) %>%
    set_engine("glmnet")



  # workflow ----------------------------------------------------------------
  # Defina o workflow com workflow(), add_model() e add_recipe().

  ##########################
  # PREENCHA AQUI
  ##########################


  diamonds_wf <- workflow() %>%
    add_model(diamonds_model) %>%
    add_recipe(diamonds_recipe)


  # reamostragem com cross-validation ---------------------------------------
  # 5 conjuntos de cross-validation


  diamonds_resamples <- vfold_cv(diamonds_train, v = 5)


  # tunagem de hiperparametros ----------------------------------------------
  # tunagem do hiperparametro usando somente a métrica rmse com grid de tamanho 100.


  diamonds_tune_grid <- tune_grid(
    diamonds_wf,
    resamples = diamonds_resamples,
    grid = 100,
    metrics = metric_set(rmse),
    control = control_grid(verbose = TRUE, allow_par = FALSE)
  )


  # inspecao da tunagem -----------------------------------------------------
  #autoplot(diamonds_tune_grid)
  collect_metrics(diamonds_tune_grid)
  show_best(diamonds_tune_grid, "rmse")



  # seleciona o melhor conjunto de hiperparametros
  diamonds_best_hiperparams <- select_best(diamonds_tune_grid, "rmse")

  # atualize o workflow com finalize_workflow()


  diamonds_wf <- diamonds_wf %>% finalize_workflow(diamonds_best_hiperparams)

  # desempenho do modelo final ----------------------------------------------

  # Faça o last_fit()

  diamonds_last_fit <- diamonds_wf %>% last_fit(split = diamonds_initial_split)


  collect_metrics(diamonds_last_fit)
  collect_predictions(diamonds_last_fit) %>%
    ggplot(aes(exp(.pred), exp(price_log))) +               # revertendo o log
    geom_point()

  collect_predictions(diamonds_last_fit) %>%
    mutate(
      price = exp(price_log),           #######   revertendo o log no price_log
      .pred = exp(.pred)                #######   revertendo o log no predito
    ) %>%
    rmse(price, .pred)                 ################## rmse     standard        790.   ######################




  collect_predictions(diamonds_last_fit) %>%
    ggplot(aes(exp(.pred), (exp(price_log)-exp(.pred)))) +
    geom_point() +
    geom_smooth(se = FALSE)

  vip(diamonds_last_fit$.workflow[[1]]$fit$fit)





  # modelo final ------------------------------------------------------------
  # Aplique o fit() no workflow usando todos os dados para o modelo final


  diamonds_final_model <- diamonds_wf %>% fit(data = diamonds2)           ######## usando o diamonds2 (que tem a coluna price_log)



  # importancia das variaveis -----------------------------------------------


  #vip::vip(diamonds_final_model$fit$fit)

  #vip::vi(diamonds_final_model$fit$fit) %>%
  #  mutate(
  #    abs_importance = abs(Importance),
  #    Variable = fct_reorder(Variable, abs_importance)
  #  ) %>%
  #  ggplot(aes(x = abs_importance, y = Variable, fill = Sign)) +
  #  geom_col()

  # predicoes ---------------------------------------------------------------



  diamonds_com_previsao <- diamonds2 %>%
    mutate(
      price_pred = exp(predict(diamonds_final_model, new_data = .)$.pred)                     #### exp para reverter o log
    )


  list(toJSON(diamonds_com_previsao))

  # guardar o modelo para usar depois ---------------------------------------
  #saveRDS(diamonds_final_model, file = "diamonds_final_model.rds")







  # coisas especiais do glmnet e regressão LASSO ----------------------------
  #diamonds_final_model$fit$fit$fit %>% plot

  # só para fins didáticos
  #diamonds_final_model$fit$fit$fit$beta %>%
  #  as.matrix() %>%
  #  t() %>%
  #  as.tibble() %>%
  #  mutate(
  #    lambda = diamonds_final_model$fit$fit$fit$lambda
  #  ) %>%
  #  pivot_longer(
  #   c(-lambda),
  #    names_to = "variavel",
  #    values_to = "peso"
  #  ) %>%
  #  ggplot(aes(x = lambda, y = peso, colour = variavel)) +
  #  geom_line(size = 1) +
  #  geom_vline(xintercept = exp(diamonds_final_model$fit$fit$spec$args$penalty), colour = "red", linetype = "dashed") +
  #  scale_x_log10() +
  #  theme_minimal()







  # 3. [desafio] Ajuste uma árvore de decisão, agora com todas as variáveis,
  # e compare:
  # (a) se as variáveis mais importantes são as mesmas.
  # (b) se o desempenho da árvore supera o do LASSO em termos de RMSE.
  # Dica: Siga as mesmas etapas, apenas mudando onde necessário.


  ##########################################################################
  ###################### USANDO O LOG COMO NA REGRESSAO LINEAR #############
  ##########################################################################

  # data("diamonds")
  #
  # diamonds2<-diamonds %>%
  #   mutate(price_log=log(price))
  #
  #
  # set.seed(1)
  # diamonds_initial_split <- diamonds2 %>% initial_split(8/10)
  #
  # diamonds_train <- training(diamonds_initial_split)
  # diamonds_test <- testing(diamonds_initial_split)
  #
  # diamonds_recipe <- recipe(price_log ~ ., data = diamonds_train) %>%     # price_log = log(price) é a minha variável resposta
  #   step_rm(price) %>%
  #   step_log(carat,depth,table,x,y,z,offset=0.001) %>%                    # apliquei log nas outras variaveis (melhorou um pouco)
  #   step_normalize(all_predictors(), -all_nominal()) %>%
  #   step_novel(all_nominal()) %>%
  #   step_dummy(all_nominal()) %>%
  #   step_zv(all_predictors())
  #
  #
  # prep(diamonds_recipe)
  # juice(prep(diamonds_recipe))
  #
  #
  # diamonds_model <- decision_tree(min_n = 5, cost_complexity = tune(), tree_depth=10) %>%
  #   set_engine("rpart") %>%
  #   set_mode("regression")
  #
  #
  # diamonds_wf <- workflow() %>%
  #   add_model(diamonds_model) %>%
  #   add_recipe(diamonds_recipe)
  #
  #
  # diamonds_resamples <- vfold_cv(diamonds_train, v = 2)
  #
  # diamonds_hiperparams <- 10
  #
  # diamonds_tune_grid <- tune_grid(
  #   diamonds_wf,
  #   resamples = diamonds_resamples,
  #   grid = diamonds_hiperparams,
  #   metrics = metric_set(rmse),
  #   control = control_grid(verbose = TRUE, allow_par = FALSE)
  # )
  #
  #
  # autoplot(diamonds_tune_grid)
  # collect_metrics(diamonds_tune_grid)
  # show_best(diamonds_tune_grid, "rmse")
  #
  # diamonds_best_hiperparams <- select_best(diamonds_tune_grid, "rmse")
  #
  # diamonds_wf <- diamonds_wf %>% finalize_workflow(diamonds_best_hiperparams)
  #
  #
  # diamonds_last_fit <- diamonds_wf %>% last_fit(split = diamonds_initial_split)
  #
  #
  # collect_metrics(diamonds_last_fit)
  # collect_predictions(diamonds_last_fit) %>%
  #   ggplot(aes(exp(.pred), exp(price_log))) +               # revertendo o log
  #   geom_point()
  #
  # collect_predictions(diamonds_last_fit) %>%
  #   mutate(
  #     price = exp(price_log),           #######   revertendo o log no price_log
  #     .pred = exp(.pred)                #######   revertendo o log no predito
  #   ) %>%
  #   rmse(price, .pred)                 ################## rmse     standard        671.   ######################
  #
  # collect_predictions(diamonds_last_fit) %>%
  #   ggplot(aes(exp(.pred), (exp(price_log)-exp(.pred)))) +
  #   geom_point() +
  #   geom_smooth(se = FALSE)
  #
  # vip(diamonds_last_fit$.workflow[[1]]$fit$fit)
  #
  # diamonds_final_model <- diamonds_wf %>% fit(data = diamonds2)           ######## usando o diamonds2 (que tem a coluna price_log)
  #
  # vip::vip(diamonds_final_model$fit$fit)
  #
  # vip::vi(diamonds_final_model$fit$fit) %>%
  #   mutate(
  #     abs_importance = abs(Importance),
  #     Variable = fct_reorder(Variable, abs_importance)
  #   ) %>%
  #   ggplot(aes(x = abs_importance, y = Variable)) +
  #   geom_col()
  #
  # diamonds_com_previsao <- diamonds2 %>%
  #   mutate(
  #     price_pred = exp(predict(diamonds_final_model, new_data = .)$.pred)
  #   )
  #
  # saveRDS(diamonds_final_model, file = "diamonds_final_model.rds")
  #
  #
  # ##########################################################################
  # # As variáveis mais importantes são diferentes. Na regressão linear,
  # # as mais importantes foram carat, clarity_1, color_1(neg) e clarity_2 (neg). Na ávore de decisão,
  # # as mais importantes foram y, x carat e z.
  # #
  # # Em termos de RMSE a regressão linear deu 790 e na árvore de decisão 671.
  #
  # ##########################################################################
  #
  #
  #
}

