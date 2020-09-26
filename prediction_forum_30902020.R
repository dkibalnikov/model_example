library(tidyverse) #универсальный набор пакетов для упрощенного программирования в среде R
library(tidymodels) #набор пакетов для моделирования 
library(deSolve) #пакет для численного решения дифференциальных уравнений
library(COVID19) #пакет для выгрузки данных о COVID19
library(patchwork) #библиотека группировки графиков 

#   ____________________________________________________________________________
#   1. Подготовительная часть                                               ####
tableau <- palette.colors(palette = "Tableau")

#   ____________________________________________________________________________
#   2. Прогноз выхода объёмов производства на целевой уровень               ####
# Оригинальная модель построена на базе данных реального производства. 
# Реальные данные защищены соглашением о конфиденциальности.
# Поэтом модель демонстрируется на синтезированных данных, близких к реальным

##  ............................................................................
##  2.1 Генерируем данные для модели                                        ####
day <- 1:540 #указываем интервал времени в днях
Ta <- 150 #устанавливаем время разгона, который в дальнейшем будем искать
power_line1 = 400 #указываем мощность линии производства
power_line2 = 700 #указываем мощность линии производства
power_line3 = 900 #указываем мощность линии производства

set.seed(38) #устанавливаем метку генерации случайных чисел для стабильного воспроизводств результатов

sim_plant <- tibble(day_num = day, #добавляем номер дня
                   maintanance = c(rep(1, 166), 10:4/12, rep(0.33, 3), 2:5/6) %>% rep(3), #синтезируем периоды остановов производства на обслуживание
                   line1 = (power_line1*(1-exp(-day/Ta)) + rnorm(day, 0, 20))*maintanance, #синтезируем данные для линии производства на основе динамической функции первого порядка
                   line2 = (power_line2*(1-exp(-day/Ta)) + rnorm(day, 0, 30))*maintanance, #синтезируем данные для линии производства на основе динамической функции первого порядка
                   line3 = (power_line3*(1-exp(-day/Ta)) + rnorm(day, 0, 40))*maintanance) #синтезируем данные для линии производства на основе динамической функции первого порядка
  
#Строим график для синтезированных данных
sim_plant %>% 
  select(-maintanance) %>% 
  pivot_longer(-day_num, values_to = "production", names_to = "line") %>% 
  ggplot(aes(x = day_num, y = production, col = line)) +
  geom_line(size = 1, alpha = 0.8) +
  scale_color_manual(values = unname(palette.colors(palette = "Tableau"))) +
  geom_hline(yintercept = power_line1, color = tableau[1], linetype = 5) +
  geom_hline(yintercept = power_line2, color = tableau[2], linetype = 5) +
  geom_hline(yintercept = power_line3, color = tableau[3], linetype = 5) +
  geom_smooth(method = "gam", formula = y ~ log(x), alpha = .5, linetype = 5) + 
  labs(title = "Динамика выпуска продукции завода по трем линиям", y = "Объем выпуска", col = "Производственные\nлинии", 
       x = "Дни", caption = "Пунктирными линиями представлена апроксимирующая кривая на базе логорифмической функции")

##  ............................................................................
##  2.2 Ищем параметры модели                                               ####
# Ищем значения сразу двух параметров: power и Ta
nl_fit1 <- nls(data = sim_plant, 
    line1 ~ power*(1-exp(-day_num/Ta)), #указываем функцию 
    start = list(power = 300, Ta = 100)) #указываем стартовые значения параметров

nl_fit1 #выводим результат
# power    Ta 
# 373.6 139.2 

nl_fit2 <- nls(data = sim_plant, 
               line2 ~ power*(1-exp(-day_num/Ta)), #указываем функцию
               start = list(power = 300, Ta = 100)) #указываем стартовые значения параметров

nl_fit2 #выводим результат
# power    Ta 
# 656.2 140.9 

nl_fit3 <- nls(data = sim_plant, 
               line3 ~ power*(1-exp(-day_num/Ta)), #указываем функцию
               start = list(power = 900, Ta = 100)) #указываем стартовые значения параметров

nl_fit3 #выводим результат
# power    Ta 
# 843.9 140.9 

##  ............................................................................
##  2.3 Исследуем модель                                                    ####
# На данном этапе пытаемся выяснить сколько дней от старта производство должно пройти чтобы наша оценка была состоятельной
#создаем функцию оценки параметра Ta (для линии 3), которая на вход принимает изменяемый интервал времени
get_Ta_est <-  function(day){
  nls(data = sim_plant[1:day,], 
      line3 ~ 900*(1-exp(-day_num/Ta)),
      start = list(Ta = 100)) %>% 
        coefficients()}
get_Ta_est(15) #проверяем работу функции
# Ta 
# 130.5721
# т.е. для 15-дневного интервала модель дает оценку параметра Ta = 130.5721

need_day <- 5:100 %>% #пробегаемся циклом по интервалу от 5 дней до 100 дней
  map_dfr(~get_Ta_est(.), .id = "need_day")

# Строим график и делаем заключения, значимые для бизнеса 
need_day %>% 
  mutate(need_day = as.numeric(need_day)) %>% 
  ggplot() +
  geom_line(aes(x = need_day, y = Ta), col = tableau[1], size = 2) + 
  geom_hline(yintercept = Ta, col = tableau[2], linetype = 5, size = 2) + 
  annotate(x = 50, y = 180, geom = "text", label = "Истинный уровень разгонной динамики Ta = 150", col = tableau[2], vjust = 1, hjust = 0, size = 6) +
  geom_curve(aes(x = 50, y = 180, xend = 30, yend = 150), arrow = arrow(length = unit(0.03, "npc")), col = tableau[2], size = 0.5) +
  labs(title = "График стабилизации параметра Та в зависимости от размера интервала оценки модели", x = "Размер интервала")

# Можно сделать заключение, что после 25 дней оценка параметра Та является состоятельно и следовательно 
# можно корректировать коммерческие и финансовые планы исходя из более точного прогноза выхода на целевой уровень выпуска

##  ............................................................................
##  2.4 Строим доверительные интервалы                                      ####
# Системно-динамические модели не могут использовать методы классической статистики для оценки доверительных интервалов
# Поэтому для данной модели используется техника bootstrap для оценки доверительных интервалов

sim_plant_btstrp1 <- sim_plant %>% 
  select(day_num, line3) %>% #выбираем линию 3 (для остальных линий используется аналогичный прием)
  slice(1:25) %>% #выбираем первые 25 дней в качестве отрезка для прогнозирования
  bootstraps(times = 1e3, apparent = TRUE) #семплируем (извлекаем примеры)  1000 значений из имеющейся выборки 

sim_plant_btstrp2 <- sim_plant %>% 
  select(day_num, line3) %>% #выбираем линию 3 (для остальных линий используется аналогичный прием)
  slice(1:50) %>% #выбираем первые 50 дней в качестве отрезка для прогнозирования для сравнения
  bootstraps(times = 1e3, apparent = TRUE) #семплируем (извлекаем примеры)  1000 значений из имеющейся выборки 

fit_nls_on_bootstrap <- function(split) {
  nls(line3 ~ 900*(1-exp(-day_num/Ta)), analysis(split), start = list(Ta = 100))} #создаем "конвеерную" функцию прогнозирования на вход которой будем подавать наши семплы

sim_plant_mdls1 <- sim_plant_btstrp1 %>% 
  mutate(model = map(splits, fit_nls_on_bootstrap), #используем конструкцию "mutate - map" для применения "конвеерной" функции к каждому семплу
         coef_info = map(model, tidy)) #выгружаем результат оценки модели

sim_plant_mdls2 <- sim_plant_btstrp2 %>% 
  mutate(model = map(splits, fit_nls_on_bootstrap), #используем конструкцию "mutate - map" для применения "конвеерной" функции к каждому семплу
         coef_info = map(model, tidy)) #выгружаем результат оценки модели
  
percentile_intervals1 <- int_pctl(sim_plant_mdls1, coef_info) #вычисляем доверительные интервалы для 25 дней
percentile_intervals1
# term  .lower .estimate .upper .alpha .method   
# <chr>  <dbl>     <dbl>  <dbl>  <dbl> <chr>     
#   1 Ta      117.      141.   182.   0.05 percentile
# Точность составляет +/- 24 дня 

percentile_intervals2 <- int_pctl(sim_plant_mdls2, coef_info) #вычисляем доверительные интервалы для 50 дней
percentile_intervals2
# term  .lower .estimate .upper .alpha .method   
# <chr>  <dbl>     <dbl>  <dbl>  <dbl> <chr>     
#   1 Ta      134.      145.   157.   0.05 percentile
# Точность составляет +/- 11 дней

sim_plant_mdls <- sim_plant_mdls1 %>% 
  unnest(coef_info) %>% #упрощаем структуру результата
  mutate(n = "n_25") %>% #добавляем маркер интервала
  bind_rows(sim_plant_mdls2 %>% #соединяем два примера с разными размерами интервалов (25 и 50)
              unnest(coef_info) %>% #упрощаем структуру результата
              mutate(n = "n_50")) #добавляем маркер интервала

# Строим графики и анализируем
sim_plant_mdls %>% 
  ggplot(aes(x = estimate)) +
  annotate(geom = "rect", ymin = 0,  ymax = Inf, alpha = .3, fill = tableau[1], 
           xmin = percentile_intervals1$.lower, xmax = percentile_intervals1$.upper) + 
  annotate(geom = "rect", ymin = 0,  ymax = Inf, alpha = .5, fill = tableau[1],
           xmin = percentile_intervals2$.lower, xmax = percentile_intervals2$.upper) +
  geom_histogram(binwidth = 5, alpha = .8) + 
  geom_density(aes(y = after_stat(count*5)), fill = tableau[3], col = tableau[3], alpha = .5) +
  facet_grid(~n) +
  guides(fill = "colorbar", fill = "legend") +
  labs(title = "Гистограммы оценки доверительных интервалов для двух примеров с разными размерами интервалов (25 и 50 дней)",
       x = "Оценка параметра", y = "Количество наблюдений")

boot_aug <- sim_plant_mdls %>% #сделаем преобразования для еще одного графика
  sample_n(500) %>% #выберем 500 случайных примеров оценок из 1000 
  mutate(augmented = map(model, augment)) %>% #создаем набор данных для отображения на графике
  unnest(augmented)

boot_aug %>% 
  ggplot(aes(x = day_num, y = line3)) +
  geom_line(aes(y = .fitted, group = id), alpha = .1, col = tableau[1], size = 2) +
  geom_line(col = tableau[3], size = 1,  alpha = .8) + 
  geom_point(col = tableau[3], size = 3,  alpha = .8) + 
  facet_grid(~n) + 
  labs(title = "Сопоставление фактических данных и множественных bootstrap оценок для двух примеров с разными размерами интервалов (25 и 50 дней)",
       x = "Дни", y = "Объем выпуска")


#   ____________________________________________________________________________
#   3. Прогноз количества заражений и смертности при эпидемиях              ####

covid_ru <- covid19("russia") #загружаем данные из облака
# Guidotti, E., Ardia, D., (2020), "COVID-19 Data Hub", Journal of Open Source Software 5(51):2376, doi:
# 10.21105/joss.02376.

##  ............................................................................
##  3.1 Осуществялеем подготовку данных                                     ####
covid_ru1 <- covid_ru %>% 
  mutate(across(c(tests, confirmed, recovered, deaths), ~(. - lag(.)), .names = "{col}_day"),
         rcvrd_dead_day = recovered_day + deaths_day) #создаем мгновенные значения (на момент времени) из накопительных 
  #slice(1:(nrow(covid_ru)-1)) #

covid_ru2 <- covid_ru1 %>% 
  filter(date > as.Date("2020-04-01")) 

rename_indicators = c(confirmed_day = "Инфицированных", deaths_day = "Смертей", recovered_day = "Выздоровлений", tests_day = "Тестирований", rcvrd_dead_day = "Выздоровлений\nсмертей")

covid_ru2 %>% 
  pivot_longer(matches("_day"), names_to = "type")  %>% 
  ggplot() +
  geom_line(aes(x = date, y = value, group = type, col = type), size = 2) +
  facet_grid(rows = vars(type), scale = "free", labeller = labeller(type = rename_indicators)) +
  scale_color_manual(values = unname(tableau), labels = rename_indicators) + 
  labs(title = "Динамика показтелий пандемии COVID-19 в России", x = "День", y = "Число наблюдений", col = "")

Infected <- covid_ru2$confirmed_day
Day <- 1:length(Infected)
N <- 140e6 # population of Russia
S_init <- covid_ru1 %>% 
  filter(date == "2020-03-14") %>% 
  pull(tests_day)

##  ............................................................................
##  3.2 Осуществляем оценку параметров                                      ####
SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/N * I * S
    dI <- beta/N * I * S - gamma * I 
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
} #создаем функцию, содержащию набор дифференциальных уравнений SIR

init <- c(S = S_init, I = Infected[1], R = 0) #устанавливаем начальные значения

RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  fit <- out[ , "I"]
  sum((Infected - fit)^2)
}

Opt <- optim(par = c(400, 0.01), 
             fn = RSS, 
             method = "L-BFGS-B", #выбираем метод оптиимизации 
             lower = c(0, 0.0002)) #устанавливаем ограничения на значения параметров
Opt$message
## [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"

Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par
# beta        gamma 
# 1.923651e+03 5.822402e-03 

t <- 1:300 # устанавливаем интервал прогнозного периода в днях


##  ............................................................................
##  3.3 Анализируем результат                                               ####
fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par)) #расчитываем прогнозные значения на базе вычисленных параметров 

covid_ru3 <- fit %>% #соединяем фактические и прогнозные значения
  left_join(covid_ru2 %>% 
              rownames_to_column("time") %>% 
              mutate(time = as.numeric(time))) 

current_day <- covid_ru3 %>% 
  filter(!is.na(date)) %>% 
  slice_tail() %>% 
  pull(time)

p1 <- covid_ru3 %>% 
  ggplot() +
  geom_line(aes(x = time, y = I, col = "Прогноз"), alpha =.7, linetype = 5, size = 2) +
  geom_line(aes(x = time, y = confirmed_day, col = "Факт"), alpha =.3, size = 2) + 
  geom_smooth(aes(x = time, y = confirmed_day, col = "Сглаженный\nфакт"), method = 'loess', linetype = 5, formula = y ~ splines::ns(x, 1)) +
  scale_color_manual(values = c("Прогноз" = unname(tableau[1]), "Факт" = unname(tableau[2]), "Сглаженный\nфакт"  = unname(tableau[2])), 
                     labels = c("Прогноз", "Факт", "Сглаженный\nфакт"), 
                     breaks = c("Прогноз", "Факт", "Сглаженный\nфакт"))  +
  annotate(geom = "rect", xmin = current_day, xmax = Inf, ymin = 0, ymax = Inf, fill = tableau[1], alpha = .3) +
  annotate(geom = "text", x = current_day + 20, y = 9000, label = "Прогнозный период", col = tableau[1], size = 8, hjust = 0) +
  labs(title = "Прогноз количества инфицированных в день COVID-19 в России", col = "", x = "Дни с момента старта пандемии", 
       y = "Новых заражений в день")

p2 <- covid_ru3 %>% 
  ggplot() +
  geom_line(aes(x = time, y = R, col = "Прогноз"), alpha =.7, linetype = 5, size = 2) +
  geom_line(aes(x = time, y = rcvrd_dead, col = "Факт"), alpha =.3, size = 2) + 
  geom_smooth(aes(x = time, y = rcvrd_dead, col = "Сглаженный\nфакт"), formula = y ~ splines::bs(x, 2), method = 'loess', linetype = 5) +
  scale_color_manual(values = c("Прогноз" = unname(tableau[1]), "Факт" = unname(tableau[2]), "Сглаженный\nфакт"  = unname(tableau[2])), 
                     labels = c("Прогноз", "Факт", "Сглаженный\nфакт"), 
                     breaks = c("Прогноз", "Факт", "Сглаженный\nфакт")) +
  annotate(geom = "rect", xmin = current_day, xmax = Inf, ymin = -Inf, ymax = Inf, fill = tableau[1], alpha = .3) +
  annotate(geom = "text", x = current_day + 20, y = 1500, label = "Прогнозный период", col = tableau[1], size = 8, hjust = 0) +
  labs(title = "Прогноз количества выздоровлений и смертей в день COVID-19 в России", col = "", x = "Дни с момента старта пандемии", 
       y = "Выздоровлений и смертей в день")
  
p1 / p2 + plot_layout(guides = "collect")
# Прогноз выздоровлений и смертей является следствием прогноза по количеству инфицированных
# Такая свзязь показателей обсуловлена структурой модели и позволяет гарантировать непротиворечивость группы исследуемых показателий
# Дальнейшее совершенствование модели возможно посредством использования в оптимизационных функциях сведений о числе умерших 
# Сведения о числе умерших может быть учтено в расширении модели SIR до модели SIRD (Susceptible-Infectious-Recovered-Deceased-Model )


