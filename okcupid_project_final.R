
# Подключение пакетов и загрузка шрифтов -----------------------------------------------------

library(RKaggle)
library(tidyverse)
library(patchwork)
library(showtext)
library(ggpop)

font_add_google("Roboto Condensed", "roboto_condensed")
font_add_google("Inter","inter")
showtext_auto()

# Загрузка датасета -------------------------------------------------------


profiles <- get_dataset("andrewmvd/okcupid-profiles") #напрямую из kaggle
profiles_small <- profiles %>% 
  select(-starts_with("essay")) # убрали совершенно точно лишнее

rm(profiles) # чтобы не засоряло память
# Делаем датасет с колонкой attitude на будущее для астрологии

profiles_with_attitude <- profiles_small %>%
  mutate(
    attitude = case_when(
      str_detect(sign, "but.*") ~ "doesn't matter",
      str_detect(sign, "and it matters.*") ~ "matters a lot",
      str_detect(sign, "and it&rsquo.*") ~ "fun to think about",
      is.na(sign) ~ NA_character_,
      TRUE ~ "no info"), .after = sign  # нам надо сохранить паттерн, чтобы на его основе создать колонку attitude
  ) %>% 
  mutate(
    sign = str_remove(sign, "(but|and).*") # здесь уже можем от него избавиться
  ) %>% 
  mutate(sign = str_trim(sign))  # здесь уже избавляемся от лишних пробелов после удаления паттерна

rm(profiles_small)

#Проверка количества мужчин и женщин 
profiles_with_attitude %>% 
  group_by(sex) %>%
  summarise(n = n()) # посчитали количество мужчин и женщин 

#profiles_with_attitude %>% 
#group_by(age) %>%
#summarise(n = n()) %>% 
#print(n = Inf) #посмотрели, что 109 и 110 - два выброса # плюс датасета в том, что здесь нет NA в sex и age

# Посмотрим как распределяются м и женщины в приложении -------------------------------
# Соотношение полов с помощью ggpop ---------------------------------------
# Сделаем отдельный дф с полом и количеством



gender_count <- profiles_with_attitude %>% 
  group_by(sex) %>%
  summarise(n = n()) 

gender_count_processed <- process_data(data = gender_count, 
                                       group_var = sex, 
                                       sum_var = n, 
                                       sample_size = 1000)
# Присвоим иконки 
gender_count_processed <- gender_count_processed %>%
  mutate(icon = case_when(
    type == "m" ~ "mars",
    type == "f" ~ "venus"))

ggplot(data = gender_count_processed, aes(icon = icon, color = type)) +
  geom_pop(size = 1, arrange = TRUE) +
  theme_void(base_size = 40) +
  theme(legend.position = "bottom") +
  labs(title = "Соотношение полов",
       subtitle = "в дейтинг-приложении OkCupid",
       caption = "Источник: OkCupid Dataset, 2012") +
  theme(legend.title = element_blank(),
        plot.background = element_rect(fill = "#171717"),
        panel.background = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(color = "#c58c00"),
        plot.title = element_text(color = "#c58c00", hjust = 0.5),
        plot.subtitle = element_text(color = "#c58c00", 
                                     hjust = 0.5, 
                                     size = 18,
                                     margin = margin(t = -2, b = 8),
        ),
        plot.caption = element_text(color = "#c58c00", size = 12),
        text = element_text(family = "inter")) +
  scale_legend_icon(size = 10) +
  scale_color_manual(values = c("m" = "#00c1ff", "f" = "#ef33f1"),
                     labels = c("f" = "Женщины: 40.2% ", "m" = "Мужчины: 59.8% "))

#total <- sum(gender_count$n)
#percent_f <- round(gender_count$n[gender_count$sex == "f"] / total * 100, 1) 
#percent_m <- round(gender_count$n[gender_count$sex == "m"] / total * 100, 1) # как мы считали проценты в подписи 


# Переходим к астрологии. Проверили, сколько людей указали знак и с чем будем работать ------------

profiles_with_attitude %>%
  filter(!is.na(sign)) %>% # отфильтровали без NA
  count(sign) %>% # здесь уже считаем процент знаков к общему количеству, указавших знаки пользователей, т.е. без NA-значений
  mutate(percent = n / sum(n) * 100) # убедились, что знаки распределены равномерно


# Строим график распределения пользователей по возрасту и полу ------------

ggplot()+
  geom_histogram(data = profiles_with_attitude, 
                 aes(x = age, fill = sex),
                 binwidth = 1, position = "identity", color = "white",
                 alpha = 0.7
  ) +
  coord_cartesian(xlim = c(18, 70)) +  # обрезаем ось X до 70, т.к. после 69 уже идут выбросы-приколы
  scale_x_continuous(breaks = seq(20, 70, by = 10)) + # будем ориентироваться по круглым десяткам
  labs(
    title = "Распределение пользователей по возрасту и полу",
    x = "Возраст",
    y = "Число пользователей",
    fill = "Пол"
  ) +
  scale_fill_manual(values = c("f" = "#CD6090", "m" = "#5f9ea0"),
                    labels = c("f" = "Ж", "m" = "М")) + # подписи для значений
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12, margin = margin(b = 9)),
    axis.title = element_text(size = 10),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.title.x = element_text(margin = margin(t = 7)),
    legend.position = "right",
    panel.grid.major = element_line(linewidth = 0.2) # оставили линии сетки, но сделали тоньше: удобно отслеживать внутри возраста пятилетки, при этом не загромождая подписи x
  )


# Создаем датасет с attitude, sex, n, share % -----------------------------


gender_profiles <- profiles_with_attitude %>% 
  filter(!is.na(attitude)) %>%  # считаем без NA значений (NA в sign = NA в attitude)
  count(sex, attitude) %>% # получаем комбинации значений колонок sex и attitude 
  group_by(sex) %>% # сгруппировали по полу
  mutate(share = n / sum(n) * 100) %>% 
  ungroup() %>% 
  mutate(
    attitude = factor(attitude, levels = c(
      "doesn't matter",
      "no info",
      "fun to think about",
      "matters a lot")
    ))
# Рисуем график-бабочку "Отношение к астрологии в зависимости от пола" ------------

# Женская половина --------------------------------------------------------

p_f <- gender_profiles %>%
  filter(sex == "f") %>% 
  ggplot(aes(x = share, y = attitude)) +
  geom_col(fill = "#ef33f1", width = 0.6, colour = NA, alpha = 0.5) +
  geom_text(aes(label = paste0(round(share, 1), "%")),
            hjust = 1.2, size = 6, color = "#333333") +
  scale_x_reverse(limits = c(55, 0), expand = c(0, 0)) +
  scale_y_discrete(labels = NULL) +
  labs(title = "Женщины", x = NULL, y = NULL) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "#ef33f1", size = 20),
    plot.margin = margin(0, 0, 0, 0)
  )
p_f

# Мужская половина --------------------------------------------------------

p_m <- gender_profiles %>%
  filter(sex == "m") %>% 
  ggplot(aes(x = share, y = attitude)) +
  geom_col(fill = "#00c1ff", width = 0.6, colour = NA, alpha = 0.5) +
  geom_text(aes(label = paste0(round(share, 1), "%")),
            hjust = -0.2, size = 6, color = "#333333") +
  scale_x_continuous(limits = c(0, 55), expand = c(0, 0)) +
  scale_y_discrete(labels = NULL) +
  labs(title = "Мужчины", x = NULL, y = NULL) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "#00c1ff", size = 20),
    plot.margin = margin(0, 0, 0, 0)
  )
p_m

# Середина ----------------------------------------------------------------

labels_df <- gender_profiles %>%
  distinct(attitude)

p_labels <- ggplot(labels_df, 
                   aes(x = 1, y = attitude,
                       label = case_when(
                         attitude == "doesn't matter" ~ "Не имеет\nзначения",
                         attitude == "no info" ~ "Мнение\nне указано",
                         attitude == "fun to think about" ~ "Занятно",
                         attitude == "matters a lot" ~ "Это\nважно"
                       ))) +
  geom_text(family = "roboto_condensed", size = 5.5, fontface = "bold", color = "#eeeeee", hjust = 0.5) +
  scale_x_continuous(limits = c(0.9, 1.1)) +
  labs(title = "Отношение:", x = NULL, y = NULL) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "#eeeeee", size = 20),
        plot.margin = margin(0, 0, 0, 0) 
  )
p_labels

combined <- (p_f + p_labels + p_m) +
  plot_layout(widths = c(3, 0.5, 3)) +
  plot_annotation(
    title    = "Отношение к астрологии в зависимости от пола",
    subtitle = "Доля пользователей OkCupid по каждой категории",
    caption  = "Источник: OkCupid Profiles Dataset, 2012",
    theme = theme(
      plot.title = element_text(face = "bold",
                                size = 25,
                                hjust = 0.5, 
                                color = "#b7b7b7",
                                margin = margin(b = 10)),
      plot.subtitle = element_text(size = 16, color = "#b7b7b7", hjust = 0.5),
      plot.caption  = element_text(size = 12, color = "#b7b7b7", hjust = 0)
    )
  ) &
  theme(
    text = element_text(family = "roboto_condensed"), # по идее должно сработать на все графики
    plot.background = element_rect(fill = "#171717", colour = NA),
    panel.background = element_rect(fill = "#171717", colour = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),  
    axis.ticks = element_blank(),
    plot.margin = margin(20, 10, 10, 10) 
  )
combined