library(tidyverse)
library(rjson)

setwd("~/Documents/seals/harbor_seal_vm")


test_actual <- read_csv("image-level-split/test/test_anno.csv")
#names(test_actual) <- c("Tile", "X1", "Y1", "X2", "Y2", "class")
test_actual$actual <- TRUE
test_actual <- select(test_actual, !"...1")

test_actual$class <- case_when(test_actual$class == "adult" ~ "Harbor Seal",
                               test_actual$class == "other" ~ "Other")

test_actual_counts <- test_actual %>% 
  #filter(class != "Other") %>%
  group_by(tile) %>%
  #count(class)
  summarise(actual_count=n())


test_predicted_json <- fromJSON(file="image-level-split/test_detections.json")

test_predicted_full <- data.frame(matrix(ncol=7, nrow=0))
names(test_predicted_full) <- c("tile", "X1", "Y1", "X2", "Y2", "class", "actual")
test_predicted_full <- transform(test_predicted_full, tile = as.character(tile), 
                            X1 = as.numeric(X1), Y1 = as.numeric(Y1),
                            X2 = as.numeric(X2), Y2 = as.numeric(Y2),
                            class = as.character(class))

test_predicted_limited <- data.frame(matrix(ncol=7, nrow=0))
names(test_predicted_limited) <- c("tile", "X1", "Y1", "X2", "Y2", "class", "actual")
test_predicted_limited <- transform(test_predicted_limited, tile = as.character(tile), 
                                 X1 = as.numeric(X1), Y1 = as.numeric(Y1),
                                 X2 = as.numeric(X2), Y2 = as.numeric(Y2),
                                 class = as.character(class))

for (image in names(test_predicted_json)) {
  #print(image)
  for (anno in test_predicted_json[[image]]) {
    label <- case_when(anno$label == 0 ~ "Harbor Seal",
                       anno$label == 1 ~ "Other")
    
    #test_predicted_json[[image]][[1]]$label
    new_row = data.frame(tile = substring(image, 24),
                         X1 = anno$box[1], 
                         Y1 = anno$box[2], 
                         X2 = anno$box[3], 
                         Y2 = anno$box[4], 
                         class = label, 
                         actual = FALSE)
    if (new_row$X2 < 499 & new_row$Y2 < 499 & new_row$X1 > 0 & new_row$Y1 > 0) {
      test_predicted_limited <- rows_append(test_predicted_limited, new_row)
    }
    test_predicted_full <- rows_append(test_predicted_full, new_row)
  }
  
  
}

test_predicted_full_counts <- test_predicted_full %>%
  group_by(tile) %>%
  summarise(pred_count=n())

test_predicted_limited_counts <- test_predicted_limited %>%
  group_by(tile) %>%
  summarise(pred_count=n())

test1 <- full_join(test_predicted_limited_counts, test_actual_counts)

test2 <- full_join(test_predicted_full_counts, test_actual_counts)

lmRaw <- lm(pred_count ~ actual_count, test2)
lmRaw
summary(lmRaw)$r.squared



lmAdjusted <- lm(pred_count ~ actual_count, test1)
lmAdjusted
summary(lmAdjusted)$r.squared


p1 <- test1 %>%
  ggplot(aes(x = actual_count, y = pred_count)) +
  geom_count(alpha = 0.5) + 
  geom_abline(slope = 1, intercept = 0, color = "red") + 
  labs(title = "Harbor Seal Counts Overall, Edges Removed")

p1

p2 <- test2 %>%
  ggplot(aes(x = actual_count, y = pred_count)) +
  geom_count(alpha = 0.5) + 
  geom_abline(slope = 1, intercept = 0, color = "red") + 
  labs(title = "Harbor Seal Counts Overall, All Anno")

p2

test3 <- test1
names(test3) <- c("tile", "pred_count_limited", "actual_count")
test3 <- inner_join(test3, test2)
names(test3) <- c("tile", "pred_count_limited", "actual_count", "pred_count_full")

p_both <- test3 %>%
  ggplot() +
  geom_count(aes(x = actual_count, y = pred_count_limited, color = "Adjusted"), alpha = 0.5) + 
  stat_smooth(aes(x = actual_count, y = pred_count_limited), color = "#F8766D", method = "lm", fullrange = TRUE, se = FALSE, linewidth = 1) + 
  geom_count(aes(x = actual_count, y = pred_count_full, color = "Raw"), alpha = 0.5) + 
  stat_smooth(aes(x = actual_count, y = pred_count_full), color = "#00BFC4", method = "lm", fullrange = TRUE, se = FALSE, linewidth = 0.5) + 
  geom_abline(slope = 1, intercept = 0, color = "grey") +
  labs(title = "Harbor Seal Counts",
       x = "Ground Truth Counts",
       y = "Model Output Counts") +
  theme_bw()

p_both$labels$colour = "Color"
p_both$labels$size = "Number of Tiles"

p_both


# no_seals <- filter(test, is.na(actual_count))
# too_many_seals <- filter(test, pred_count - actual_count > 0)
# not_enough_seals <- filter(test, actual_count - pred_count > 0)

test_predicted_limited_counts_class <- test_predicted_limited %>%
  group_by(tile, class) %>%
  summarise(pred_count=n())

test_predicted_full_counts_class <- test_predicted_full %>%
  group_by(tile, class) %>%
  summarise(pred_count=n())

test_actual_counts_class <- test_actual %>% 
  group_by(tile, class) %>%
  summarise(actual_count=n())


test1_class <- inner_join(test_predicted_limited_counts_class, test_actual_counts_class)
test2_class <- inner_join(test_predicted_full_counts_class, test_actual_counts_class)


testHS <- test2_class %>% filter(class == "Harbor Seal")
testO <- test2_class %>% filter(class == "Other")

lmRawHS <- lm(pred_count ~ actual_count, testHS)
lmRawHS
summary(lmRawHS)$r.squared
summary(lmRawHS)$coefficients

lmRawO <- lm(pred_count ~ actual_count, testO)
lmRawO
summary(lmRawO)$r.squared
summary(lmRawO)$coefficients


testHS1 <- test1_class %>% filter(class == "Harbor Seal")
testO1 <- test1_class %>% filter(class == "Other")

lmAdjHS <- lm(pred_count ~ actual_count, testHS1)
lmAdjHS
summary(lmAdjHS)$r.squared
summary(lmAdjHS)$coefficients

lmAdjO <- lm(pred_count ~ actual_count, testO1)
lmAdjO
summary(lmAdjO)$r.squared
summary(lmAdjO)$coefficients



p3 <- test1_class %>%
  ggplot(aes(x = actual_count, y = pred_count)) +
  geom_count(alpha = 0.5) + 
  facet_wrap(vars(class)) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Harbor Seal Counts by Class, Edges removed")
p3

p4 <- test2_class %>%
  ggplot(aes(x = actual_count, y = pred_count, color = class)) +
  geom_count(alpha = 0.5) + 
  facet_wrap(vars(class)) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Harbor Seal Counts by Class, All Anno")
p4

test3_class <- test1_class
names(test3_class) <- c("tile", "class", "pred_count_limited", "actual_count")
test3_class <- inner_join(test3_class, test2_class)
names(test3_class) <- c("tile", "class", "pred_count_limited", "actual_count", "pred_count_full")

p5 <- test3_class %>%
  ggplot() +
  geom_count(aes(x = actual_count, y = pred_count_limited, color = "Adjusted"), alpha = 0.5) + 
  stat_smooth(aes(x = actual_count, y = pred_count_limited), color = "#F8766D", method = "lm", fullrange = TRUE, se = FALSE, linewidth = 1) + 
  geom_count(aes(x = actual_count, y = pred_count_full, color = "Raw"), alpha = 0.5) + 
  stat_smooth(aes(x = actual_count, y = pred_count_full), color = "#00BFC4", method = "lm", fullrange = TRUE, se = FALSE, linewidth = 0.5) +
  facet_wrap(vars(class)) + 
  geom_abline(slope = 1, intercept = 0, color = "grey") + 
  labs(title = "Harbor Seal Counts by Class",
       x = "Ground Truth Counts",
       y = "Model Output Counts") +
 theme_bw(base_size = 18)


p5$labels$colour = "Color"
p5$labels$size = "Number of Tiles"

p5

p_final <- test3_class %>%
  filter(class == "Harbor Seal") %>%
  ggplot() +
  geom_count(aes(x = actual_count, y = pred_count_limited, color = "Adjusted"), alpha = 0.6) + 
  stat_smooth(aes(x = actual_count, y = pred_count_limited), color = "#F8766D", method = "lm", fullrange = TRUE, se = FALSE, linewidth = 1.5) + 
  geom_count(aes(x = actual_count, y = pred_count_full, color = "Raw"), alpha = 0.3) + 
  stat_smooth(aes(x = actual_count, y = pred_count_full), color = "#00BFC4", method = "lm", fullrange = TRUE, se = FALSE, linewidth = 0.2) + 
  geom_abline(slope = 1, intercept = 0, color = "grey") + 
  labs(x = "Ground Truth Counts",
       y = "Model Output Counts") +
  xlim(0, 20) + ylim(0, 25) +
  theme_bw(base_size = 18)


p_final$labels$colour = "Model Output"
p_final$labels$size = "Number of Tiles"

p_final
ggsave("HS_model_compared_to_truth.png", width = 7, height = 5)

