library(tidyverse)
library(rjson)

setwd("~/Documents/seals/mids-251-elephant-seal")
test_actual <- read_csv("Data/image-level-split/test/test_anno.csv", col_names = F)
names(test_actual) <- c("Tile", "X1", "Y1", "X2", "Y2", "class", "image")

#write.table(test_actual, "Data/image-level-split/detections_with_other_full.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")

test_actual$actual <- TRUE

test_actual$class <- case_when(test_actual$class == "bull" ~ "Bull",
                               test_actual$class == "cow" ~ "Cow",
                               test_actual$class == "pup" ~ "Pup",
                               test_actual$class == "other" ~ "Other")

test_actual_counts <- test_actual %>% 
  filter(class != "Other") %>%
  group_by(Tile) %>%
  summarise(actual_count=n())


test_predicted_json <- fromJSON(file="Data/image-level-split/test_detections_no_other.json")

test_predicted_full <- data.frame(matrix(ncol=8, nrow=0))
names(test_predicted_full) <- c("Tile", "X1", "Y1", "X2", "Y2", "class", "conf", "actual")
test_predicted_full <- transform(test_predicted_full, Tile = as.character(Tile), 
                            X1 = as.numeric(X1), Y1 = as.numeric(Y1),
                            X2 = as.numeric(X2), Y2 = as.numeric(Y2),
                            conf = as.numeric(conf),
                            class = as.character(class))

test_predicted_limited <- data.frame(matrix(ncol=8, nrow=0))
names(test_predicted_limited) <- c("Tile", "X1", "Y1", "X2", "Y2", "class", "conf", "actual")
test_predicted_limited <- transform(test_predicted_limited, Tile = as.character(Tile), 
                            X1 = as.numeric(X1), Y1 = as.numeric(Y1),
                            X2 = as.numeric(X2), Y2 = as.numeric(Y2),
                            conf = as.numeric(conf),
                            class = as.character(class))

for (image in names(test_predicted_json)) {
  #test_predicted_json[[image]]
  for (anno in test_predicted_json[[image]]) {
    label <- case_when(anno$label == 0 ~ "Bull",
                       anno$label == 1 ~ "Cow",
                       anno$label == 2 ~ "Pup",
                       anno$label == 3 ~ "Other")
    
    #test_predicted_json[[image]][[1]]$label
    new_row = data.frame(Tile = substring(image, 32),
                         X1 = anno$box[1], 
                         Y1 = anno$box[2], 
                         X2 = anno$box[3], 
                         Y2 = anno$box[4], 
                         class = label, 
                         conf = anno$score,
                         actual = FALSE)
    if (new_row$X2 < 499 & new_row$Y2 < 499 & new_row$X1 > 0 & new_row$Y1 > 0) {
      test_predicted_limited <- rows_append(test_predicted_limited, new_row)
    }
    test_predicted_full <- rows_append(test_predicted_full, new_row)
  }
}

#write.table(test_predicted_limited, "Data/image-level-split/detections_with_other_adjusted.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")
#write.table(test_predicted_full, "Data/image-level-split/detections_with_other_full.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")


test_predicted_full_counts <- test_predicted_full %>%
  group_by(Tile) %>%
  #count(class)
  summarise(pred_count=n())

test_predicted_limited_counts <- test_predicted_limited %>%
  group_by(Tile) %>%
  #count(class)
  summarise(pred_count=n())

test1 <- inner_join(test_predicted_limited_counts, test_actual_counts)
test2 <- inner_join(test_predicted_full_counts, test_actual_counts)
  
p1 <- test1 %>%
  ggplot(aes(x = actual_count, y = pred_count)) +
  geom_count(alpha = 0.5) + 
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Elephant Seal Counts, Edges removed")
  
p1

p2 <- test2 %>%
  ggplot(aes(x = actual_count, y = pred_count)) +
  geom_count(alpha = 0.5) + 
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Elephant Seal Counts, All Anno")

p2

test3 <- test1
names(test3) <- c("Tile", "pred_count_limited", "actual_count")
test3 <- inner_join(test3, test2)
names(test3) <- c("Tile", "pred_count_limited", "actual_count", "pred_count_full")

p_both <- test3 %>%
  ggplot() +
  geom_count(aes(x = actual_count, y = pred_count_limited, color = "Adjusted"), alpha = 0.5) + 
  stat_smooth(aes(x = actual_count, y = pred_count_limited), color = "#F8766D", method = "lm", fullrange = TRUE, se = FALSE, linewidth = 1) + 
  geom_count(aes(x = actual_count, y = pred_count_full, color = "Raw"), alpha = 0.5) + 
  stat_smooth(aes(x = actual_count, y = pred_count_full), color = "#00BFC4", method = "lm", fullrange = TRUE, se = FALSE, linewidth = 0.5) + 
  geom_abline(slope = 1, intercept = 0, color = "grey") +
  labs(title = "Elephant Seal Counts",
       x = "Ground Truth Counts",
       y = "Model Output Counts") +
  theme_bw()

p_both$labels$colour = "Color"
p_both$labels$size = "Number of Tiles"

p_both


p6 <- test3_class %>%
  ggplot() +
  geom_count(aes(x = actual_count, y = pred_count_limited, color = "Adjusted"), alpha = 0.5) + 
  stat_smooth(aes(x = actual_count, y = pred_count_limited), color = "#F8766D", method = "lm", fullrange = TRUE, se = FALSE, linewidth = 1) + 
  geom_count(aes(x = actual_count, y = pred_count_full, color = "Raw"), alpha = 0.5) + 
  stat_smooth(aes(x = actual_count, y = pred_count_full), color = "#00BFC4", method = "lm", fullrange = TRUE, se = FALSE, linewidth = 0.5) +
  facet_wrap(vars(class)) + 
  geom_abline(slope = 1, intercept = 0, color = "grey") + 
  labs(title = "Elephant Seal Counts by Class",
       x = "Ground Truth Counts",
       y = "Model Output Counts") +
  xlim(0, 47) + ylim(0, 47) + theme_bw()
  

test_predicted_full_counts_class <- test_predicted_full %>%
  group_by(Tile, class) %>%
  summarise(pred_count=n())

test_predicted_limited_counts_class <- test_predicted_limited %>%
  group_by(Tile, class) %>%
  summarise(pred_count=n())

test_actual_counts_class <- test_actual %>% 
  group_by(Tile, class) %>%
  summarise(actual_count=n())

test1_class <- inner_join(test_predicted_limited_counts_class, test_actual_counts_class)
test2_class <- inner_join(test_predicted_full_counts_class, test_actual_counts_class)


test2_class_bull <- test2_class %>%
  filter(class == "Bull")

test2_class_cow <- test2_class %>%
  filter(class == "Cow")

test2_class_pup <- test2_class %>%
  filter(class == "Pup")



lmRawB <- lm(pred_count ~ actual_count, test2_class_bull)
lmRawB
summary(lmRawB)$r.squared

lmRawC <- lm(pred_count ~ actual_count, test2_class_cow)
lmRawC
summary(lmRawC)$r.squared

lmRawP <- lm(pred_count ~ actual_count, test2_class_pup)
lmRawP
summary(lmRawP)$r.squared


test1_class_bull <- test1_class %>%
  filter(class == "Bull")

test1_class_cow <- test1_class %>%
  filter(class == "Cow")

test1_class_pup <- test1_class %>%
  filter(class == "Pup")


lmAdjustedB <- lm(pred_count ~ actual_count, test1_class_bull)
lmAdjustedB
summary(lmAdjustedB)$r.squared

lmAdjustedC <- lm(pred_count ~ actual_count, test1_class_cow)
lmAdjustedC
summary(lmAdjustedC)$r.squared

lmAdjustedP <- lm(pred_count ~ actual_count, test1_class_pup)
lmAdjustedP
summary(lmAdjustedP)$r.squared

# look into jitter and alpha together, reduce amount of noise
p3 <- test1_class %>%
  ggplot(aes(x = actual_count, y = pred_count)) +
  geom_count(alpha = 0.5) + 
  facet_wrap(vars(class)) +
  geom_abline(slope = 1, intercept = 0, color = "red") + 
  labs(title = "Elephant Seal Counts by Class, Edges removed")
p3

p4 <- test2_class %>%
  ggplot(aes(x = actual_count, y = pred_count)) +
  geom_count(alpha = 0.5) + 
  facet_wrap(vars(class)) +
  geom_abline(slope = 1, intercept = 0, color = "red") + 
  labs(title = "Elephant Seal Counts by Class, All Anno")
p4

# one graph -- probably too much going on
p5 <- test1_class %>%
  ggplot(aes(x = actual_count, y = pred_count, color = class)) +
  geom_count(alpha = 0.5) + 
  #facet_wrap(vars(class)) +
  geom_abline(slope = 1, intercept = 0, color = "red") + 
  labs(title = "Elephant Seal Counts by Class, 499 removed", 
       x = "Ground Truth Counts",
       y = "Model Output Counts")
p5
  
# image field in the tables?

test3_class <- test1_class
names(test3_class) <- c("Tile", "class", "pred_count_limited", "actual_count")
test3_class <- inner_join(test3_class, test2_class)
names(test3_class) <- c("Tile", "class", "pred_count_limited", "actual_count", "pred_count_full")

p6 <- test3_class %>%
  filter(class != "Other") %>%
  ggplot() +
  geom_count(aes(x = actual_count, y = pred_count_limited, color = "Adjusted"), alpha = 0.5) + 
  stat_smooth(aes(x = actual_count, y = pred_count_limited), color = "#F8766D", method = "lm", se = FALSE, linewidth = 1.5) + 
  geom_count(aes(x = actual_count, y = pred_count_full, color = "Raw"), alpha = 0.5) + 
  stat_smooth(aes(x = actual_count, y = pred_count_full), color = "#00BFC4", method = "lm", se = FALSE, linewidth = 0.5) +
  facet_wrap(vars(class)) + 
  geom_abline(slope = 1, intercept = 0, color = "grey") + 
  labs(title = "Elephant Seal Counts by Class",
       x = "Ground Truth Counts",
       y = "Model Output Counts") +
  xlim(0, 45) + ylim(0, 45) + theme_bw(base_size = 18)


p6$labels$colour = "Color"
p6$labels$size = "Number of Tiles"

p6

lmRaw <- lm(pred_count ~ actual_count, test2)
summary(lmRaw)$r.squared

lmAdjusted <- lm(pred_count ~ actual_count, test1)
summary(lmAdjusted)$r.squared

  