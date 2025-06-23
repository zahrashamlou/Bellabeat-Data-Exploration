
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

#  Read the csv-Manually
#  combine 2 DFs having the same columns:dailyActivity_merged <- bind_rows(daily1, daily2) or dailyActivity_merged <- rbind(daily1, daily2)


dailyActivity_merged1 <- read_csv("C:/Users/zsham/OneDrive/google_data_analysis_bellabeat/data/First_Collection/dailyActivity_merged.csv")
dailyActivity_merged2 <- read_csv("C:/Users/zsham/OneDrive/google_data_analysis_bellabeat/data/Second_Collection/dailyActivity_merged.csv")
dailyActivity_merged <- rbind(dailyActivity_merged1, dailyActivity_merged2)
rm(dailyActivity_merged1, dailyActivity_merged2)

#  check for duplicate rows based on Id and ActivityDate
dailyActivity_merged %>%
  group_by(Id, ActivityDate) %>%
  filter(n() > 1)

# Group and aggrigate
dailyActivity_merged <- dailyActivity_merged %>%
  group_by(Id, ActivityDate) %>%
  summarise(
    across(where(is.numeric), sum, na.rm = TRUE),
    .groups = "drop"
  )

View(dailyActivity_merged)
str(dailyActivity_merged)

#  Check whether TrackerDistance  and TotalDistance are the same
which(dailyActivity_merged$TrackerDistance != dailyActivity_merged$TotalDistance)
dailyActivity_merged[!(dailyActivity_merged$TrackerDistance == dailyActivity_merged$TotalDistance), ]



#  check for Duplicates
duplicates <- dailyActivity_merged[duplicated(dailyActivity_merged), ]
#  or
any(duplicated(dailyActivity_merged))


#  Formating
dailyActivity_merged <- dailyActivity_merged %>%
  mutate(ActivityDate = as.Date(ActivityDate, format = "%m/%d/%Y"))


#  Check for missing values
summary(dailyActivity_merged)
colSums(is.na(dailyActivity_merged))


#  Create new calculated columns, total active distance
dailyActivity_merged <- dailyActivity_merged %>%
  mutate(total_active_dis = VeryActiveDistance + ModeratelyActiveDistance + LightActiveDistance) #  + SedentaryActiveDistance)

#  Create new calculated columns, total active minutes
dailyActivity_merged <- dailyActivity_merged %>%
  mutate(total_active_min = VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes) # + SedentaryMinutes)

#  Create new calculated columns, Weekday
dailyActivity_merged$Weekday <- weekdays(as.Date(dailyActivity_merged$ActivityDate))


#  Create a mapping from original Id to numeric short Id
# dailyActivity_merged <- dailyActivity_merged %>%
#   rename(original_Id = Id)
# dailyActivity_merged$Id <- as.integer(factor(dailyActivity_merged$original_Id))


#  Viz
ggplot(data = dailyActivity_merged) + geom_point(mapping = aes(x = total_active_min , y = total_active_dis, colour = Id)) + facet_wrap(~Id)
ggplot(data = dailyActivity_merged) + geom_point(mapping = aes(x = total_active_min , y = total_active_dis, colour = Weekday)) + facet_wrap(~Weekday)
ggplot(data = dailyActivity_merged) + geom_point(mapping = aes(x = total_active_min , y = total_active_dis, colour = Weekday)) 
ggplot(data = dailyActivity_merged) + geom_point(mapping = aes(x = total_active_min , y = total_active_dis), colour = "skyblue") # + facet_wrap(~Id)

#  Data Distributions
library(ggplot2)
ggplot(dailyActivity_merged, aes(x = TotalSteps)) + geom_histogram(binwidth = 1000  , color = "white", fill = "skyblue") + theme_minimal()
ggplot(dailyActivity_merged, aes(x = Calories)) + geom_histogram(binwidth = 200, color = "white", fill = "skyblue") + theme_minimal()
ggplot(dailyActivity_merged, aes(x = total_active_dis)) + geom_histogram(color = "white", fill = "orange") + theme_minimal()
ggplot(dailyActivity_merged, aes(x = total_active_min)) + geom_histogram(color = "white", fill = "orange") + theme_minimal()


#   Stacked Total Distance by User and Weekday
ggplot(
  dailyActivity_merged %>%
    group_by(Id, Weekday) %>%
    summarise(total_active_dis = sum(total_active_dis), .groups = "drop"), 
  aes(x = as.factor(Id), y = total_active_dis, fill = Weekday)) +
  geom_col(position = "stack", width = 0.7) +
  labs(x = "Users", y = "Total Active Distance", title = "Stacked Total Distance by User and Weekday") +
  theme_minimal()+
  theme(axis.text.x = element_blank()) 



#   Time Series
#   Mean Active Distance per day, with max-min range:
ggplot(
  dailyActivity_merged %>%
    group_by(ActivityDate) %>%
    summarise(
      mean_active_dis = mean(total_active_dis),
      min_active_dis = min(total_active_dis),
      max_active_dis = max(total_active_dis),
      .groups = "drop"), 
  aes(x = ActivityDate, y = mean_active_dis)) +
  geom_point(alpha = 0.7) +
  geom_ribbon(aes(ymin = min_active_dis, ymax = max_active_dis), fill = "lightblue", alpha = 0.3) +
  labs(x = "Date", y = "Mean Active Distance", title = "Time Series of Activity per User") +
  theme_minimal()



#   Mean Active Distance per day,  with max-min range:
ggplot(
  dailyActivity_merged %>%
    group_by(ActivityDate) %>%
    summarise(
      mean_active_dis = mean(total_active_dis),
      min_active_dis = min(total_active_dis),
      max_active_dis = max(total_active_dis),
      .groups = "drop"
    ),
  aes(x = ActivityDate, y = mean_active_dis)
) +
  geom_point(color = "steelblue", alpha = 0.7) +  # mean points
  geom_linerange(aes(ymin = min_active_dis, ymax = max_active_dis), color = "gray50", alpha = 0.5, size = 1) +
  labs(x = "Date", y = "Active Distance", title = "Time Series with Vertical Range (Min to Max)") +
  theme_minimal()


#   Mean Active Distance per day,  with confidence interval. mean ± standard error and add error bars:
# Calculate mean and standard error per date
summary_df <- dailyActivity_merged %>%
  group_by(ActivityDate) %>%
  summarise(
    mean_active_dis = mean(total_active_dis),
    se_active_dis = sd(total_active_dis) / sqrt(n()),
    .groups = "drop"
  )
#   Plot with error bars
ggplot(summary_df, aes(x = ActivityDate, y = mean_active_dis)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_active_dis - se_active_dis, ymax = mean_active_dis + se_active_dis),
                width = 0.2, color = "gray30", alpha = 0.8) +
  labs(x = "Date", y = "Mean Active Distance", title = "Time Series with Standard Error Bars") +
  theme_minimal()



#   Avg Steps Over Weekdays , could show behavioral trends
#   plot with 0.95 confidence intervals
dailyActivity_merged %>%
  mutate(Weekday = factor(Weekday,
                          levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  group_by(Weekday) %>%
  summarise(
    AvgSteps = mean(TotalSteps, na.rm = TRUE),
    SE = sd(TotalSteps, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    CI_low = AvgSteps - 1.96 * SE,
    CI_high = AvgSteps + 1.96 * SE
  ) %>%
  ggplot(aes(x = Weekday, y = AvgSteps)) +
  geom_line(group = 1, color = "steelblue") +
  geom_point(color = "steelblue") +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2, color = "darkgray") +
  labs(title = "Average Steps by Weekday with 95% CI", x = "Weekday", y = "Average Steps") +
  theme_minimal()




#   Avg Total Distance Over Weekdays  , could show behavioral trends
#   plot with 0.95 confidence intervals 
dailyActivity_merged %>%
  mutate(Weekday = factor(Weekday,
                          levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  group_by(Weekday) %>%
  summarise(
    AvgDistance = mean(TotalDistance, na.rm = TRUE),
    SE = sd(TotalDistance, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    CI_low = AvgDistance - 1.96 * SE,
    CI_high = AvgDistance + 1.96 * SE
  ) %>%
  ggplot(aes(x = Weekday, y = AvgDistance)) +
  geom_line(group = 1, color = "darkgreen") +
  geom_point(color = "darkgreen") +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2, color = "darkgray") +
  labs(title = "Average Distance by Weekday with 95% CI", x = "Weekday", y = "Average Distance (km)") +
  theme_minimal()




#   Avg Total Calories Over Weekdays  , could show behavioral trends
#   plot with 0.95 confidence intervals 
dailyActivity_merged %>%
  mutate(Weekday = factor(Weekday,
                          levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  group_by(Weekday) %>%
  summarise(
    AvgCalories = mean(Calories, na.rm = TRUE),
    SE = sd(Calories, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    CI_low = AvgCalories - 1.96 * SE,
    CI_high = AvgCalories + 1.96 * SE
  ) %>%
  ggplot(aes(x = Weekday, y = AvgCalories)) +
  geom_line(group = 1, color = "darkgreen") +
  geom_point(color = "darkgreen") +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2, color = "darkgray") +
  labs(title = "Average Calories by Weekday with 95% CI", x = "Weekday", y = "Average Calories") +
  theme_minimal()



#   Activity Level Breakdown (Bar Chart)
#   Goal: Show how much time users spend in different activity intensity levels.
activity_levels <- dailyActivity_merged %>%
  summarise(
    Sedentary = mean(SedentaryMinutes),
    LightlyActive = mean(LightlyActiveMinutes),
    FairlyActive = mean(FairlyActiveMinutes),
    VeryActive = mean(VeryActiveMinutes)
  ) %>%
  #   transform data from a "wide" format to a "long" format
  pivot_longer(everything(), names_to = "ActivityLevel", values_to = "Minutes")

ggplot(activity_levels, aes(x = ActivityLevel, y = Minutes, fill = ActivityLevel)) +
  geom_col() +
  labs(title = "Average Minutes by Activity Level", y = "Minutes") +
  theme_minimal()


#   Steps by Day of the Week (Box Plot), 
#   Median Q2, Q1, Q3,  The range of data considered "normal"( ≥ Q1 − 1.5 × IQR and ≤ Q3 + 1.5 × IQR)
#   Interquartile Range (IQR): distance between Q3 and Q1 (IQR = Q3 - Q1)
dailyActivity_merged %>%
  mutate(Weekday = factor(Weekday,
                          levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  ggplot(aes(x = Weekday, y = TotalSteps)) +
    geom_boxplot(fill = "skyblue") +
    labs(title = "Steps by Weekday", x = "Weeksay", y = "Total Steps") +
    theme_minimal()
  

#   Calories Burned vs Steps (Bubble Plot)
#   Goal: Combine three variables: steps, calories, and activity level.

ggplot(dailyActivity_merged, aes(x = TotalSteps, y = Calories, size = VeryActiveMinutes)) +
  geom_point(alpha = 0.6, color = "tomato") +
  labs(title = "Calories vs Steps (Size = Very Active Minutes)",
       x = "Total Steps", y = "Calories Burned") +
  theme_minimal()



#   Cumulative Steps per User (Line Chart or Area Chart)
#   Goal: Compare activity between users over time.
cumulative_steps <- dailyActivity_merged %>%
  group_by(Id, ActivityDate) %>%
  summarise(Steps = sum(TotalSteps)) %>%
  arrange(Id, ActivityDate) %>%     # <-- this ensures sorting!
  group_by(Id) %>%
  mutate(CumulativeSteps = cumsum(Steps))

ggplot(cumulative_steps, aes(x = ActivityDate, y = CumulativeSteps, color = factor(Id))) +
  geom_line() +
  labs(title = "Cumulative Steps Over Time", x = "Date", y = "Cumulative Steps") +
  theme_minimal()



#   Total Steps vs Calories Burned (Scatter Plot)
#   Explore how physical activity relates to energy expenditure.
ggplot(dailyActivity_merged, aes(x = TotalSteps, y = Calories)) +
  geom_point(alpha = 0.5, color = "tomato") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Steps vs Calories Burned", x = "Steps", y = "Calories") +
  theme_minimal()

