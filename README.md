# dplyr-lab3
##EX1

##Q1
```{r}
```{r}
spotify <- readRDS("C:/Users/DELL/Desktop/dplyr-lab3/top-spotify-hits-2000-2019.Rds")
library(dplyr)
num_songs <- spotify %>% distinct(song) %>% nrow()
num_artists <- spotify %>% distinct(artist) %>% nrow()
num_genres <- spotify %>% distinct(genre) %>% nrow()
cat("The dataset contains", num_songs, "songs," ,num_artists,"artists, and ", num_genres, "musical genres.")
```

##Q2
```{r}
library(knitr)
# Grouping by year and computing distinct artists per year
artists_per_year <- spotify %>%
  group_by(year) %>%
  summarise(num_artists = n_distinct(artist))

# Displaying the table in the knitted document
knitr::kable(artists_per_year, 
             col.names = c("Year", "Number of Distinct Artists"),
             caption = "Number of Distinct Artists per Year")
```

##Q3
```{r}
# Grouping by artist and computing distinct songs for each artist
popular_artist <- spotify %>%
  group_by(artist) %>%
  summarise(num_songs = n_distinct(song)) %>%
  arrange(-num_songs) %>%
  head(1)

artist_name <- popular_artist$artist
artist_songs <- popular_artist$num_songs

# Including the result in the text
cat("The most popular artist in the dataset is", artist_name, "with", artist_songs, "songs.")

```

##Q4
```{r}
# Ensure we are working with unique songs
unique_songs <- spotify %>% distinct(song)%>% nrow()

tempo <- spotify %>%
  group_by(genre) %>%
  summarise(
    min_bpm = min(tempo, na.rm = TRUE),
    max_bpm = max(tempo, na.rm = TRUE),
    mean_bpm = mean(tempo, na.rm = TRUE),
    median_bpm = median(tempo, na.rm = TRUE),
    number_of_songs = n())

knitr::kable(tempo, caption = "BPM Statistics by Musical Genre")
```

##Q5
```{r}
median_stats_per_year <- spotify %>%
  group_by(year) %>%
  summarise(
    nrgy = median(energy),
    dnce = median(danceability)
  )

# Display the results:
knitr::kable(median_stats_per_year, caption = "Median energy and danceability per year")

```

##Q6
```{r}
library(ggplot2)

ggplot(median_stats_per_year, aes(x = year)) +
  geom_line(aes(y = nrgy, color = 'energy')) +
  geom_line(aes(y = dnce, color = 'danceability')) +
  labs(title = 'Temporal Evolution of Median Annual Energy and Danceability',
    y = 'Value',
    x = 'Year',
    color = 'Metric')
```

##EX2

##Q1
```{r}
library(dplyr)

dropout <- readRDS("C:/Users/DELL/Desktop/dplyr-lab3/dropout.Rds")
median_age <- dropout %>%
  group_by(Gender, `Marital status`) %>%
  summarise(median_age = median(`Age at enrollment`, na.rm = TRUE),.groups = 'keep')
knitr::kable(median_age, caption = "the median age at enrollment conditioned  on the Gender and on the marital status")
```

##Q2
```{r}
library(tidyr)
wide_format <- median_age %>%
pivot_wider(names_from = Gender, values_from = median_age)
knitr::kable(wide_format, caption = "Median Age at Enrollment by Marital Status and Gender")
```

##Q3
```{r}
library(dplyr)
result <- dropout %>%
  group_by(Target) %>%
  summarise(
    mean_CU1_credited = mean(`Curricular units 1st sem (credited)`, na.rm = TRUE),
    mean_CU1_enrolled = mean(`Curricular units 1st sem (enrolled)`, na.rm = TRUE),
    mean_CU1_evaluations = mean(`Curricular units 1st sem (evaluations)`, na.rm = TRUE),
    mean_CU1_approved = mean(`Curricular units 1st sem (approved)`, na.rm = TRUE),
    mean_CU1_grade = mean(`Curricular units 1st sem (grade)`, na.rm = TRUE),
    mean_CU1_no_evaluations = mean(`Curricular units 1st sem (without evaluations)`, na.rm = TRUE),
    mean_CU2_credited = mean(`Curricular units 2nd sem (credited)`, na.rm = TRUE),
    mean_CU2_enrolled = mean(`Curricular units 2nd sem (enrolled)`, na.rm = TRUE),
    mean_CU2_evaluations = mean(`Curricular units 2nd sem (evaluations)`, na.rm = TRUE),
    mean_CU2_approved = mean(`Curricular units 2nd sem (approved)`, na.rm = TRUE),
    mean_CU2_grade = mean(`Curricular units 2nd sem (grade)`, na.rm = TRUE),
    mean_CU2_no_evaluations = mean(`Curricular units 2nd sem (without evaluations)`, na.rm = TRUE))
print(result)
```

##Q4
```{r}
reshaped_data <- result %>%
  pivot_longer(-Target, names_to = "Units", values_to = "Values") %>%
  pivot_wider(names_from = Target, values_from = Values)
knitr::kable(reshaped_data, caption = "Summary Statistics for Curricular Units")
