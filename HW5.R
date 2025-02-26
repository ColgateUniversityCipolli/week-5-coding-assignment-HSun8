# Henry Sun 
# Homework 5 
# 2/26/25


# Step 0
library("stringr")
library("jsonlite")
library(tidyverse)


#Step 1, Front Bottoms Example
current.filename = "The Front Bottoms-Talon of the Hawk-Au Revoir (Adios).json"
currentfile.split = str_split_1(current.filename, "-")

# artist, album, track
artist = currentfile.split[1]
album = currentfile.split[2]
track = str_sub(currentfile.split[3], start = 0, end = -6)

# Essentia Output
curr.json = (fromJSON(paste("EssentiaOutput/", current.filename, sep="")))

# lowlevel
overall.loudness = curr.json$lowlevel$loudness_ebu128$integrated
spectral.energy = curr.json$lowlevel$spectral_energy$mean
dissonance = curr.json$lowlevel$dissonance$mean
pitch.salience = curr.json$lowlevel$pitch_salience$mean

# rhythm
bpm = curr.json$rhythm$bpm
beats.loudness = curr.json$rhythm$beats_loudness$mean
danceability = curr.json$rhythm$danceability

# tonal
tuning.freq = curr.json$tonal$tuning_frequency

# data from example file
currfile.data = c(overall.loudness, spectral.energy, dissonance, pitch.salience, bpm, beats.loudness, 
                  danceability, tuning.freq)


# Step 2, repeat for all .json files
# load files
all.files = (list.files("EssentiaOutput", recursive=TRUE))

# only check files with .json 
json.check = str_count(all.files, pattern=".json")
all.json = all.files[which(json.check == 1)]

# create data frame
json.data = tibble(
  artist = character(),
  album = character(),
  overall.loudness = numeric(),
  spectral.energy = numeric(),
  dissonance = numeric(),
  pitch.sailence = numeric(),
  tempo.bpm = numeric(),
  beat.loudness = numeric(),
  danceability = numeric(),
  tuning.freq = numeric()
)

# repeat same for all files in step 1
for (i in 1:length(all.json)){
  curr.file = fromJSON(paste("EssentiaOutput/", all.json[i], sep = ""))
  currfile.split =  str_split_1(all.json[i], "-")
  curr.artist = currfile.split[1]
  curr.album = currfile.split[2]
  curr.track = str_sub(currfile.split[3], start = 0, end = -6)
  
  overall.loudness = curr.file$lowlevel$loudness_ebu128$integrated
  spectral.energy = curr.file$lowlevel$spectral_energy$mean
  dissonance = curr.file$lowlevel$dissonance$mean
  pitch.salience = curr.file$lowlevel$pitch_salience$mean
  bpm = curr.file$rhythm$bpm
  beats.loudness = curr.file$rhythm$beats_loudness$mean
  danceability = curr.file$rhythm$danceability
  tuning.freq = curr.file$tonal$tuning_frequency
  
  # insert into tibble
  # create row of data for each song, add to tibble
  json.data = bind_rows(json.data, tibble(
    artist = curr.artist, album = curr.album, track = curr.track, overall.loudness, 
    spectral.energy, dissonance, pitch.salience, bpm, beats.loudness, 
    danceability, tuning.freq))
}


# Step 3
# read csv into a tibble
essentia.file = read_csv("EssentiaOutput/EssentiaModelOutput.csv") 
# create new cols using mutate, 
# use rowMeans() + cbind to find means for some features
essentia.data <- essentia.file |>
  mutate(valence = rowMeans(bind_cols(deam_valence, emo_valence, muse_valence)),
         arousal = rowMeans(bind_cols(deam_arousal, emo_arousal, muse_arousal)),
         aggressive = rowMeans(bind_cols(eff_aggressive, nn_aggressive)),
         happy = rowMeans(bind_cols(eff_happy, nn_happy)),
         party = rowMeans(bind_cols(eff_party, nn_party)),
         relaxed = rowMeans(bind_cols(eff_relax, nn_relax)),
         sad = rowMeans(bind_cols(eff_sad, nn_sad)),
         acoustic = rowMeans(bind_cols(eff_acoustic, nn_acoustic)),
         electric = rowMeans(bind_cols(eff_electronic, nn_electronic)),
         instrumental = rowMeans(bind_cols(eff_instrumental, nn_instrumental))) |>
# rename timbreBright
  rename(timbreBright = eff_timbre_bright) |>
# select features
  select(artist, album, track, valence, arousal, aggressive, happy, party, relaxed,
         sad, acoustic, electric, instrumental, timbreBright)


# Step 4
# join all data into one file, grouping by artist, album, track (eliminate dupes)
liwc.data = read_csv("LIWCOutput/LIWCOutput.csv")




# some.data = left_join(df.essentia, df.json)
# all.data = left_join(some.data, df.liwc)
# #check dimensions of data, should be dim(df.json) + dim(df.essentia) - 3 cols
# #duplicate cols for artist, track, album
# #tried to merge in one step but doesnt work
# dim(some.data)
# dim(all.data)
# 
# # all.data = merge(some.data, liwc.file)
# # #View(some.data)
# # #View(all.data)
# # dim(all.data)
# 
# names(all.data)[which(names(all.data) == "function.")] = "funct"
# View(all.data)


#Step 5
# tried to use which(), didn't work

training.data = subset(all.data, all.data$track != "Allentown")
testing.data = subset(all.data, all.data$track == "Allentown")

training.csv = write.csv(x=training.data, "trainingdata.csv")
testing.csv = write.csv(x=testing.data, "testingdata.csv")

# Coding challenge, make a graph 
####################################
# Load Data
####################################
dat <- read_csv("trainingdata.csv")
####################################
# Select data for plot
####################################
df <- dat %>%
  dplyr::select("relaxed", "artist") %>%
  filter(!is.na(!!sym("artist")))
####################################
# Create Plot
####################################
p <- ggplot(df, aes(x = fct_rev(!!sym("artist")), y = !!sym("relaxed"))) +
  geom_violin(fill = "lightgrey", trim = FALSE) +
  geom_boxplot(fill = "white", width = 0.1) +
  geom_jitter(color = "black", size = 0.4, alpha = 0.9, width = 0.125) +
  get("theme_bw")() +
  xlab("relaxed") +
  ylab("artist") +
  ggtitle("", "") +
  coord_flip()
####################################
# Print Plot
####################################
p


