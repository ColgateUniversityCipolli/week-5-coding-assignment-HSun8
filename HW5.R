# Henry Sun 
# Homework 5 
# 2/26/25

# Step 0

library("stringr")
library("jsonlite")
library(tidyverse)

#Step 1, Front Bottoms Example
current.filename = "The Front Bottoms-Talon of the Hawk-Au Revoir (Adios).json"
(currentfile.split = str_split_1(current.filename, "-"))

#artist, album, track
(artist = currentfile.split[1])
(album = currentfile.split[2])
(track = str_sub(currentfile.split[3], start = 0, end = -6))

#ask professor
curr.json = (fromJSON(paste("EssentiaOutput/", current.filename, sep="")))

#lowlevel
(overall.loudness = curr.json$lowlevel$loudness_ebu128$integrated)
spectral.energy = curr.json$lowlevel$spectral_energy$mean
dissonance = curr.json$lowlevel$dissonance$mean
pitch.salience = curr.json$lowlevel$pitch_salience$mean

#rhythm
bpm = curr.json$rhythm$bpm
beats.loudness = curr.json$rhythm$beats_loudness$mean
danceability = curr.json$rhythm$danceability

#tonal
tuning.freq = curr.json$tonal$tuning_frequency

#data from example file
currfile.data = c(overall.loudness, spectral.energy, dissonance, pitch.salience, bpm, beats.loudness, 
                  danceability, tuning.freq)
#Step 2
# load files
(all.files = (list.files("EssentiaOutput", recursive=TRUE)))

# only check filws with .json 
json.check = str_count(all.files, pattern=".json")
(all.json = all.files[which(json.check == 1)])

# create data frame
df.json = data.frame(artist = rep(x = NA, times=length(all.json)),
                     album = rep(x = NA, times=length(all.json)),
                     track = rep(x = NA, times=length(all.json)),
                     overall.loudness = rep(x = NA, times=length(all.json)),
                     spectral.energy = rep(x = NA, times=length(all.json)),
                     dissonance = rep(x = NA, times=length(all.json)),
                     pitch.salience = rep(x = NA, times=length(all.json)),
                     bpm = rep(x = NA, times=length(all.json)),
                     beats.loudness = rep(x = NA, times=length(all.json)),
                     danceability = rep(x = NA, times=length(all.json)),
                     tuning.freq = rep(x = NA, times=length(all.json)))
# repeat same for all files in step 1

for (i in 1:length(all.json)){
  curr.file = fromJSON(paste("EssentiaOutput/", all.json[i], sep= 
                               ""))
  currfile.split =  str_split_1(all.json[i], "-")
  (curr.artist = currfile.split[1])
  (curr.album = currfile.split[2])
  (curr.track = str_sub(currfile.split[3], start = 0, end = -6))
  
  (overall.loudness = curr.file$lowlevel$loudness_ebu128$integrated)
  spectral.energy = curr.file$lowlevel$spectral_energy$mean
  dissonance = curr.file$lowlevel$dissonance$mean
  pitch.salience = curr.file$lowlevel$pitch_salience$mean
  bpm = curr.file$rhythm$bpm
  beats.loudness = curr.file$rhythm$beats_loudness$mean
  danceability = curr.file$rhythm$danceability
  tuning.freq = curr.file$tonal$tuning_frequency
  
  currfile.data = c(curr.artist, curr.album, curr.track,
                    overall.loudness, spectral.energy, 
                    dissonance, pitch.salience, bpm,
                    beats.loudness, danceability, tuning.freq)
  
  # How do i add a vector as a row to a dataframe?
  df.json[i,] <-currfile.data
}
#View(df.json)

# Step 3
essentia.file = read_csv("EssentiaOutput/EssentiaModelOutput.csv") 
df.essentia <- essentia.file |>
  mutate(valence = rowMeans(cbind(deam_valence, emo_valence, muse_valence)),
         arousal = rowMeans(cbind(deam_arousal, emo_arousal, muse_arousal)),
         aggressive = rowMeans(cbind(eff_aggressive, nn_aggressive)),
         happy = rowMeans(cbind(eff_happy, nn_happy)),
         party = rowMeans(cbind(eff_party, nn_party)),
         relaxed = rowMeans(cbind(eff_relax, nn_relax)),
         sad = rowMeans(cbind(eff_sad, nn_sad)),
         acoustic = rowMeans(cbind(eff_acoustic, nn_acoustic)),
         electric = rowMeans(cbind(eff_electronic, nn_electronic)),
         instrumental = rowMeans(cbind(eff_instrumental, nn_instrumental))) |>
  rename(timbreBright = eff_timbre_bright) |>
  select(artist, album, track, valence, arousal, aggressive, happy, party, relaxed,
         sad, acoustic, electric, instrumental, timbreBright)
  


# df.essentia = data.frame(essentia.file$artist, essentia.file$album, essentia.file$track,
#                           mean(c(essentia.file$deam_valence, essentia.file$emo_valence,
#                           essentia.file$muse_valence)))
# df.essentia = data.frame(artist = rep(x = NA, times=length(dim(essentia.file)[1])),
#                          album = rep(x = NA, times=length(dim(essentia.file)[1])),
#                          track = rep(x = NA, times=length(dim(essentia.file)[1])),
#                          valence  = rep(x = NA, times=length(dim(essentia.file)[1])),
#                          arousal = rep(x = NA, times=length(dim(essentia.file)[1])),
#                          aggressive = rep(x = NA, times=length(dim(essentia.file)[1])),
#                          happy = rep(x = NA, times=length(dim(essentia.file)[1])),
#                          party = rep(x = NA, times=length(dim(essentia.file)[1])),
#                          relaxed = rep(x = NA, times=length(dim(essentia.file)[1])),
#                          sad = rep(x = NA, times=length(dim(essentia.file)[1])),
#                          acoustic = rep(x = NA, times=length(dim(essentia.file)[1])),
#                          electric = rep(x = NA, times=length(dim(essentia.file)[1])),
#                          instrumental = rep(x = NA, times=length(dim(essentia.file)[1])),
#                          timbreBright = rep(x = NA, times=length(dim(essentia.file)[1])))
# for (j in 1:dim(essentia.file)[1]){
#   artist = essentia.file$artist[j]
#   album = essentia.file$album[j]
#   track = essentia.file$track[j]
#   valence = mean(c(essentia.file$deam_valence[j], essentia.file$emo_valence[j],
#                    essentia.file$muse_valence[j]))
#   arousal = mean(c(essentia.file$deam_arousal[j], essentia.file$emo_arousal[j],
#                    essentia.file$muse_arousal[j]))
#   aggressive = mean(c(essentia.file$eff_aggressive[j],
#                       essentia.file$nn_aggressive[j]))
#   happy = mean(c(essentia.file$eff_happy[j],
#                  essentia.file$nn_happy[j]))
#   party = mean(c(essentia.file$eff_party[j],
#                  essentia.file$nn_party[j]))
#   relaxed = mean(c(essentia.file$eff_relax[j],
#                    essentia.file$nn_relax[j]))
#   sad = mean(c(essentia.file$eff_sad[j],
#                essentia.file$nn_sad[j]))
#   acoustic = mean(c(essentia.file$eff_acoustic[j],
#                     essentia.file$nn_acoustic[j]))
#   electric = mean(c(essentia.file$eff_electronic[j],
#                     essentia.file$nn_electronic[j]))
#   instrumental = mean(c(essentia.file$eff_instrumental[j],
#                         essentia.file$nn_instrumental[j]))
#   #timbreBright is just renaming the col eff_timbre_bright col
#   timbreBright = essentia.file$eff_timbre_bright[j]
#   essentiafile.data = c(artist, album, track,
#                         valence, arousal, aggressive, happy, party,
#                         relaxed, sad, acoustic, electric, instrumental, timbreBright)
#   
#   # How do i add a vector as a row to a dataframe?
#   df.essentia[j,] <-essentiafile.data
# }
# View(df.essentia)
# dim(df.essentia)

#Step 4
liwc.file = read.csv("LIWCOutput/LIWCOutput.csv")
some.data = merge(df.json, df.essentia)

#check dimensions of data, should be dim(df.json) + dim(df.essentia) - 3 cols
#duplicate cols for artist, track, album
#tried to merge in one step but doesnt work
dim(some.data)


all.data = merge(some.data, liwc.file)
#View(some.data)
#View(all.data)
dim(all.data)

names(all.data)[which(names(all.data) == "function.")] = "funct"
View(all.data)


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


