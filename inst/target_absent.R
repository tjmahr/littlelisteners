
library("littlelisteners")
library("dplyr")

ta_blocks <- list.files(
  path = "",
  pattern = "TargetAbsent.*gazedata",
  recursive = TRUE,
  full.names = TRUE)

ta_blocks <- ta_blocks[!stringr::str_detect(ta_blocks, "exclude|Exclude")]
ta_blocks

aois <- list(
  ImageL = create_aoi("ImageL", c(0, 600), c(180, 780), 1920, 1080),
  ImageR = create_aoi("ImageR", c(1320, 1920), c(180, 780), 1920, 1080)
)

try_read_gazedata <- failwith(NULL, read_gazedata)

# No duplicated block names
stopifnot(length(unique(basename(ta_blocks))) == length(ta_blocks))

all_gazes <- ta_blocks %>% lapply(try_read_gazedata)

gazes <- all_gazes %>% bind_rows()

gazes  <- gazes %>%
  add_aois(aois) %>%
  group_by(Basename, TrialNo) %>%
  interpolate_looks(window = 235, fps = 60, response_col = "GazeByAOI",
                    interp_col = "Interpolated",
                    fillable = c("ImageL", "ImageR"),
                    missing_looks = NA) %>%
  ungroup

summary(gazes$Interpolated)






library("rprime")
library("stringr")
prepare_eprime_ta <- function(path) {
  message("Loading", basename(path))
  target_onset_target_absent <- 1100
  stim <- read_eprime(path) %>%
    FrameList %>%
    keep_levels(3) %>%
    to_data_frame %>%
    as.tbl %>%
    readr::type_convert(.)
  names(stim) <- names(stim) %>%
    str_replace("[.]OnsetTime", "Onset") %>%
    str_replace("[.]OnsetDelay", "OnsetDelay") %>%
    str_replace("Sample", "TrialNo") %>%
    str_replace("Eprime.Basename", "Basename")

  subject <- str_extract(stim$Basename, "\\d{4}") %>% unique
  version <- str_extract(stim$Basename, "TargetAbsent(Block|Order)\\d") %>% unique
  order <- version %>% str_replace("Block", "Order")
  stim$Subject <- subject
  stim$Version <- version
  stim$Order <- order

  stim <- stim %>%
    select(Subject, Version, Order, Basename, TrialNo, StimType, AudioDur, AudioStim,
           ReinforcerDur, TargetImage = Target, Condition = StimType, ImageL, ImageR,
           ends_with("Onset"), ends_with("OnsetDelay")) %>%
    # TargetOnset 1230ms into audio
    mutate(TargetOnset = AudioStimOnset + target_onset_target_absent)
  stim
}

# keep going if there's an error
safe_prepare_eprime_ta <- failwith(NULL, prepare_eprime_ta)


ta_eprime <- list.files(
  path = "",
  pattern = "TargetAbsent.*txt",
  recursive = TRUE,
  full.names = TRUE)

ta_eprime <- ta_eprime[!stringr::str_detect(ta_eprime, "exclude|Exclude")]
all_eprime <- ta_eprime %>%
  lapply(safe_prepare_eprime_ta)



all_eprime_df <- all_eprime %>% bind_rows

unique(all_eprime_df$Basename)
unique(gazes$Basename)

looks_times <- gazes %>%
  left_join(all_eprime_df, by = c("Basename", "TrialNo")) %>%
  filter(!is.na(ImagesOnset)) %>%
  mutate(Time = Time - TargetOnset,
         Level = interaction(Basename, TrialNo))

target_l_trials <- looks_times %>% filter(TargetImage == "ImageL")
target_r_trials <- looks_times %>% filter(TargetImage == "ImageR")


target_l_trials <- target_l_trials %>%
  mutate(
    GazeByImageAOI = GazeByAOI,
    GazeByImageAOI = ifelse(GazeByAOI == "ImageL", "Target", GazeByImageAOI),
    GazeByImageAOI = ifelse(GazeByAOI == "ImageR", "Distractor", GazeByImageAOI))

target_r_trials <- target_r_trials %>%
  mutate(
    GazeByImageAOI = GazeByAOI,
    GazeByImageAOI = ifelse(GazeByAOI == "ImageR", "Target", GazeByImageAOI),
    GazeByImageAOI = ifelse(GazeByAOI == "ImageL", "Distractor", GazeByImageAOI))

target_l_trials %>% count(TargetImage, GazeByAOI, GazeByImageAOI)
target_r_trials %>% count(TargetImage, GazeByAOI, GazeByImageAOI)

looks <- bind_rows(target_l_trials, target_r_trials) %>%
  arrange(Basename, TrialNo, Time) %>%
  group_by(Basename, TrialNo) %>%
  adjust_times_around_zero(time_col = "Time", fps = 60, ties = "first")




aggregated <- looks %>% lookr::AggregateLooks(Basename + Condition + Time ~ GazeByImageAOI)

library("ggplot2")
ggplot(aggregated) +
  aes(x = Time, y = Proportion, color = Condition) +
  stat_summary()


