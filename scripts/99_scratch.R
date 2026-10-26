# 99_scratch.r 


# the final model we're trying to build 
fish_model <- glmmTMB(
  Count ~ feeding_guild * type * pair * date_s 
  + (1 | survey_id),
  ziformula = ~feeding_guild, 
  family = nbinom1, 
  data = abundance_data
)

