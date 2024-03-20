library(baseballr, dplyr, grid, data.table, ggplot2)

#-----------------------------Step 1: Data collection using the baseballr package
data1 <- scrape_statcast_savant(start_date = '2023-04-01', end_date = '2023-04-07', player_type = 'pitcher')
data2 <- scrape_statcast_savant(start_date = '2023-05-01', end_date = '2023-05-04', player_type = 'pitcher')
data3 <- scrape_statcast_savant(start_date = '2023-06-01', end_date = '2023-06-04', player_type = 'pitcher')
data4 <- scrape_statcast_savant(start_date = '2023-08-01', end_date = '2023-08-04', player_type = 'pitcher')
data5 <- scrape_statcast_savant(start_date = '2023-08-10', end_date = '2023-08-14', player_type = 'pitcher')
data6 <- scrape_statcast_savant(start_date = '2023-05-10', end_date = '2023-05-14', player_type = 'pitcher')
data <- data1 %>% bind_rows(., data2,data3,data4,data5,data6)


#-----------------------------Step 2: Data Preprocessing:
new_names <- c('Date' = 'game_date','RelSpeed' = 'release_speed','RelSide' = 'release_pos_x', 'RelHeight' = 'release_pos_z', 'PitcherName' = 'player_name',
               'Zone' = 'zone','BatterSide' = 'stand', 'PitcherThrows' = 'p_throws','Balls' = 'balls', 'Strikes' = 'strikes','HorzBreak' = 'pfx_x',
               'InducedVertBreak' = 'pfx_z','PlateLocSide' = 'plate_x', 'PlateLocHeight' = 'plate_z', 'On3B' = 'on_3b', 'On2B' = 'on_2b', 'On1B' = 'on_1b',
               'Outs' = 'outs_when_up', 'Inning' = 'inning', 'Top.Bottom' = 'inning_topbot','ExitSpeed' = 'launch_speed','Angle' = 'launch_angle',
               'SpinRate' = 'release_spin_rate','Extension' = 'release_extension','PitchofPA' = 'pitch_number','TaggedPitchType' = 'pitch_name',
               'SpinAxis' = 'spin_axis')

Pitch_Calls <- c('swinging_strike' = 'StrikeSwinging', 'foul' = 'FoulBall', 'ball' = 'BallCalled', 'called_strike' = 'StrikeCalled',
  'hit_into_play' = 'InPlay', 'blocked_ball' = 'BallCalled', 'foul_tip' = 'FoulBall', 'swinging_strike_blocked' = 'StrikeSwinging',
  'hit_by_pitch' = 'HitByPitch', 'foul_bunt' = 'BuntFoul', 'missed_bunt' = 'BuntSwingingStrike', 'bunt_foul_tip' = 'BuntFoul')

Pitch_Types <- c('4-Seam Fastball' = 'Fastball', 'Split-Finger' = 'Splitter')

Play_Results <- c( 'Field_out' = 'Out', 'Grounded_into_double_play' = 'Out', 'Force_out' = 'Out','Home_run' = 'Homerun',
  'Hit_by_pitch' = 'HitByPitch', 'Strikeout_double_play' = 'Strikeout','Field_error' = 'Error', 'Sac_fly' = 'Sacrifice',
  'Sac_bunt' = 'Sacrifice', 'Fielders_choice' = 'FieldersChoice', 'Sac_fly_double_play' = 'Sacrifice',
  'Double_play' = 'Out', 'Fielders_choice_out' = 'FieldersChoice', 'Other_out' = 'Out')

DF <- data %>% rename(any_of(new_names)) %>% mutate(Count = paste0(Balls, '-', Strikes)) %>% 
  mutate(OutsOnPlay = case_when(events %in% c('strikeout', 'field_out', 'force_out', 'caught_stealing_home', 'sac_fly', 'caught_stealing_2b', 'sac_bunt', 'fielders_choice',
  'pickoff_1b', 'fielders_choice_out', 'other_out', 'pickoff_caught_stealing_2b', 'pickoff_3b', 'caught_stealing_3b')~1,
  events %in% c('double_play', 'sac_fly_double_play', 'strikeout_double_play', 'grounded_into_double_play')~2, T~0)) %>% 
  mutate(PitcherThrows = recode(PitcherThrows, 'L' = 'Left', 'R' = 'Right'), BatterSide = recode(BatterSide, 'L' = 'Left', 'R' = 'Right'),
  TaggedPitchType = recode(TaggedPitchType, !!!Pitch_Types),PitchCall = recode(description, !!!Pitch_Calls),PlayResult = str_to_title(events), 
  PlayResult = recode(PlayResult, !!!Play_Results)) %>% 
  filter(., !(TaggedPitchType %in% c('', 'Other', 'Screwball', 'Pitch Out')) & PitchCall != 'pitchout' & RelSpeed > 0) %>% 
  mutate(ScoreDiff = case_when(Top.Bottom == 'Top'~away_score-home_score, T~home_score-away_score)) %>% 
  mutate(Swing = case_when(PitchCall %in% c('StrikeSwinging', 'FoulBall', 'InPlay', 'BuntFoul', 'BuntSwingingStrike')~'Yes', T~'No')) %>% 
  mutate(ManOn1 = case_when(is.na(On1B) == TRUE~'No', T~'Yes'), ManOn2 = case_when(is.na(On2B) == TRUE~'No', T~'Yes'),
  ManOn3 = case_when(is.na(On3B) == TRUE~'No', T~'Yes')) %>% 
  mutate_at(vars(ManOn1:ManOn3), ~ifelse(. == 'No', 0, 1)) %>% 
  mutate(PCAT = case_when(TaggedPitchType %in% c('Sinker', 'Cutter', 'Fastball')~'FAST',
  TaggedPitchType %in% c('Changeup', 'Splitter', 'Eephus')~'OFF', T~"BREAK")) %>% 
  arrange(Date, Inning, at_bat_number, PitchofPA) %>% group_by(Date, PitcherName,batter) %>% 
  mutate(TimeSeeing = rleid(at_bat_number)) %>% ungroup() %>% 
  group_by(Date, PitcherName) %>% mutate(PitchNo = seq_len(n())) %>% ungroup() %>% 
  mutate(PitcherScore = case_when(Top.Bottom == 'Top'~home_score, T~away_score)) %>% 
  mutate(Runs = post_bat_score-bat_score) %>% 
  mutate(PostPAOuts = Outs+OutsOnPlay) %>% mutate(BatterTeam = case_when(Top.Bottom == 'Top'~away_team, T~home_team)) %>% 
  arrange(Date,home_team,Inning, at_bat_number, PitchofPA) %>% group_by(Date, BatterTeam) %>% 
  mutate(LineupPosition = cumsum(batter != lag(batter, default = first(batter)) | is.na(lag(batter))) %% 9) %>% 
  mutate(LineupPosition = ifelse(LineupPosition == 0,9,as.numeric(LineupPosition))) %>% ungroup()


#-----------------------------Step 3: Statistical Analysis

#---Section 3.1 (Total Games, Total PAs, LHP Starters, RHP Starters, Total Starters, AVG Starter IP, AVG Starter PAs, AVG Starter Lineup)
STARTERS_Distinct <- DF %>% filter(., at_bat_number == '1') %>% distinct(PitcherName)

DFS <- DF %>% filter(., PitcherName %in% c(STARTERS_Distinct))

Total_Games <- DFS %>% filter(., at_bat_number == '1') %>% count(at_bat_number) %>% select(Games = n)

Total_PAs <- DFS %>% filter(., PitchofPA == '1') %>% summarize(PAs = sum(PitchofPA, na.rm = T))

Starters_Count <- DFS %>% filter(., at_bat_number == '1') %>% distinct(PitcherName, PitcherThrows) %>% group_by(PitcherThrows) %>% 
  count(PitcherThrows) %>% ungroup() %>% add_row(., PitcherThrows = 'Total', n = sum(.$n)) %>% 
  reshape2::dcast(., .~PitcherThrows, value.var = 'n') %>% select(-1)

IP_Length <- DFS %>% filter(., PitcherName %in% c(STARTERS_Distinct$PitcherName)) %>% group_by(Date, PitcherName) %>% 
  filter(., PitchNo == max(PitchNo, na.rm = T)) %>% select(Date,PitcherName, Inning, PostPAOuts) %>% 
  mutate(TotalOuts = (Inning*3)+PostPAOuts) %>% ungroup() %>% summarize(TotalIP = round(mean(TotalOuts, na.rm = T)/3, 1)) %>% 
  separate(TotalIP, into = c('Inning', 'Outs'), sep = '[.]') %>% mutate(Outs = as.numeric(Outs), Outs = case_when(Outs > 1 & Outs < 4~'.1', Outs >= 4 & Outs < 9~'.2', T~'.0')) %>% 
  mutate(TotalIP = paste0(Inning, Outs)) %>% select(TotalIP)

PA_Length <- DFS %>% filter(., PitcherName %in% c(STARTERS_Distinct$PitcherName) & PitchofPA == '1') %>% group_by(Date, PitcherName) %>%
  summarize(PASA = sum(PitchofPA, na.rm = T)) %>% ungroup() %>% summarize(TotalPA = mean(PASA, na.rm = T))

Lineup_Length <- DFS %>% filter(., PitcherName %in% c(STARTERS_Distinct$PitcherName)) %>% group_by(Date, PitcherName) %>%
  filter(., PitchNo == max(PitchNo, na.rm = T)) %>% ungroup() %>% summarize(AVG_TimeSeeing = mean(TimeSeeing, na.rm = T))

#---Preliminary Statistics Table Creation

df1 <- Total_Games %>% bind_cols(., Total_PAs, Starters_Count, IP_Length, PA_Length, Lineup_Length) %>% 
  mutate_at(vars(TotalPA, AVG_TimeSeeing), ~sprintf('%.1f', .))
  
tt <- ttheme_default(core = list(bg_params=list(fill='#ffffff', col = 'black', lwd = 3.5),fg_params = list(fontsize = 34,  col = 'black', fontface = 1)), colhead = list(bg_params=list(fill='#d5d9e0', col='black', lwd = 3.5),fg_params = list(fontsize = 32)))
tt2 <- ttheme_default(core = list(bg_params=list(fill='#b6cff0', col = 'black', lwd = 3.5),fg_params = list(fontsize = 32,  col = 'white', fontface = 'bold')), colhead = list(bg_params=list(fill='#b6cff0', col='black', lwd = 3.5),fg_params = list(fontsize = 40, col = 'black')))
tab3 <- tableGrob(df1[1:dim(df1)[1], 1:dim(df1)[2]], rows = NULL, cols = c('Total Games', 'Total PAs', 'LHP Starters', 'RHP Starters', 'Total Starters',
                                                                           'AVG Starter IP', 'AVG Starter PAs', 'AVG Starter Lineup'), theme = tt)
header2 <- tableGrob(df1[1,c(1:1)], rows=NULL, cols=c('Preliminary Statistics'), theme = tt2) 
jn2 <- gtable_combine(header2[1,], tab3, along=2, join = 'outer')
jn2$widths <- rep(max(jn2$widths), length(jn2$widths)) # make column widths equal
jn2$layout[1:2 , c("l", "r")] <- list(c(1),c(dim(df1)[2]))
g <- jn2
g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd = 3)),t = 1, b = nrow(g), l = 1, r = dim(df1)[2])
g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd =1.5)),t = 1, b = 2, l = 1, r = dim(df1)[2])
g$widths <- unit(c(rep(1/ncol(g), ncol(g))), 'npc')
g$heights <- unit(c(.3, .3, .4),'npc')
TABLE1 <- g


#---Section 3.2 (Pitch/PA, Strike %, Zone %, Chase %, Strikeout %, Walk %, xwOBA, Runs Allowed, FIP)

Pitch_Per_PA <- DFS %>% group_by(Date, PitcherName, batter, at_bat_number) %>% filter(., PitchofPA == max(PitchofPA, na.rm = T)) %>% 
  group_by(TimeSeeing) %>% summarize(AVGPitchofPA = mean(PitchofPA, na.rm = T)) %>% mutate(AVGPitchofPA = sprintf('%.1f', AVGPitchofPA))

Runs_Allowed <- DFS %>% group_by(Date, PitcherName, TimeSeeing) %>% summarize(Runs = sum(Runs, na.rm = T)) %>% 
  group_by(TimeSeeing) %>% summarize(AVGRuns = sprintf('%.1f', mean(Runs, na.rm = T)))

xwOBA <- DFS %>% group_by(TimeSeeing) %>% summarize(AVGxwoba = sprintf('%.3f', mean(estimated_woba_using_speedangle, na.rm = T)))

Strike_Percentage <- DFS %>% mutate(PitchCall = factor(PitchCall, levels = c('StrikeSwinging', 'InPlay', 'FoulBall', 'StrikeCalled'))) %>% 
  group_by(TimeSeeing, PitchCall) %>% count(PitchCall, .drop = F) %>% 
  ungroup(PitchCall) %>% mutate(Total = sum(n,na.rm = T)) %>% filter(., PitchCall != 'NA') %>% 
  mutate(Strikes = sum(n,na.rm = T)) %>% mutate(StrikeP = paste0(sprintf('%.0f', (Strikes/Total)*100), "%")) %>% 
  distinct(TimeSeeing, StrikeP)

Zone_Percentage <- DFS %>% mutate(Zone = factor(Zone, levels = c('1', '2', '3', '4','5','6','7','8','9'))) %>% 
  group_by(TimeSeeing, Zone) %>% count(Zone, .drop = F) %>% 
  ungroup(Zone) %>% mutate(Total = sum(n,na.rm = T)) %>% filter(., Zone != 'NA') %>% 
  mutate(Strikes = sum(n,na.rm = T)) %>% mutate(ZoneP = paste0(sprintf('%.0f', (Strikes/Total)*100), "%")) %>% 
  distinct(TimeSeeing, ZoneP)

Strikeouts <- DFS %>% mutate(PlayResult = factor(PlayResult, levels = c('Strikeout'))) %>% 
  group_by(TimeSeeing, PlayResult) %>% count(PlayResult, .drop = F) %>% 
  filter(., PlayResult == 'Strikeout')

Strikeout_Percentage <- DFS %>% filter(., PitchofPA == '1') %>% group_by(TimeSeeing) %>% summarize(PAS = sum(PitchofPA, na.rm = T)) %>% 
  left_join(Strikeouts, ., by = c('TimeSeeing')) %>% mutate(StrikeoutP = paste0(sprintf('%.0f', (n/PAS)*100), '%')) %>% 
  ungroup() %>% distinct(TimeSeeing, StrikeoutP)

Walks <- DFS %>% mutate(PlayResult = factor(PlayResult, levels = c('Walk'))) %>% 
  group_by(TimeSeeing, PlayResult) %>% count(PlayResult, .drop = F) %>% 
  filter(., PlayResult == 'Walk')

Walk_Percentage <- DFS %>% filter(., PitchofPA == '1') %>% group_by(TimeSeeing) %>% summarize(PAS = sum(PitchofPA, na.rm = T)) %>% 
  left_join(Walks, ., by = c('TimeSeeing')) %>% mutate(WalkP = paste0(sprintf('%.0f', (n/PAS)*100), '%')) %>% 
  ungroup() %>% distinct(TimeSeeing, WalkP)

Play_Results <- DFS %>% mutate(PitcherName = factor(PitcherName, levels = c(STARTERS_Distinct$PitcherName))) %>% 
  mutate(TimeSeeing = factor(TimeSeeing, levels = c('1','2','3','4'))) %>% 
  mutate(PlayResult = factor(PlayResult, levels = c('Homerun', 'Strikeout', 'Walk', 'HitByPitch'))) %>% 
  group_by(PitcherName, TimeSeeing, PlayResult) %>% count(PlayResult, .drop = F) %>% filter(., PlayResult != 'NA') %>% 
  reshape2::dcast(., TimeSeeing+PitcherName~PlayResult, value.var = 'n')

FIP <- DFS %>% group_by(Date, PitcherName, TimeSeeing) %>% filter(., PitchNo == max(PitchNo)) %>% 
  select(Date, PitcherName, TimeSeeing, Inning, PostPAOuts, PitchNo, PlayResult) %>% mutate(Outs2 = PostPAOuts) %>% 
   mutate(TimeSeeing = factor(TimeSeeing, levels = c('1','2', '3', '4'))) %>% mutate(PitcherName = factor(PitcherName, levels = c(STARTERS_Distinct$PitcherName))) %>% 
  group_by(PitcherName, TimeSeeing, .drop = F) %>% 
  summarize(InningSum = sum(Inning, na.rm = T), Outs2Summ = sum(Outs2)) %>% 
  mutate(IP = ((InningSum*3)+(Outs2Summ))/3) %>% select(PitcherName, TimeSeeing, IP) %>% 
  left_join(Play_Results, ., by = c('PitcherName', 'TimeSeeing')) %>% ungroup() %>% 
  group_by(TimeSeeing) %>% summarize(Homerun = sum(Homerun), Walk = sum(Walk), HitByPitch = sum(HitByPitch), Strikeout = sum(Strikeout), IP = sum(IP)) %>% 
  mutate(FIP=(((13*Homerun)+3*(Walk+HitByPitch)-(2*Strikeout))/IP)+3.255) %>% distinct(TimeSeeing, FIP)

Chase_Percentage <- DFS %>% filter(., Zone %in% c('11','12','13','14')) %>% 
  mutate(PitchCall = factor(PitchCall, levels = c('StrikeSwinging', 'InPlay', 'FoulBall'))) %>% 
  mutate(TimeSeeing = factor(TimeSeeing, levels = c('1','2','3','4'))) %>% group_by(TimeSeeing, PitchCall) %>% 
  count(PitchCall, .drop = F) %>% ungroup(PitchCall) %>% mutate(Total = sum(n,na.rm = T)) %>% filter(., PitchCall != 'NA') %>% 
  mutate(Swings = sum(n,na.rm = T)) %>% mutate(ChaseP = paste0(sprintf('%.0f', (Swings/Total)*100), "%")) %>% 
  distinct(TimeSeeing, ChaseP)

#---Performance Statistics Table Creation

df1 <- Reduce(function(x,y) merge(x,y, by.all = c('TimeSeeing')), list(Pitch_Per_PA, Runs_Allowed,xwOBA, Strike_Percentage,
                                                                       Zone_Percentage, Strikeout_Percentage,Walk_Percentage, FIP, Chase_Percentage)) %>% 
  mutate(FIP = sprintf('%.2f', (FIP))) %>% select(TimeSeeing, AVGPitchofPA, StrikeP, ZoneP, ChaseP, StrikeoutP, WalkP, AVGxwoba, AVGRuns, FIP)

tt <- ttheme_default(core = list(bg_params=list(fill='#ffffff', col = 'black', lwd = 3.5),fg_params = list(fontsize = 34,  col = 'black', fontface = 1)), colhead = list(bg_params=list(fill='#d5d9e0', col='black', lwd = 3.5),fg_params = list(fontsize = 32)))
tt2 <- ttheme_default(core = list(bg_params=list(fill='#b6cff0', col = 'black', lwd = 3.5),fg_params = list(fontsize = 32,  col = 'white', fontface = 'bold')), colhead = list(bg_params=list(fill='#b6cff0', col='black', lwd = 3.5),fg_params = list(fontsize = 40, col = 'black')))
tab3 <- tableGrob(df1[1:dim(df1)[1], 1:dim(df1)[2]], rows = NULL, cols = c('Time Seeing', 'Pitch/PA', 'Strike %', 'Zone %', 'Chase %',
                                                                           'Strikeout %', 'Walk %', 'xwOBA', 'RA', 'FIP'), theme = tt)
header2 <- tableGrob(df1[1,c(1:1)], rows=NULL, cols=c('Performance Statistics'), theme = tt2) 
jn2 <- gtable_combine(header2[1,], tab3, along=2, join = 'outer')
jn2$widths <- rep(max(jn2$widths), length(jn2$widths)) # make column widths equal
jn2$layout[1:2 , c("l", "r")] <- list(c(1),c(dim(df1)[2]))
g <- jn2
g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd = 3)),t = 1, b = nrow(g), l = 1, r = dim(df1)[2])
g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd =1.5)),t = 1, b = 2, l = 1, r = dim(df1)[2])
g$widths <- unit(c(rep(1/ncol(g), ncol(g))), 'npc')
g$heights <- unit(c(rep(1/nrow(g), nrow(g))), 'npc')
TABLE2 <- g

#---Section 3.3 T-Test

FirstTimeSeeing <- DFS %>% filter(., TimeSeeing == '1' & is.na(estimated_woba_using_speedangle) == FALSE)
ThirdTimeSeeing <- DFS %>% filter(., TimeSeeing == '3' & is.na(estimated_woba_using_speedangle) == FALSE)

sample_FirstTime <- FirstTimeSeeing %>% sample_n(2000, replace = FALSE)
sample_ThirdTime <- ThirdTimeSeeing %>% sample_n(2000, replace = FALSE)

xwOBA_t_test <- t.test(sample_FirstTime$estimated_woba_using_speedangle, sample_ThirdTime$estimated_woba_using_speedangle, paired = TRUE)

print(xwOBA_t_test)

#---Section 3.4 (Seen %, xwOBA based on Lineup Position)

Seen <- DFS %>% group_by(LineupPosition) %>% summarize(xwOBA = mean(estimated_woba_using_speedangle, na.rm = T))
df1 <- DFS %>% filter(., PitchofPA == '1') %>% group_by(LineupPosition) %>% count(LineupPosition) %>% ungroup() %>% 
  mutate(Total = sum(n,na.rm = T)) %>% mutate(Seen = paste0(sprintf('%.0f', (n/Total)*100), '%')) %>% select(LineupPosition, Seen) %>% 
  left_join(., Seen, by = c('LineupPosition')) %>% mutate(xwOBA = sprintf('%.3f', xwOBA))

#---Lineup Position Table Creation

tt <- ttheme_default(core = list(bg_params=list(fill='#ffffff', col = 'black', lwd = 3.5),fg_params = list(fontsize = 34,  col = 'black', fontface = 1)), colhead = list(bg_params=list(fill='#d5d9e0', col='black', lwd = 3.5),fg_params = list(fontsize = 32)))
tt2 <- ttheme_default(core = list(bg_params=list(fill='#b6cff0', col = 'black', lwd = 3.5),fg_params = list(fontsize = 32,  col = 'white', fontface = 'bold')), colhead = list(bg_params=list(fill='#b6cff0', col='black', lwd = 3.5),fg_params = list(fontsize = 40, col = 'black')))
tab3 <- tableGrob(df1[1:dim(df1)[1], 1:dim(df1)[2]], rows = NULL, cols = c('Lineup', 'Seen', 'xwOBA'), theme = tt)
header2 <- tableGrob(df1[1,c(1:1)], rows=NULL, cols=c('Lineup Position'), theme = tt2) 
jn2 <- gtable_combine(header2[1,], tab3, along=2, join = 'outer')
jn2$widths <- rep(max(jn2$widths), length(jn2$widths)) # make column widths equal
jn2$layout[1:2 , c("l", "r")] <- list(c(1),c(dim(df1)[2]))
g <- jn2
g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd = 3)),t = 1, b = nrow(g), l = 1, r = dim(df1)[2])
g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd =1.5)),t = 1, b = 2, l = 1, r = dim(df1)[2])
g$widths <- unit(c(rep(1/ncol(g), ncol(g))), 'npc')
g$heights <- unit(c(rep(1/nrow(g), nrow(g))), 'npc')
TABLE3 <- g

#---Section 3.5 (Lineup Position Time Seeing Graph & xwOBA, wOBA, BABIP, ISO Table)

Lineup_xwOBA <- DFS %>% 
  mutate(LineupPosition = factor(LineupPosition, levels = c('1','2','3','4','5','6','7','8','9'))) %>% 
  mutate(TimeSeeing = factor(TimeSeeing, levels = c('1','2','3','4'))) %>% group_by(LineupPosition, TimeSeeing, .drop = F) %>% 
  summarize(xwOBA = mean(estimated_woba_using_speedangle, na.rm = T),
  woba_value = mean(woba_value, na.rm = T), babip_value = mean(babip_value, na.rm = T),
  iso_value = mean(iso_value, na.rm = T)) %>% ungroup() %>% 
  mutate(LineupPosition = paste0('Lineup Position: ', LineupPosition))

G1 <- Lineup_xwOBA %>% filter(., is.nan(xwOBA) == FALSE) %>% ggplot(aes(x = TimeSeeing, y = xwOBA))+
  geom_bar(stat = 'identity', width = 0.5, fill = '#b6cff0', color = 'black')+
  facet_wrap(vars(LineupPosition), nrow = 3, ncol = 3)+
  coord_cartesian(ylim = c(.25,.5))+
  scale_y_continuous(labels = function(x) str_remove(sprintf('%.3f', x), '^0+'))+
  xlab('\nTime Seeing Starter')+
  ylab('xwOBA\n')+
  theme_bw()+
  theme(plot.title = element_text(size = 36, hjust = 0.5, face = 'bold'),
        axis.text = element_text(size = 30), axis.title = element_text(size = 32),
        strip.background = element_rect(fill = '#b6cff0'),
        strip.text = element_text(size = 28),
        panel.spacing = unit(2, 'lines'))+
  ggtitle('Lineup Position xwOBA\n')


df2 <- Lineup_xwOBA %>% mutate(LineupPosition = gsub('Lineup Position: ', '', LineupPosition)) %>% distinct(LineupPosition)
df1 <- Lineup_xwOBA %>% mutate(LineupPosition = gsub('Lineup Position: ', '', LineupPosition)) %>% 
    mutate_at(vars(xwOBA:iso_value), ~str_remove(sprintf('%.3f', .), '^0+')) %>% 
    mutate_all(., ~case_when(. %in% c('NaN')~'', T~as.character(.)))

#---Lineup Position Statistics Table Creation
  
colours <- df1 %>% mutate_at(vars(TimeSeeing:iso_value), ~case_when(LineupPosition %in% c('2','4','6','8')~'#dde5f0', T~'#ffffff')) %>% 
    mutate(LineupPosition = TimeSeeing) %>% as.matrix()
pcol <- df2 %>% mutate(Colour = case_when(LineupPosition %in% c('2','4','6','8')~'#dde5f0', T~'#ffffff')) %>% 
    select(-LineupPosition) %>% as.matrix()
  
HVEC1 <- data.frame(x = seq(1,dim(df1)[1])+1)
HVEC1$TF <- ifelse(HVEC1$x%%4 == 1, 'FALSE', 'TRUE')
HVEC1S <- subset(HVEC1, TF == 'FALSE')
HVEC1S <- rbind(HVEC1S, HVEC1S)
FX <- HVEC1S$x
FY <- FX-3
tt <- ttheme_default(core = list(bg_params=list(fill=colours, col = 'black', lwd = 3.5),fg_params = list(fontsize = 36,  col = 'black', fontface = 1)), colhead = list(bg_params=list(fill='#d5d9e0', col='black', lwd = 3.5),fg_params = list(fontsize = 36)))
tt2 <- ttheme_default(core = list(bg_params=list(fill='gray45', col = 'black', lwd = 3.5),fg_params = list(fontsize = 32,  col = 'white', fontface = 'bold')), colhead = list(bg_params=list(fill='#b6cff0', col='black', lwd = 3.5),fg_params = list(fontsize = 40, col = 'black')))
tt3 <- ttheme_default(core = list(bg_params=list(fill=pcol, col = 'black', lwd = 3.5),fg_params = list(fontsize = 34,  col = 'black', fontface = 'bold')), colhead = list(bg_params=list(fill='#d5d9e0', col='black', lwd = 3.5),fg_params = list(fontsize = 36)))
tab2 <- tableGrob(df2[1:dim(df2)[1], 1:1], rows = NULL,cols = c('Lineup'), theme = tt3)
tab3 <- tableGrob(df1[1:dim(df1)[1], 1:6], rows = NULL, cols = c('no', 'Time', 'xwOBA', 'wOBA', 'BABIP', 'ISO'), theme = tt)
halign_alt <- gtable_combine(tab2,tab3, along =1)
halign_alt$layout[halign_alt$layout$t != 1 & halign_alt$layout$l == 1, c("t")] <- c(FX)
halign_alt$layout[halign_alt$layout$b != 1  & halign_alt$layout$l == 1, c("b")] <- c(FY)
header2 <- tableGrob(df1[1,1:1], rows=NULL, cols=c('Lineup Position Statistics'), theme = tt2) 
jn2 <- gtable_combine(header2[1,], halign_alt, along=2, join = 'outer')
jn2$widths <- rep(max(jn2$widths), length(jn2$widths)) # make column widths equal
jn2$layout[1:2 , c("l", "r")] <- list(c(1),c(6))
g <- jn2
g <- gtable_add_grob(g, grobs = rectGrob(gp = gpar(fill = NA, lwd = 7)),t = 1, b = nrow(g), l = 1, r = 6)
g <- gtable_add_grob(g, grobs = rectGrob(gp = gpar(fill = NA, lwd =1.5)),t = 1, b = 2, l = 1, r = 6)
g$widths <- unit(c(.15, rep(.85/5, 5)), 'npc')
g$heights <- unit(rep(1/nrow(g), nrow(g)), 'npc')
TABLE4 <- g

#-----------------------------Step 4: Pitcher Specific Analysis

target_1 <- c('Wheeler, Zack', 'Strider, Spencer', 'Gausman, Kevin', 'Gray, Sonny', 'Cole, Gerrit', 'Gallen, Zac')
target_2 <- c('Kelly, Merrill', 'Gilbert, Logan', 'Mikolas, Miles', 'Peralta, Freddy', 'Berríos, José', 'Alcantara, Sandy')
target_3 <- c('Kremer, Dean', 'Giolito, Lucas', 'Corbin, Patrick', 'Sears, JP', 'Lynn, Lance', 'Lyles, Jordan')

coding_1_1 <- c('Strider, Spencer', 'Gray, Sonny', 'Gallen, Zac')
coding_2_1 <- c('Gilbert, Logan', 'Peralta, Freddy', 'Alcantara, Sandy')
coding_3_1 <- c('Giolito, Lucas', 'Sears, JP', 'Lyles, Jordan')

coding_1_2 <- c('Spencer Strider', 'Sonny Gray', 'Zac Gallen')
coding_2_2 <- c('Logan Gilbert', 'Freddy Peralta', 'Sandy Alcantara')
coding_3_2 <- c('Lucas Giolito', 'JP Sears', 'Jordan Lyles')

Pitcher_Table <- function(DF, target,Pitcher_Coding_1,Pitcher_Coding_2,Title){
  
A1 <- DFS %>% group_by(Date, PitcherName, batter, at_bat_number) %>% filter(., PitchofPA == max(PitchofPA, na.rm = T)) %>% 
  group_by(TimeSeeing, PitcherName) %>% summarize(AVGPitchofPA = mean(PitchofPA, na.rm = T)) %>% mutate(AVGPitchofPA = sprintf('%.1f', AVGPitchofPA))

A2 <- DFS %>% group_by(Date, PitcherName, TimeSeeing) %>% summarize(Runs = sum(Runs, na.rm = T)) %>% 
  group_by(TimeSeeing, PitcherName) %>% summarize(AVGRuns = sprintf('%.1f', mean(Runs, na.rm = T)))

A3 <- DFS %>% group_by(TimeSeeing, PitcherName) %>% summarize(AVGxwoba = sprintf('%.3f', mean(estimated_woba_using_speedangle, na.rm = T)))

A4 <- DFS %>% mutate(PitchCall = factor(PitchCall, levels = c('StrikeSwinging', 'InPlay', 'FoulBall', 'StrikeCalled'))) %>% 
  group_by(TimeSeeing, PitchCall, PitcherName) %>% count(PitchCall, .drop = F) %>% 
  ungroup(PitchCall) %>% mutate(Total = sum(n,na.rm = T)) %>% filter(., PitchCall != 'NA') %>% 
  mutate(Strikes = sum(n,na.rm = T)) %>% mutate(StrikeP = paste0(sprintf('%.0f', (Strikes/Total)*100), "%")) %>% 
  distinct(TimeSeeing, PitcherName, StrikeP)

A5 <- DFS %>% mutate(Zone = factor(Zone, levels = c('1', '2', '3', '4','5','6','7','8','9'))) %>% 
  group_by(TimeSeeing, Zone, PitcherName) %>% count(Zone, .drop = F) %>% 
  ungroup(Zone) %>% mutate(Total = sum(n,na.rm = T)) %>% filter(., Zone != 'NA') %>% 
  mutate(Strikes = sum(n,na.rm = T)) %>% mutate(ZoneP = paste0(sprintf('%.0f', (Strikes/Total)*100), "%")) %>% 
  distinct(TimeSeeing, PitcherName, ZoneP)

A6 <- DFS %>% mutate(PlayResult = factor(PlayResult, levels = c('Strikeout'))) %>% 
  group_by(TimeSeeing, PlayResult, PitcherName) %>% count(PlayResult, .drop = F) %>% 
  filter(., PlayResult == 'Strikeout')

A6_1 <- DFS %>% filter(., PitchofPA == '1') %>% group_by(TimeSeeing, PitcherName) %>% summarize(PAS = sum(PitchofPA, na.rm = T)) %>% 
  left_join(A6, ., by = c('TimeSeeing', 'PitcherName')) %>% mutate(StrikeoutP = paste0(sprintf('%.0f', (n/PAS)*100), '%')) %>% 
  ungroup() %>% distinct(TimeSeeing, PitcherName, StrikeoutP)

A7 <- DFS %>% mutate(PlayResult = factor(PlayResult, levels = c('Walk'))) %>% 
  group_by(TimeSeeing, PlayResult, PitcherName) %>% count(PlayResult, .drop = F) %>% 
  filter(., PlayResult == 'Walk')

A7_1 <- DFS %>% filter(., PitchofPA == '1') %>% group_by(TimeSeeing, PitcherName) %>% summarize(PAS = sum(PitchofPA, na.rm = T)) %>% 
  left_join(A7, ., by = c('TimeSeeing', 'PitcherName')) %>% mutate(WalkP = paste0(sprintf('%.0f', (n/PAS)*100), '%')) %>% 
  ungroup() %>% distinct(TimeSeeing, PitcherName, WalkP)


A8 <- DFS %>% mutate(PitcherName = factor(PitcherName, levels = c(target))) %>% 
  mutate(TimeSeeing = factor(TimeSeeing, levels = c('1','2','3'))) %>% 
  mutate(PlayResult = factor(PlayResult, levels = c('Homerun', 'Strikeout', 'Walk', 'HitByPitch'))) %>% 
  group_by(PitcherName, TimeSeeing, PlayResult) %>% count(PlayResult, .drop = F) %>% filter(., PlayResult != 'NA') %>% 
  reshape2::dcast(., TimeSeeing+PitcherName~PlayResult, value.var = 'n')

A8_1 <- DFS %>% group_by(Date, PitcherName, TimeSeeing) %>% filter(., PitchNo == max(PitchNo)) %>% 
  select(Date, PitcherName, TimeSeeing, Inning, PostPAOuts, PitchNo, PlayResult) %>% mutate(Outs2 = PostPAOuts) %>% 
   mutate(TimeSeeing = factor(TimeSeeing, levels = c('1','2', '3'))) %>% mutate(PitcherName = factor(PitcherName, levels = c(target))) %>% 
  group_by(PitcherName, TimeSeeing, .drop = F) %>% 
  summarize(InningSum = sum(Inning, na.rm = T), Outs2Summ = sum(Outs2)) %>% 
  mutate(IP = ((InningSum*3)+(Outs2Summ))/3) %>% select(PitcherName, TimeSeeing, IP) %>% 
  left_join(A8, ., by = c('PitcherName', 'TimeSeeing')) %>% ungroup() %>% 
  group_by(TimeSeeing, PitcherName) %>% summarize(Homerun = sum(Homerun), Walk = sum(Walk), HitByPitch = sum(HitByPitch), Strikeout = sum(Strikeout), IP = sum(IP)) %>% 
  mutate(FIP=(((13*Homerun)+3*(Walk+HitByPitch)-(2*Strikeout))/IP)+3.255) %>% distinct(TimeSeeing, PitcherName, FIP)

A9 <- DFS %>% filter(., Zone %in% c('11','12','13','14')) %>% 
  mutate(PitchCall = factor(PitchCall, levels = c('StrikeSwinging', 'InPlay', 'FoulBall'))) %>% 
  mutate(TimeSeeing = factor(TimeSeeing, levels = c('1','2','3'))) %>% group_by(TimeSeeing, PitchCall, PitcherName) %>% 
  count(PitchCall, .drop = F) %>% ungroup(PitchCall) %>% mutate(Total = sum(n,na.rm = T)) %>% filter(., PitchCall != 'NA') %>% 
  mutate(Swings = sum(n,na.rm = T)) %>% mutate(ChaseP = paste0(sprintf('%.0f', (Swings/Total)*100), "%")) %>% 
  distinct(TimeSeeing, PitcherName, ChaseP)

df1 <- Reduce(function(x,y) merge(x,y, by.all = c('TimeSeeing', 'PitcherName')), list(A1, A2,A3, A4, A5, A6_1,A7_1, A8_1, A9)) %>% 
  mutate(FIP = sprintf('%.2f', (FIP))) %>% select(PitcherName, TimeSeeing, AVGPitchofPA, StrikeP, ZoneP, ChaseP, StrikeoutP, WalkP, AVGxwoba, AVGRuns, FIP) %>% 
  arrange(match(PitcherName, c(target)))
 
df2 <- df1 %>% distinct(PitcherName) %>% separate(., PitcherName, into = c('Last', 'First'), sep = ', ') %>% 
  mutate(Pitcher = paste0(First, ' ', Last)) %>% select(Pitcher)

colours <- df1 %>% mutate_at(vars(TimeSeeing:FIP), ~case_when(PitcherName %in% c(Pitcher_Coding_1)~'#dde5f0', T~'#ffffff')) %>% 
    mutate(PitcherName = TimeSeeing) %>% as.matrix()

pcol <- df2 %>% mutate(Colour = case_when(Pitcher %in% c(Pitcher_Coding_2)~'#dde5f0', T~'#ffffff')) %>% 
    select(-Pitcher) %>% as.matrix()
  
  HVEC1 <- data.frame(x = seq(1,dim(df1)[1])+1)
  HVEC1$TF <- ifelse(HVEC1$x%%3 == 1, 'FALSE', 'TRUE')
  HVEC1S <- subset(HVEC1, TF == 'FALSE')
  HVEC1S <- rbind(HVEC1S, HVEC1S)
  FX <- HVEC1S$x
  FY <- FX-2
  tt <- ttheme_default(core = list(bg_params=list(fill=colours, col = 'black', lwd = 3.5),fg_params = list(fontsize = 36,  col = 'black', fontface = 1)), colhead = list(bg_params=list(fill='#d5d9e0', col='black', lwd = 3.5),fg_params = list(fontsize = 36)))
  tt2 <- ttheme_default(core = list(bg_params=list(fill='gray45', col = 'black', lwd = 3.5),fg_params = list(fontsize = 32,  col = 'white', fontface = 'bold')), colhead = list(bg_params=list(fill='#b6cff0', col='black', lwd = 3.5),fg_params = list(fontsize = 40, col = 'black')))
  tt3 <- ttheme_default(core = list(bg_params=list(fill=pcol, col = 'black', lwd = 3.5),fg_params = list(fontsize = 34,  col = 'black', fontface = 'bold')), colhead = list(bg_params=list(fill='#d5d9e0', col='black', lwd = 3.5),fg_params = list(fontsize = 36)))
  tab2 <- tableGrob(df2[1:dim(df2)[1], 1:1], rows = NULL,cols = c('Pitcher'), theme = tt3)
  tab3 <- tableGrob(df1[1:dim(df1)[1], 1:11], rows = NULL, cols = c('no', 'Time Seeing', 'Pitch/PA', 'Strike %', 'Zone %', 'Chase %',
                                                                          'Strikeout %', 'Walk %', 'xwOBA', 'RA', 'FIP'), theme = tt)
  halign_alt <- gtable_combine(tab2,tab3, along =1)
  halign_alt$layout[halign_alt$layout$t != 1 & halign_alt$layout$l == 1, c("t")] <- c(FX)
  halign_alt$layout[halign_alt$layout$b != 1  & halign_alt$layout$l == 1, c("b")] <- c(FY)
  header2 <- tableGrob(df1[1,1:1], rows=NULL, cols=c(TITLE), theme = tt2) 
  jn2 <- gtable_combine(header2[1,], halign_alt, along=2, join = 'outer')
  jn2$widths <- rep(max(jn2$widths), length(jn2$widths)) # make column widths equal
  jn2$layout[1:2 , c("l", "r")] <- list(c(1),c(11))
  g <- jn2
  g <- gtable_add_grob(g, grobs = rectGrob(gp = gpar(fill = NA, lwd = 7)),t = 1, b = nrow(g), l = 1, r = 11)
  g <- gtable_add_grob(g, grobs = rectGrob(gp = gpar(fill = NA, lwd =1.5)),t = 1, b = 2, l = 1, r = 11)
  g$widths <- unit(c(.15, rep(.85/10, 10)), 'npc')
  g$heights <- unit(rep(1/nrow(g), nrow(g)), 'npc')
  TABLE5 <- g

  
  
}

DF1 <- read.csv('group_a_pitchers.csv')
DF2 <- read.csv('group_b_pitchers.csv')
DF3 <- read.csv('group_c_pitchers.csv')

TABLE5 <- Pitcher_Table(DF1, target_1, coding_1_1, coding_1_2, 'Group A Pitcher Statistics')
TABLE6 <- Pitcher_Table(DF2, target_2, coding_2_1, coding_2_2, 'Group B Pitcher Statistics')
TABLE7 <- Pitcher_Table(DF3, target_3, coding_3_1, coding_3_2, 'Group C Pitcher Statistics')

