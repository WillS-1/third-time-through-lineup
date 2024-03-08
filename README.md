Examining the Third Time-Through-The-Order: A Statistical Approach<br />

<ins>**Description:**</ins> In baseball there have been numerous instances where a pitcher exhibits exceptional performance through the initial five innings. Then seemingly nowhere, the manager decides to remove that pitcher out from the game, right as the leadoff man was due up to face the starter for the third time. This analysis looks to explore this phenomenon and evaluate whether or not there is enough empirical evidence to support the theory: That when a pitcher faces a batter for the third time, his performance is prone to decline.<br />

<ins>**Step 1: Data Collection**</ins>
By utilizing the Baseball Savant API through the package *baseballr* in **R Studio**, I will be able to gather data containing all play-by-play information. The importance of having play-by-play information is so I can get a full picture of what is happening once a pitcher faces a lineup for the third time. Cumulative statistics like FIP or wOBA against will be utilized in this study, however, understanding why the pitcher ended up with a certain FIP or wOBA is paramount.<br />

By looking pitch by pitch, I will be able to understand the thought process of both the pitcher and hitter and be able to make a more informed decision on the theory.<br /> 

Below is the code I used to gather data from the Baseball Savant API.<br />

![image](https://github.com/WillS-1/third-time-through-lineup/assets/162635224/da48738e-94f0-4e09-8fcf-5f158c86c2ff)

 
The function *scape_statcast_savant* gathers every pitch from the entire league on any given day. This results in a lot of information being delivered to my machine and unfortunately results in some loading issues. That is why I broke up the data collection to 3-5 days over the course of the regular season to eliminate biases from one particular point in the season.<br />

Utilizing the *dplyr* package I am able to bind all the gathered data frames seamlessly into one large data frame.<br />

The first part of this study will be looking at all pitchers as a whole and then I will explore a handful of pitchers of varying skill level.<br /> 

<ins>**Step 2: Data Preprocessing**</ins>
The data that was returned to my new data frame **data** is the raw data collected by MLB Statcast. Now I have to go through the process of cleaning and manipulating some variables so I can perform my statistical analysis. Below is the code I used to clean and create/edit some columns which will be useful in my study.<br /> 

![image](https://github.com/WillS-1/third-time-through-lineup/assets/162635224/c223a274-769f-4e5d-9b02-c17033972cff)

The following are explanations of the changes and additions I made to the raw data frame data.<br />  
1. *new_names* is a vector that allows me to seamlessly rename the column names in the data frame. I do this so it follows an ordered pattern without the underscores and so I can remember what each column is called.
2.	*Pitch_Calls* is a vector that allows me to utilize the function recode from dplyr so I can rename all of the values in the description column. There are a couple instances where some values are named differently but for the purposes of this study can be classified as one value. For example, ‘swinging_strike_blocked’ is the same as ‘swinging_strike’ which is why I recoded them both to be ‘StrikeSwinging’. 
3.	*Pitch_Types* is also a vector that allows me to recode some of the pitch types in the file.
4.	*Play_Results* is the last vector that allows me to recode the descriptions of the play outcomes such as Single, Double, Triple or Homerun. 
    a.	I have broken up Pitch Calls and Play Results into two different columns so they are separated which will be helpful in seeing how a pitcher got to a certain play result.
5.	The last important addition to the data frame is the *TimeSeeing* column. This column tracks the number of times a pitcher has seen a batter in any given game. This isn’t included in the raw data, so I had to utilize the data.table package with the function rleid. I arranged the data frame, so it was in order from the start of the game to finish, then I grouped the data frame by Date, Pitcher, and Batter and on the ‘at_bat_number’ column I ran rleid to count the number of times a pitcher faced a batter in any one game.<br />

<ins>**Step 3: Statistical Analysis**</ins>
The first step in my stastical analysis was to get a feel for the data and run some preliminary calculations. I wanted to first confirm that I had enough data and with 1,371 games with over 27,000 individual plate appearances, I felt that was sufficient. Additionally, there were 189 total starters included in this study. <br />

My findings from the preliminary analysis were that the average starter threw 6 and 1/3 innings with 20 total plate appearances each game. Also, my initial hypothesis that starters in the MLB don’t normally go through the lineup a 3rd time seems to be holding true as the average starter faces the same batter 2.7 times per game.<br />

Below is a table displaying these statistics: 

![image](https://github.com/WillS-1/third-time-through-lineup/assets/162635224/36770ca8-1e8b-4042-b6d5-b5b652cd31b8)
 

Next, I analyzed some performance statistics in which I feel tell a good story of pitcher performance. All of the statistics I looked at here are averages from all the pitchers grouped together based on their time through the order.<br /> 

*Pitch/PA* : The number of pitches a pitcher threw per plate appearance.<br />
*Strike %* : The number of strikes a pitcher threw divided by the total number of pitches thrown.<br />
*Zone %* : The number of pitches a pitcher throws inside the strike zone divided by the total number of pitches thrown.<br />
*Chase %* : The number of pitches a swung at on pitches thrown outside the strike zone divided by the total number of pitches thrown outside the strike zone.<br />
*Strikeout %* : The number of strikeouts a pitcher had divided by the total number of plate appearances.<br />
*Walk %* : The number of walks a pitcher had divided by the total number of plate appearances.<br />
*xwOBA* : The average expected wOBA from a pitch thrown.<br />
*RA* : The average number of runs allowed<br />
*FIP* : The average FIP pitchers had which is a cumulative statistic comparable to ERA.<br />

Below is a table of my findings:

![image](https://github.com/WillS-1/third-time-through-lineup/assets/162635224/311ced7f-0c01-4274-a8ff-14c5c8302c51)

After going through the data, I realized that I should be comparing the 1st time a pitcher sees a particular batter to the 3rd time a pitcher sees the batter. The first time should be the benchmark where a batter should not have expectations of what will happen compared to his 3rd time seeing the pitcher.<br />  
What surprised me in these statistics was how close several of this descriptive statistics turned out to be. For example, Strike %, Zone %, and Chase % all were virtually the same from the 1st time through to the 3rd time through. Those statistics can be used as explanatory variables for the xwOBA and FIP statistics.<br />  
If you notice, the 1st time seeing a batter a pitcher’s xwOBA is on average .366, then the 3rd time through his xwOBA goes to on average .402. From first glance, that jump of nearly .040 seems like a pretty large gap which would support the initial hypothesis that pitchers shouldn’t face a batter more than twice.<br />  
I ran a t-test on the xwOBA when pitchers face a batter for the 1st time versus when they face one a 3rd time and to my expectations, I was able to reject the null hypothesis at a significance level of 5%. Since I failed to reject the null hypothesis that the two means were the same, based on my random sample of 2000 rows for each category, there is a statistically significant difference.<br /> 

![image](https://github.com/WillS-1/third-time-through-lineup/assets/162635224/a76add42-9420-4784-8a0c-384fe082834c)

 
Only looking at the xwOBA and FIP increases, I would have expected Strike % and Chase % to decrease as when those statistics are higher it tends to end up with favorable outcomes for the pitcher. Therefore, I will now start to look at what is causing that increase in xwOBA and determine if there are any relationships between those factors and the number of times a pitcher faces a particular batter.<br />  
I decided to look at hitters specifically in their lineup order. So, there are 9 different lineup positions, and as to be expected, throughout a game the 1-3 hitters tend to get more plate appearances. In the table below, I show the percentages as to how often a certain lineup position gets a plate appearance. Then I also show the expected wOBA of those lineup positions. Teams strategize their lineups, so their stronger hitters are in the beginning and their weaker hitters tail off at the end. In the table it can be inferred that the 2-5 hitters are typically the strongest.<br />  

![image](https://github.com/WillS-1/third-time-through-lineup/assets/162635224/b87be6e5-530c-4410-a226-2e0a457b513f)

Next I look at how well those hitters in the order of their lineup perform against starting pitchers facing them more than once. 

![image](https://github.com/WillS-1/third-time-through-lineup/assets/162635224/e3bfbfdb-1e44-4d70-ab53-943400684035)

Looking at these graphs, one can see that the 2-4 positions really continue to get better as the game goes on. Their expected wOBA steadily increases the more they see the starting pitcher. This brings me to my next point, where I am now starting to believe managers will remove a pitcher prior to their 3rd time facing the 2-4 hitters.<br />   
The expected wOBA as well as the other statistics I have mentioned before and in the table on the next page do show clear evidence that hitters performed better the 3rd time around.<br />  

![image](https://github.com/WillS-1/third-time-through-lineup/assets/162635224/2c354306-bf5a-4a9c-a955-b1594f3af9a2)

<ins>**Step 4: Specific Pitcher Analysis**</ins>
Now that I have evidence starters as whole tend to perform worse against hitters the more often they face them, I want to look at specific starters and see if this pattern persists. My initial thought is that the rule “Starters should not face a hitter more than twice in a game” should not be a general rule of thumb. I believe it can pertain to some pitchers, but others would perform just as well the 3rd time around as they did the 1st.<br />  
For this part, I have selected 6 pitchers who had the highest WAR amongst starters in 2023, as well as 6 pitchers who were in the middle of the pack in terms of WAR and 6 from the bottom of the pack.<br /> 

<ins>**Group A:**</ins><br /> 
Zack Wheeler: WAR – 5.9<br /> 
Spencer Strider: WAR – 5.5<br /> 
Kevin Gausman: WAR – 5.3<br /> 
Sonny Gray: WAR – 5.3<br /> 
Gerrit Cole: WAR – 5.2<br /> 
Zac Gallen: WAR – 5.2<br /> 
 
![image](https://github.com/WillS-1/third-time-through-lineup/assets/162635224/24ce7bdd-87c0-4c54-ac29-489e66574abe)

To my surprise, these 6 pitchers seemed to follow the pattern that removing a pitcher after they have faced the lineup twice is the right decision. These were the 6 best pitchers in MLB last season and all of them have worse Strikeout % and higher FIPs than when they faced a lineup the first time.<br />

<ins>**Group B:**</ins><br /> 
Merrill Kelly: WAR – 3.2<br />
Logan Gilbert: WAR – 3.2<br />
Miles Mikolas: WAR – 3.1<br />
Freddy Peralta: WAR – 3.0<br />
Jose Berrios: WAR – 3.0<br />
Sandy Alcantara: WAR – 2.9<br />

![image](https://github.com/WillS-1/third-time-through-lineup/assets/162635224/f0113408-1045-4aab-9d45-dd08eabbdb16)

This group of pitchers don’t follow a clear-cut pattern as we saw with Group A. For example, from these statistics, it would appear that Peralta actually improves the more he faces a batter. The interesting part of that is out of these 6 pitchers he gets pulled before he faces a lineup for the 3rd time the most. Still, he faced 140 batters at least 3 times over the 2023 season, but it would be interesting to see if this boils down to a team philosophy choice.<br />

While Merrill Kelly’s FIP slightly improved the 3rd time around, his xwOBA went from .332 to on average a .417. That’s a huge gap between performance which can explain why a manager may be inclined to remove him after he’s gone through the lineup twice.<br /> 

<ins>**Group C:**</ins><br /> 
Dean Kremer: WAR – 1.5<br /> 
Lucas Giolito: WAR – 1.0<br /> 
Patrick Corbin: WAR – 0.9<br /> 
JP Sears: WAR – 0.6<br /> 
Lance Lynn: WAR – 0.5<br /> 
Jordan Lyles: WAR – 0.2<br /> 

![image](https://github.com/WillS-1/third-time-through-lineup/assets/162635224/03a95c43-df11-453a-92fe-8949952f3d31)
 
Similar to Group B, these pitchers don’t follow a clear-cut pattern that they should be removed from the game prior to the 3rd time around. However, this is where game management comes into play because these pitchers nearly all have their best FIPs that are higher than Group A’s worst FIPs. Even though it would appear that these pitchers are improving as the game continues, they still pose a threat of allowing a run.<br /> 

<ins>**Step 5: Summary and Conclusion:**</ins><br />
This study sought out to examine whether managers should remove their starting pitcher before the third time through the lineup. Utilizing Baseball Savant, I was able to retrieve raw pitch by pitch data and examine players as a whole as well as by themselves.<br />

The main driver of ‘success’ I used was expected wOBA which has been used widely throughout baseball as a measure of success. By looking at xwOBA we are able to see what should have happened given a particular hit profile. I like to look at this rather than wOBA because defenses around the league are not made equal. Some pitchers have the luxury of having an all-star defense, while  others do not. Expected wOBA is meant to remove that variable from the equation as a whole. Several other statistics such as Strikeout%, Chase %, and FIP were used to measure a pitcher’s success.<br />

Overall, this was an interesting study, and my conclusion is that there is merit to pulling a pitcher prior to the 3rd time through the lineup. We saw the best pitchers in MLB struggle against hitters for their 3rd time. However, I do not believe this should be a rule of thumb for all pitchers. As I showed when I analyzed pitchers individually, some pitchers showed improvement as the game progressed. Freddy Peralta routinely gets pulled from the game prior to the 3rd time through. This may be due to his pitch count, however, the times he does face a batter for the 3rd time he showed improvement.<br />

For these reasons, I believe this should not be a strategy a team poses to all their pitchers but takes a deep look at their rotation and make that call on a case-by-case basis.
