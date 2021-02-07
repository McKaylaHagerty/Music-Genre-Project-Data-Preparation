
Homework 7

McKayla Hagerty

CS 625, Fall 2020

## EDA Process

### First dataset: Importing and Initial exploration

The first dataset I found is a list of the Top 100 songs for each week
from 8/2/1958 through the week of 12/28/2019.

Original Source:
<https://data.world/kcmillersean/billboard-hot-100-1958-2017> (the
orginial dataset was scraped using import.io)

``` r
#Import Billboard data.
billboard <- read.csv("https://query.data.world/s/j53mutis3fjnygz4fjctj576eljh32", header=TRUE, stringsAsFactors=FALSE)
billboard$WeekID <- as.Date(billboard$WeekID , format = "%m/%d/%Y")
View(billboard)
```

Billboard Data Columns:

  - Billboard Chart URL
  - WeekID
  - Song name
  - Performer name
  - SongID - Concatenation of song & performer
  - Current week on chart
  - Instance (this is used to separate breaks on the chart for a given
    song. Example, an - instance of 6 tells you that this is the sixth
    time this song has appeared on the chart)
  - Previous week position
  - Peak Position (as of the corresponding week)
  - Weeks on Chart (as of the corresponding week)

<!-- end list -->

``` r
names(billboard)
```

    ##  [1] "ï..url"                 "WeekID"                 "Week.Position"         
    ##  [4] "Song"                   "Performer"              "SongID"                
    ##  [7] "Instance"               "Previous.Week.Position" "Peak.Position"         
    ## [10] "Weeks.on.Chart"

``` r
#View(billboard)
```

From my first look at the data, I see a combined column called SongID
which is a combination of the song name and performer name. I was
curious about a potential ability to combine datasets and was excited to
find this type of Song ID is widely used.

Another initial finding is that weeks on chart values have a wide range,
between 1 and 87.

This leads me to my first question: *What can be considered a hit song?*

We know all the songs in our dataset were popular because they made the
top 100 songs. However, some songs may have made the charts only to be
pushed out and never return. For example, I Miss You Baby by Millie
Jackson made the charts for 4 nonconsecutive weeks with a peak position
of \#95.

I’ll take a look at a scatterplot of peak poition on the charts and
weeks on the charts.

``` r
library(ggplot2)
ggplot(billboard, aes(x=Weeks.on.Chart, y=Peak.Position)) + geom_point()
```

![](report_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

I realized two things about this chart: the peak position axis should be
reversed because an artist wants to be at the top of the charts (1 or
close to 1) and that each of these dots represents a week and position
combination when I want to be looking at song success.

I’ll first determine the number of unique songs on the billboard list to
use as a check for the next step.

``` r
length(unique(billboard$SongID))
```

    ## [1] 28474

Looking at the billboard data, I see that in order to filter out only
unique values in a helpful way, I need to keep only the row with the
largest value of weeks on chart for each unique song. This should leave
only one row for each song, the row of the last week they were on the
charts which ensure the peak position of the lifetime of the song on the
billboards is accurate.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
billboard_last_week_on_chart <- billboard %>% group_by(SongID) %>% top_n(1, Weeks.on.Chart)
#View(billboard_last_week_on_chart)
count(billboard_last_week_on_chart)
```

    ## # A tibble: 28,474 x 2
    ## # Groups:   SongID [28,474]
    ##    SongID                                                n
    ##    <chr>                                             <int>
    ##  1 '03 Bonnie & ClydeJay-Z Featuring Beyonce Knowles     1
    ##  2 '65 Love AffairPaul Davis                             1
    ##  3 '98 Thug ParadiseTragedy, Capone, Infinite            1
    ##  4 'Round We GoBig Sister                                1
    ##  5 'til I Can Make It On My OwnTammy Wynette             1
    ##  6 'til It's Time To Say GoodbyeJonathan Cain            1
    ##  7 'Til My Baby Comes HomeLuther Vandross                1
    ##  8 'Til Summer Comes AroundKeith Urban                   1
    ##  9 'Til You Do Me RightAfter 7                           1
    ## 10 'TilThe Angels                                        1
    ## # ... with 28,464 more rows

Now, I’ll produce the same chart but with our filtered data and with the
y axis order revered.

``` r
library(ggplot2)
ggplot(billboard_last_week_on_chart, aes(x=Weeks.on.Chart, y=Peak.Position)) + geom_point(alpha=I(0.1))+  scale_y_continuous(trans = "reverse")
```

![](report_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

From this I can see that the vast majority of songs which did not climb
above the 50th spot on the charts did not make it to the charts at all
for more than about 20 weeks. However, the range of songs that made it
to a top 10 spot on the charts were on the charts for a total of between
1 and 87 weeks, a much larger range.

  - So what makes a song a hit song? \* According to Wikipedia, a song
    is typically considered a hit when it makes the top 40 of the
    Billboard Hot 100 and stays there for at least a week.

For the sake of dealing with less data, I’ll create a dataset filtered
by the songs that fit that criteria.

``` r
library(dplyr)
hitbillboard <- billboard %>% filter(Peak.Position <= 40 & Weeks.on.Chart > 1)
#View(hitbillboard)
```

``` r
length(unique(hitbillboard$SongID))
```

    ## [1] 12764

This reduces the dataset to 12851 unique songs. Should I chose to use it
for anything moving forward, this dataset is smaller and more manageable
without taking a random sample of songs. The ones left are by definition
actual hits.

This leads to my next question: *what is the typical route to the top of
the billboard charts? Do songs land on the top of the charts from the
start or do they climb there? Is it a slow climb or a slow one? What
about decent?*

To investigate this, I’ll use that dataset reduced to only the songs
than made it to the top 40 and stayed on the charts for at least a week.
With 12764 lines, this should be a crazy lineplot. I’ll see if there are
some general trends worth investigating before moving farther.

``` r
ggplot(hitbillboard, aes(x = Weeks.on.Chart, y = Week.Position, group = factor(SongID))) +
  geom_line(alpha=.05) + scale_y_continuous(trans = "reverse")
```

![](report_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
library(ggplot2)
ggplot(hitbillboard, aes(x=Weeks.on.Chart, y=Peak.Position)) + geom_point(alpha=I(0.1))+  scale_y_continuous(trans = "reverse")
```

![](report_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

It appears that most hit songs make the top of the charts early and are
followed by a sharp decine in popularity. I’m not quite understanding
this line at position 50 were most hit songs don’t drop below if they
are on the charts for more than 25 weeks. Next, I’ll look into just the
songs that make the top spot.

``` r
library(dplyr)
top_hit_billboard <- billboard %>% filter(Peak.Position <= 1 & Weeks.on.Chart > 1)
#View(hitbillboard)
```

``` r
length(unique(top_hit_billboard$SongID))
```

    ## [1] 1095

``` r
ggplot(top_hit_billboard, aes(x = Weeks.on.Chart, y = Week.Position, group = factor(SongID))) + geom_line(alpha=.05) + scale_y_continuous(trans = "reverse")
```

![](report_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

It is easier to see from this graph that most top songs start at or near
the top but tend to fall quickly, at least quicker than I expected.
However, we can see that there is a significant portion of songs that
endured for 40 to 60+ weeks. Of course, we aren’t able to compare this
to songs that never made the top 100 list. To make this or a similar
visualization effective, some sort of summary statistics would need to
be implemented to significantly reduce the clutter. I’ll move on for now
and see if something more interesting emerges.

During the process of looking into defining and understanding hit songs,
I started to wonder about one hit wonders. *Is there anything different
about one hit wonders in comparision to other top 100 songs?* Music
journalist Wayne Jancik defines a one-hit wonder as “an act that has won
a position on Billboard’s national, pop, Top 40 just once.”

I’ll filter the billboard data to keep just one-hit-wonders.

``` r
library(dplyr)
ohw_billboard <- billboard_last_week_on_chart %>% group_by(Performer) %>% dplyr::filter(n() == 1) %>% ungroup()
#View(ohw_billboard)
```

Looking at the data, I’m seeing variations in performers because of
features. These obviously aren’t being detected as the same performer.
If I wanted to do a visualization for one hit wonders, I’d need to clean
that data. I’ll try graphing a barplot with the frequency of one hit
wonder occurrences each week anyway to see if it’s worth investigating.
*Is there a period of time when one hit wonders were more prevalent? *

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
hit.freq = table(ohw_billboard$WeekID)
barplot(hit.freq)
```

![](report_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
#View(hit.freq)
```

There is an odd spike of one hit wonders that made the top 40 in one
month of 1965. Otherwise, there’s not much interesting here. As one last
look, I’ll see if there is a difference between the lifetime of one hit
wonders on the chart vs in comparison to all songs in our original
dataset. If this shows something interesting, I could work on cleaning
to better define one hit wonders and filter them out for the comparison
group.

``` r
library(MASS)
hist(ohw_billboard$Weeks.on.Chart, xlim = c(0,80), breaks=40)
```

![](report_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
library(MASS)
hist(billboard_last_week_on_chart$Weeks.on.Chart, xlim = c(0,80), breaks=60)
```

![](report_files/figure-gfm/unnamed-chunk-17-1.png)<!-- --> There could
be something to investigate more here. The histograms are frequencies in
bins so the comparison is by general distribution. This still isn’t very
interesting so I’ll move on for now.

### Second Dataset: Importing and Initial exploration

The same source also has a features dataset which has a list of
additional values for each track pulled from the Spotify Web API.

Original Source:
<https://data.world/kcmillersean/billboard-hot-100-1958-2017> (the
original dataset was pulled from Spotify Web API)

``` r
library("httr")
library("readxl")
GET("https://query.data.world/s/5phlvfg3im7qyffzy66v4m2uvzmnr3", write_disk(tf <- tempfile(fileext = ".xlsx")))
```

    ## Response [https://download.data.world/file_download/kcmillersean/billboard-hot-100-1958-2017/Hot%20100%20Audio%20Features.xlsx?auth=eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50Om1ja2F5bGFoYWdlcnR5IiwiaXNzIjoiYWdlbnQ6bWNrYXlsYWhhZ2VydHk6OmE0ZGU1MTNlLTY5Y2UtNDY4YS05ZTc2LTRkMzFlMzAxZDI0ZiIsImlhdCI6MTYwNjM0MDk2OCwicm9sZSI6WyJ1c2VyIiwidXNlcl9hcGlfYWRtaW4iLCJ1c2VyX2FwaV9yZWFkIiwidXNlcl9hcGlfd3JpdGUiXSwiZ2VuZXJhbC1wdXJwb3NlIjpmYWxzZSwidXJsIjoiNTgzMTYzZTAyMTU0OTNjMTI3NzE4OTg1ZTkyMzk1YjY0MzEzYTc0NyJ9.snQDN2aVskJ-HU8NRNFwtZliVYi7bpnG2Hkr9NjQLcxvVEyjARmhiRhDAn6Xc3p3-4_mOQBh_sINHHyfzSegyQ]
    ##   Date: 2020-12-11 14:54
    ##   Status: 200
    ##   Content-Type: application/vnd.openxmlformats-officedocument.spreadsheetml.sheet
    ##   Size: 5.73 MB
    ## <ON DISK>  C:\Users\13mck\AppData\Local\Temp\RtmpSYsamz\file90b044ac2994.xlsx

``` r
genres_first_look <- read_excel(tf)
names(genres_first_look)
```

    ##  [1] "SongID"                    "Performer"                
    ##  [3] "Song"                      "spotify_genre"            
    ##  [5] "spotify_track_id"          "spotify_track_preview_url"
    ##  [7] "spotify_track_album"       "spotify_track_explicit"   
    ##  [9] "spotify_track_duration_ms" "spotify_track_popularity" 
    ## [11] "danceability"              "energy"                   
    ## [13] "key"                       "loudness"                 
    ## [15] "mode"                      "speechiness"              
    ## [17] "acousticness"              "instrumentalness"         
    ## [19] "liveness"                  "valence"                  
    ## [21] "tempo"                     "time_signature"

genres\_first\_look Data Columns:

  - duration\_ms: The duration of the track in milliseconds.
  - key: The estimated overall key of the track. E.g. 0 = C, 1 = C/D, 2
    = D, and so on. If no key was detected, the value is -1.
  - mode: Indicates the modality (major or minor) of a track, the type
    of scale from which its melodic content is derived. Major is
    represented by 1 and minor is 0.
  - time\_signature: An estimated overall time signature of a track.
  - acousticness: A confidence measure from 0.0 to 1.0 of whether the
    track is acoustic.
  - danceability: Describes how suitable a track is for dancing based on
    a combination of musical elements including tempo, rhythm stability,
    beat strength, and overall regularity.
  - energy: Represents a perceptual measure of intensity and activity.
  - instrumentalness: Predicts whether a track contains no vocals. “Ooh”
    and “aah” sounds are treated as instrumental in this context.
  - liveness: Detects the presence of an audience in the recording.
  - loudness: The overall loudness of a track in decibels (dB).
  - speechiness: Detects the presence of spoken words in a track.
  - valence: Describing the musical positiveness conveyed by a track.
    Tracks with high valence sound more positive (e.g. happy, cheerful,
    euphoric), while tracks with low valence sound more negative
    (e.g. sad, depressed, angry).
  - tempo: The overall estimated tempo of a track in beats per minute
    (BPM).
  - id: The Spotify ID for the track.
  - uri: The Spotify URI for the track.
  - track\_href: A link to the Web API endpoint providing full details
    of the track.
  - analysis\_url: An HTTP URL to access the full audio analysis of this
    track. An access token is required to access this data.
  - type: The object type: “audio\_features”

Since this data was pulled from Spotify’s API through searching the
Spotify SongID column values from the billboard dataset, I’ll compare
the unique values from the SongID column in both datasets.

``` r
print('Unique Song IDs in billboard:')
```

    ## [1] "Unique Song IDs in billboard:"

``` r
length(unique(billboard$SongID))
```

    ## [1] 28474

``` r
print('Unique Song IDs in genres_first_look:')
```

    ## [1] "Unique Song IDs in genres_first_look:"

``` r
length(unique(genres_first_look$SongID))
```

    ## [1] 28377

While it appears there is not only data from the second set for about
100 songs, the fact that it isn’t an exact match makes me question how
the second extra data was collected. I’ll search for SongIDs in the
billboard data which is not included in the genres\_first\_look data.

``` r
require(dplyr) 

head(anti_join(billboard,genres_first_look))
```

    ## Joining, by = c("Song", "Performer", "SongID")

    ##                                               ï..url     WeekID Week.Position
    ## 1 http://www.billboard.com/charts/hot-100/1958-08-02 1958-08-02             1
    ## 2 http://www.billboard.com/charts/hot-100/1995-12-02 1995-12-02             1
    ## 3 http://www.billboard.com/charts/hot-100/1997-10-11 1997-10-11             1
    ## 4 http://www.billboard.com/charts/hot-100/2006-07-01 2006-07-01             1
    ## 5 http://www.billboard.com/charts/hot-100/2009-10-24 2009-10-24             1
    ## 6 http://www.billboard.com/charts/hot-100/2011-02-26 2011-02-26             1
    ##                                                               Song
    ## 1                                                 Poor Little Fool
    ## 2                                                    One Sweet Day
    ## 3 Candle In The Wind 1997/Something About The Way You Look Tonight
    ## 4                                              Do I Make You Proud
    ## 5                                                                3
    ## 6                                                    Born This Way
    ##                    Performer
    ## 1               Ricky Nelson
    ## 2 Mariah Carey & Boyz II Men
    ## 3                 Elton John
    ## 4               Taylor Hicks
    ## 5             Britney Spears
    ## 6                  Lady Gaga
    ##                                                                       SongID
    ## 1                                               Poor Little FoolRicky Nelson
    ## 2                                    One Sweet DayMariah Carey & Boyz II Men
    ## 3 Candle In The Wind 1997/Something About The Way You Look TonightElton John
    ## 4                                            Do I Make You ProudTaylor Hicks
    ## 5                                                            3Britney Spears
    ## 6                                                     Born This WayLady Gaga
    ##   Instance Previous.Week.Position Peak.Position Weeks.on.Chart
    ## 1        1                     NA             1              1
    ## 2        1                     NA             1              1
    ## 3        1                     NA             1              1
    ## 4        1                     NA             1              1
    ## 5        1                     NA             1              1
    ## 6        1                     NA             1              1

``` r
#The number of Song IDs in billboard that are not represented in the additional features dataset
count(anti_join(billboard,genres_first_look))
```

    ## Joining, by = c("Song", "Performer", "SongID")

    ##      n
    ## 1 2056

``` r
#Unique Song IDs in billboard
length(unique(billboard$SongID))
```

    ## [1] 28474

It appears 2056 songs of the original 28474 song dataset do not have
values in the supplemental features dataset. That is close to 7%. I
would prefer to work with data for all the songs as this would show a
better representation and allow for percent of whole by week
comparisons. I found a dataset that would likely be more effective.
Unfortunately, the complex steps of attaining that data for personal use
is not within the scope of this assignment.

I’ll first take a look at the columns and data types.

``` r
#str(genres_first_look)
```

I see a combination of mostly characters and numerical data. I’ll first
start with a correlation matrix for this new data to get a sense of how
the variables may be correlated. Note that we are working with all song
data from the second dataset which includes songs that are not of
interest. At this point, I am just getting farmiliar with the data.

``` r
my_num_data <- genres_first_look[, sapply(genres_first_look, is.numeric)]
mydata.cor = cor(my_num_data, use = "complete.obs")
library(corrplot)
```

    ## corrplot 0.84 loaded

``` r
corrplot(mydata.cor, method="circle")
```

![](report_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

Some things that jump out are a strong positive correlation between
loudness and energy, not a surprise. Likewise, there is a strongly
negative correlation between energy and acousticness. Again, this isn’t
a surprise, but may be interesting to consider in terms of the makeup of
top 100 songs, potentially over time.

As for the character data, the only one really of interest is the genre.

I’m intrigued by the genre as this could be interesting to see a change
overtime.

``` r
print('Unique Genres in genres_first_look:')
```

    ## [1] "Unique Genres in genres_first_look:"

``` r
length(unique(genres_first_look$spotify_genre))
```

    ## [1] 3276

``` r
head(genres_first_look$spotify_genre)
```

    ## [1] "['pop reggaeton']"                                                                                        
    ## [2] "['arkansas country', 'contemporary country', 'country', 'country road', 'modern country rock', 'redneck']"
    ## [3] "['dance pop', 'pop']"                                                                                     
    ## [4] "['dfw rap', 'melodic rap', 'rap']"                                                                        
    ## [5] "['dfw rap', 'rap', 'southern hip hop', 'trap']"                                                           
    ## [6] "['dance pop', 'pop', 'post-teen pop']"

Unfortunately, the genre column has 3276 unique values. I decided it was
worth investigating so I used OpenRefine to clean the data, focusing
more on the main, most recognizable categories. As a starting point, I
used the below aggregate genres provided by Kevin Schaich
(<https://github.com/kevinschaich/billboard>):

\[{“rock”: \[“symphonic rock”, “jazz-rock”, “heartland rock”, “rap
rock”, “garage rock”, “folk-rock”, “roots rock”, “adult alternative
pop rock”, “rock roll”, “punk rock”, “arena rock”, “pop-rock”, “glam
rock”, “southern rock”, “indie rock”, “funk rock”, “country rock”,
“piano rock”, “art rock”, “rockabilly”, “acoustic rock”, “progressive
rock”, “folk rock”, “psychedelic rock”, “rock & roll”, “blues rock”,
“alternative rock”, “rock and roll”, “soft rock”, “rock and indie”,
“hard rock”, “pop/rock”, “pop rock”, “rock”, “classic pop and rock”,
“psychedelic”, “british psychedelia”, “punk”, “metal”, “heavy
metal”\]}, {“alternative/indie”: \[“adult alternative pop rock”,
“alternative rock”, “alternative metal”, “alternative”, “lo-fi indie”,
“indie”, “indie folk”, “indietronica”, “indie pop”, “indie rock”, “rock
and indie”\]},

{“electronic/dance”: \[“dance and electronica”, “electro house”,
“electronic”, “electropop”, “progressive house”, “hip house”, “house”,
“eurodance”, “dancehall”, “dance”, “trap”\]},

{“soul”: \[“psychedelic soul”, “deep soul”, “neo-soul”, “neo soul”,
“southern soul”, “smooth soul”, “blue-eyed soul”, “soul and reggae”,
“soul”\]},

{“classical/soundtrack”: \[“classical”, “orchestral”, “film soundtrack”,
“composer”\]},

{“pop”: \[“country-pop”, “latin pop”, “classical pop”, “pop-metal”,
“orchestral pop”, “instrumental pop”, “indie pop”, “sophisti-pop”,
“pop punk”, “pop reggae”, “britpop”, “traditional pop”, “power pop”,
“sunshine pop”, “baroque pop”, “synthpop”, “art pop”, “teen pop”,
“psychedelic pop”, “folk pop”, “country pop”, “pop rap”, “pop soul”,
“pop and chart”, “dance-pop”, “pop”, “top 40”\]},

{“hip-hop/rnb”: \[“conscious hip hop”, “east coast hip hop”, “hardcore
hip hop”, “west coast hip hop”, “hiphop”, “southern hip hop”, “hip-hop”,
“hip hop”, “hip hop rnb and dance hall”, “contemporary r b”, “gangsta
rap”, “rapper”, “rap”, “rhythm and blues”, “contemporary rnb”,
“contemporary r\&b”, “rnb”, “rhythm & blues”,“r\&b”, “blues”\]},

{“disco”: \[“disco”\]},

{“swing”: \[“swing”\]},

{“folk”: \[“contemporary folk”, “folk”\]},

{“country”: \[“country rock”, “country-pop”, “country pop”,
“contemporary country”, “country”\]},

{“jazz”: \[“vocal jazz”, “jazz”, “jazz-rock”\]},

{“religious”: \[“christian”, “christmas music”, “gospel”\]},

{“blues”: \[“delta blues”, “rock blues”, “urban blues”, “electric
blues”, “acoustic blues”, “soul blues”, “country blues”, “jump blues”,
“classic rock. blues rock”, “jazz and blues”, “piano blues”, “british
blues”, “british rhythm & blues”, “rhythm and blues”, “blues”, “blues
rock”, “rhythm & blues”\]},

{“reggae”: \[“reggae fusion”, “roots reggae”, “reggaeton”, “pop reggae”,
“reggae”, “soul and reggae”\]}\]

``` r
#Cleaned genre data.
genres <- read.csv("C:/Users/13mck/Desktop/genres-final.csv")
names(genres)
```

    ##  [1] "SongID"                    "Performer"                
    ##  [3] "Song"                      "spotify_genre.1"          
    ##  [5] "Genre"                     "spotify_track_id"         
    ##  [7] "spotify_track_preview_url" "spotify_track_album"      
    ##  [9] "spotify_track_explicit"    "spotify_track_duration_ms"
    ## [11] "spotify_track_popularity"  "danceability"             
    ## [13] "energy"                    "key"                      
    ## [15] "loudness"                  "mode"                     
    ## [17] "speechiness"               "acousticness"             
    ## [19] "instrumentalness"          "liveness"                 
    ## [21] "valence"                   "tempo"                    
    ## [23] "time_signature"

``` r
head(genres)
```

    ##                                               SongID
    ## 1                      AdictoTainy, Anuel AA & Ozuna
    ## 2 The Ones That Didn't Make It Back HomeJustin Moore
    ## 3                  ShallowLady Gaga & Bradley Cooper
    ## 4                EnemiesPost Malone Featuring DaBaby
    ## 5    Bacc At It AgainYella Beezy, Gucci Mane & Quavo
    ## 6                             The ArcherTaylor Swift
    ##                         Performer                                   Song
    ## 1         Tainy, Anuel AA & Ozuna                                 Adicto
    ## 2                    Justin Moore The Ones That Didn't Make It Back Home
    ## 3      Lady Gaga & Bradley Cooper                                Shallow
    ## 4    Post Malone Featuring DaBaby                                Enemies
    ## 5 Yella Beezy, Gucci Mane & Quavo                       Bacc At It Again
    ## 6                    Taylor Swift                             The Archer
    ##    spotify_genre.1       Genre       spotify_track_id
    ## 1    pop reggaeton         pop 3jbT1Y5MoPwEIpZndDDwVq
    ## 2 arkansas country     country                       
    ## 3        dance pop         pop 2VxeLyX666F8uXCJ0dZF8B
    ## 4          dfw rap hip-hop/rnb 0Xek5rqai2jcOWCYWJfVCF
    ## 5          dfw rap hip-hop/rnb 2biNa12dMbHJrHVFRt8JyO
    ## 6        dance pop         pop 3pHkh7d0lzM2AldUtz2x37
    ##                                                                                     spotify_track_preview_url
    ## 1                                                                                                            
    ## 2                                                                                                            
    ## 3                                                                                                            
    ## 4                                                                                                            
    ## 5 https://p.scdn.co/mp3-preview/fa6fa6f6f363be29ae45af3470675ef5cbcd081f?cid=b8d3901151d34489a160e3cf0ab1fa94
    ## 6                                                                                                            
    ##              spotify_track_album spotify_track_explicit
    ## 1 Adicto (with Anuel AA & Ozuna)                  FALSE
    ## 2                                                    NA
    ## 3      A Star Is Born Soundtrack                  FALSE
    ## 4           Hollywood's Bleeding                   TRUE
    ## 5               Bacc At It Again                   TRUE
    ## 6                          Lover                  FALSE
    ##   spotify_track_duration_ms spotify_track_popularity danceability energy key
    ## 1                    270740                       91        0.734  0.836  10
    ## 2                        NA                       NA           NA     NA  NA
    ## 3                    215733                       88        0.572  0.385   7
    ## 4                    196760                       86        0.542  0.674   6
    ## 5                    228185                       61        0.948  0.623   8
    ## 6                    211240                       76        0.292  0.574   0
    ##   loudness mode speechiness acousticness instrumentalness liveness valence
    ## 1   -4.803    0      0.0735      0.01700         1.64e-05   0.1790   0.623
    ## 2       NA   NA          NA           NA               NA       NA      NA
    ## 3   -6.362    1      0.0308      0.37100         0.00e+00   0.2310   0.323
    ## 4   -4.169    1      0.2100      0.05880         0.00e+00   0.0955   0.667
    ## 5   -5.725    0      0.1680      0.00124         1.07e-06   0.0716   0.856
    ## 6   -9.375    1      0.0401      0.12000         5.69e-03   0.0663   0.166
    ##     tempo time_signature
    ## 1  80.002              4
    ## 2      NA             NA
    ## 3  95.799              4
    ## 4  76.388              4
    ## 5 135.979              4
    ## 6 124.344              4

``` r
uGenres <- unique(genres$Genre)
print('The total number of unique genres is: ')
```

    ## [1] "The total number of unique genres is: "

``` r
length(uGenres)
```

    ## [1] 215

``` r
head(genres$Genre)
```

    ## [1] "pop"         "country"     "pop"         "hip-hop/rnb" "hip-hop/rnb"
    ## [6] "pop"

With the cleaning, I dropped the unique genres down to 215 which is
better. I chose not to force songs into broader genres if putting them
there would be a stretch. I anticipate only using the main, most
recognizable categories like rock, pop, country, hip-hop/rnb,
alternative/indie, and reggae. Based on initial samples, I expect about
20% missing values when I merge this with the billboard data. This
wouldn’t have been an issue if I had access to some complete song
datasets that exist. Likewise, I was not expecting to have to do much
cleaning so I’ll consider the day I spend on that alone a solid enough
effort for now. Should I choose to use the genre for my final
visualization, I will return for additional cleaning.

Next, I’ll merge the two datasets using the SongID column.

### Merged Data

``` r
#Removing duplicate SongID rows in the genre dataset to allow for merging. 
genres <- genres[!duplicated(genres[c('SongID')]),] 
```

``` r
merged<-merge(billboard, genres, by = "SongID",all.x=TRUE)
merged$WeekID <- as.Date(merged$WeekID , format = "%m/%d/%Y")

View(merged)
```

267191 of the 320496 rows have data for the genre. As expected, we have
data from the additional dataset for about 83% of the original billboard
dataset.

I want to investigate the data types for our combined dataset.

``` r
#str(merged)
```

Before I look into the genres, I want to see if I can find any
correlation between song position/popularity/weeks on chart and any of
the other numerical values. I’ll again filter the merged data to create
a new dataframe with the row for each song with the largest
weeks.on.chart value.

``` r
merged_last_week_on_chart<- merged %>% group_by(SongID) %>% top_n(1, Weeks.on.Chart)
```

``` r
my_num_data_merged <- merged_last_week_on_chart[, sapply(merged_last_week_on_chart, is.numeric)]
mydata.cor_merged = cor(my_num_data_merged, use = "complete.obs")
library(corrplot)
corrplot(mydata.cor_merged, method="circle")
```

![](report_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

I know any of the positional data is on a scale of 1 to 100 with 1 being
the top spot so I have to keep that in mind when looking at the
correlations. Some notable findings here include:

1.  Weeks.on.Chart and Peak.Position have a fairly strong negative
    correlation. Given that the positions data is on a scale of 1 to 100
    with 1 being best, this could be redone and more understandably be
    interpreted as a positive correlation. As a song is on the charts
    longer, it is more likely to reach a higher spot on the charts. This
    reaffirms what was discovered earlier.

2.  Peak.Position and Week.Position have a weak but present positive
    correlation. The Week.Position couldn’t be higher than the
    Peak.Position for each song, but a scatter plot (below) shows a
    cluster of songs that made the top 25 in the charts and had their
    last appearance on the charts as a top 50 song. This in in addition
    to a cluster across all ranks of songs that had their last
    appearance in the bottom 10 songs of the chart.

<!-- end list -->

``` r
library(ggplot2)
ggplot(merged_last_week_on_chart, aes(x=Peak.Position, y=Week.Position)) + geom_point(alpha=I(0.1))+scale_x_continuous(trans = "reverse")+scale_y_continuous(trans = "reverse")
```

![](report_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

3.  Peak.Position and spotify\_track\_popularity have a slight negative
    correlation (again, this should be interpreted as a positive
    correlation). Since all my dataset made the top 100, I’ll also use
    the correlation between spotify\_track\_popularity and the data from
    the additional dataset as hints for what makes a popular song.

<!-- end list -->

``` r
library(ggplot2)
ggplot(merged_last_week_on_chart, aes(x=Peak.Position, y=spotify_track_popularity)) + geom_point(alpha=I(0.1))+scale_x_continuous(trans = "reverse")
```

    ## Warning: Removed 4848 rows containing missing values (geom_point).

![](report_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

4.  There isn’t any notable correlation between Peak.Position and any of
    the features in the additional dataset. This isn’t surprising
    considering our dataset has all top 100 songs. There is a chance
    there is a homogeneity to a majority of the songs that make the top
    100 list. There could also be a difference between what makes a top
    song depending on the time period.

I’ll create some histograms to investigate the distribution of where all
the top 100 songs from all the weeks combined fall in some categories of
interest identified by the correlation plot: acousticness, loudness,
dancability, valence, and speechiness.

``` r
library(MASS)
hist(merged$acousticness, breaks=30)
```

![](report_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

``` r
library(MASS)
hist(merged$loudness, breaks=30)
```

![](report_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

``` r
library(MASS)
hist(merged$danceability, breaks=30)
```

![](report_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

``` r
library(MASS)
hist(merged$valence, breaks=30)
```

![](report_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

``` r
library(MASS)
hist(merged$speechiness, breaks=30)
```

![](report_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

Top songs are more likely to be less acoustic, louder, and less speech
filled (fewer words, more music). They tend to also be slightly easier
to dance to and positive (valence).

I’ll compare these same traits of top 100 songs in 1959 and 2000 to see
if this is worth investigating more.

``` r
#Converting the date to be read as dates.

#merged$WeekID <- as.Date(merged$WeekID , format = "%m/%d/$Y")

#View(merged)
```

``` r
#Filtered for 1959 dates.
merged1959 <- subset(merged, WeekID >= ("1959-01-01") & WeekID <= ("1959-12-31"))
```

``` r
#Filtered for 2000 dates.
merged2000 <- subset(merged, WeekID >= ("2000-01-01") & WeekID <= ("2000-12-31"))
```

``` r
library(MASS)
hist(merged1959$acousticness, xlim = c(0,1),breaks=30)
```

![](report_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

``` r
library(MASS)
hist(merged2000$acousticness, xlim = c(0,1),breaks=15)
```

![](report_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

I see a significant difference in the distributions here. Top 100 1959
songs are much more likely to be acoustic.

``` r
library(MASS)
hist(merged1959$loudness, xlim = c(-30,0),breaks=15)
```

![](report_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

``` r
library(MASS)
hist(merged2000$loudness, xlim = c(-30,0),breaks=15)
```

![](report_files/figure-gfm/unnamed-chunk-48-1.png)<!-- -->

2000 top songs were louder in comparison to 1959 songs.

``` r
library(MASS)
hist(merged1959$danceability, xlim = c(0,1),breaks=30)
```

![](report_files/figure-gfm/unnamed-chunk-49-1.png)<!-- -->

``` r
library(MASS)
hist(merged2000$danceability, xlim = c(0,1),breaks=15)
```

![](report_files/figure-gfm/unnamed-chunk-50-1.png)<!-- -->

2000 top songs were more dancable in comparison to 1959 songs. I’m
definely seeing something interesting here.

``` r
library(MASS)
hist(merged1959$valence, xlim = c(0,1),breaks=30)
```

![](report_files/figure-gfm/unnamed-chunk-51-1.png)<!-- -->

``` r
library(MASS)
hist(merged2000$valence, xlim = c(0,1),breaks=30)
```

![](report_files/figure-gfm/unnamed-chunk-52-1.png)<!-- -->

I see a tend for top 1959 songs to be more positive in comparison to top
2000 songs.

``` r
library(MASS)
hist(merged1959$speechiness, xlim = c(0,1),breaks=30)
```

![](report_files/figure-gfm/unnamed-chunk-53-1.png)<!-- -->

``` r
library(MASS)
hist(merged2000$speechiness, xlim = c(0,1),breaks=15)
```

![](report_files/figure-gfm/unnamed-chunk-54-1.png)<!-- -->

There is less of a difference in speechiness.

I could also graph the averages of each these traits overtime with a
line chart. I’ll scale the traits I plan to include from 0 to 1 so they
can all be placed on a line chart together. Since I’m not sure how to
handle the loudness since it is a combination of positive and negative
numbers so I’ll return to that trait later.

*How have traits of popular songs changed over time?*

``` r
#Creating new column for year, month, and day. 
library(tidyr)
```

    ## 
    ## Attaching package: 'tidyr'

    ## The following object is masked _by_ '.GlobalEnv':
    ## 
    ##     billboard

``` r
merged <- merged %>%
  separate(WeekID, sep="-", into = c("year", "month", "day"))
```

``` r
#New dataframe for mean for each year.
library(dplyr)
mean_data <- group_by(merged, year) %>%
             summarise(danceability = mean(danceability, na.rm = TRUE),energy = mean(energy, na.rm = TRUE),speechiness = mean(speechiness, na.rm = TRUE),acousticness = mean(acousticness, na.rm = TRUE),liveness = mean(liveness, na.rm = TRUE),valence = mean(valence, na.rm = TRUE),loudness = mean(loudness, na.rm = TRUE))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
mean_data <- as.data.frame(mean_data)

class(mean_data)
```

    ## [1] "data.frame"

``` r
ggplot(data = mean_data, aes(x = year,group = 1)) + 
  geom_line(aes(y = danceability),color="blue") + 
  geom_line(aes(y = energy),color="green") +
  geom_line(aes(y = speechiness),color="purple") +
  geom_line(aes(y = acousticness),color="red") +
  geom_line(aes(y = liveness),color="orange") +
  geom_line(aes(y = valence)) +
  labs(y = "") # Delete or change y axis title if desired.
```

![](report_files/figure-gfm/unnamed-chunk-57-1.png)<!-- -->

This is interesting. I can see how traits of what makes a top 100 song
have changed over the years. This can especially be seen in the
acousticness and valence. From previous findings, we would expect
something interesting from the loudness as well.

I also remember reading about radar charts and wonder if I could make an
interactive radar chart with a slider to look into the traits for each
year or decade. Or perhaps I could make one with an overlapping view
with averages by decade. The problem with radar charts is that it is
difficult to compare or get a sense of a specific measurement. However,
if I layered them then comparing would be easier. Also, exact values are
not the point of showing this data. The value is in the comparison
between years or decades.

The last trait I haven’t spent much time on is the genre. I would like
to see the change over time in what genres make the top 100 list. I’ll
first start with a stacked bar chart as use the years column rather than
the WeekID column. *Has the popularity of different genres changed over
time?*

``` r
genrebyyear <- table(merged$year, merged$Genre) 
genrebyyear <- as.data.frame(genrebyyear)
names(genrebyyear)[names(genrebyyear) == "Var1"] <- "year"
names(genrebyyear)[names(genrebyyear) == "Var2"] <- "Genre"
names(genrebyyear)[names(genrebyyear) == "Freq"] <- "Count"
```

``` r
# library
library(ggplot2)
topgenrebyyear <- genrebyyear %>% filter(Genre %in% c("rock", "pop","alternative/indie","electronic/dance","soul","hip-hop/rnb","country"))
  

# Stacked
ggplot(topgenrebyyear, aes(fill=Genre, y=Count, x=year)) + 
    geom_bar(position="stack", stat="identity")
```

![](report_files/figure-gfm/unnamed-chunk-59-1.png)<!-- -->

I don’t generally like stacked bar graphs because it is difficult to
make comparisons between groups. However, I discovered streamgraphs
which may be a better fit.

“Streamgraphs are a generalization of stacked area graphs where the
baseline is free. By shifting the baseline, it is possible to minimize
the change in slope (or wiggle) in individual series, thereby making it
easier to perceive the thickness of any given layer across the data.
Byron & Wattenberg describe several streamgraph algorithms in ‘Stacked
Graphs—Geometry & Aesthetics3’”4

``` r
#installing and loading streamgraph
#install.packages("devtools")
library(devtools)
```

    ## Loading required package: usethis

``` r
#install.packages("zoo")
devtools::install_github("hrbrmstr/streamgraph")
```

    ## WARNING: Rtools is required to build R packages, but is not currently installed.
    ## 
    ## Please download and install Rtools 4.0 from https://cran.r-project.org/bin/windows/Rtools/.

    ## Skipping install of 'streamgraph' from a github remote, the SHA1 (76f7173e) has not changed since last install.
    ##   Use `force = TRUE` to force installation

``` r
topgenrebyyear$year <- as.Date(topgenrebyyear$year, format="%Y")
```

``` r
library(dplyr)
library(streamgraph)

#topgenrebyyear %>% 
  #streamgraph(key="Genre", value="Count", date="year", interpolate="linear", sort=TRUE)%>%     #sg_fill_brewer("PuOr")%>%
  #sg_fill_manual(c("black", "#ffa500", "blue", "white", "#00ff00", "red")) %>% 
  #sg_axis_x(tick_interval=3, tick_units="year","%Y") 
  #sg_legend(FALSE, "Genre: ")
```

Should I choose to move forward with this chart, I would like to add key
genre labels and more appropriate colors. The axis labels would also
need adjusted. As mentioned before, if I continue down this route for my
final visualization, I will probably return to the cleaning to see if I
can show more of a representation of the pre-1990 song genres.

At this point, Professor Weigle suggested I consider also looking into
how particular genres have changed over time. To see if there was
something interesting to be discovered there, I will create three new
datasets with just rock, pop, and country music and create a multiple
line plot for each featuring the music characteristics as before. *How
have popular songs in key genres changed over time?*

``` r
library(ggplot2)
rockmusic <- merged %>% filter(Genre %in% c("rock"))
```

``` r
#New dataframe for mean for each year for just rock music.
library(dplyr)
mean_data_rockmusic <- group_by(rockmusic, year) %>%
             summarise(danceability = mean(danceability, na.rm = TRUE),energy = mean(energy, na.rm = TRUE),speechiness = mean(speechiness, na.rm = TRUE),acousticness = mean(acousticness, na.rm = TRUE),liveness = mean(liveness, na.rm = TRUE),valence = mean(valence, na.rm = TRUE),loudness = mean(loudness, na.rm = TRUE))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
mean_data_rockmusic <- as.data.frame(mean_data_rockmusic)

class(mean_data_rockmusic)
```

    ## [1] "data.frame"

``` r
p1 <- ggplot(data = mean_data_rockmusic, aes(x = year,group = 1)) + 
  geom_line(aes(y = danceability),color="blue") + 
  geom_line(aes(y = energy),color="green") +
  geom_line(aes(y = speechiness),color="purple") +
  geom_line(aes(y = acousticness),color="red") +
  geom_line(aes(y = liveness),color="orange") +
  geom_line(aes(y = valence)) +
  labs(y = "") # Delete or change y axis title if desired.
p1+theme(axis.text.x = element_text(angle = 90,vjust = .5))
```

![](report_files/figure-gfm/unnamed-chunk-64-1.png)<!-- -->

This is interesting. I can see how rock music has changed since 1958.

``` r
library(ggplot2)
popmusic <- merged %>% filter(Genre %in% c("pop"))
```

``` r
#New dataframe for mean for each year for just pop music.
library(dplyr)
mean_data_popmusic <- group_by(popmusic, year) %>%
             summarise(danceability = mean(danceability, na.rm = TRUE),energy = mean(energy, na.rm = TRUE),speechiness = mean(speechiness, na.rm = TRUE),acousticness = mean(acousticness, na.rm = TRUE),liveness = mean(liveness, na.rm = TRUE),valence = mean(valence, na.rm = TRUE),loudness = mean(loudness, na.rm = TRUE))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
mean_data_popmusic <- as.data.frame(mean_data_popmusic)

class(mean_data_popmusic)
```

    ## [1] "data.frame"

``` r
p1 <- ggplot(data = mean_data_popmusic, aes(x = year,group = 1)) + 
  geom_line(aes(y = danceability),color="blue") + 
  geom_line(aes(y = energy),color="green") +
  geom_line(aes(y = speechiness),color="purple") +
  geom_line(aes(y = acousticness),color="red") +
  geom_line(aes(y = liveness),color="orange") +
  geom_line(aes(y = valence)) +
  labs(y = "") # Delete or change y axis title if desired.
p1+theme(axis.text.x = element_text(angle = 90,vjust = .5))
```

![](report_files/figure-gfm/unnamed-chunk-67-1.png)<!-- -->

``` r
library(ggplot2)
countrymusic <- merged %>% filter(Genre %in% c("country"))
```

``` r
#New dataframe for mean for each year for just country music.
library(dplyr)
mean_data_countrymusic <- group_by(countrymusic, year) %>%
             summarise(danceability = mean(danceability, na.rm = TRUE),energy = mean(energy, na.rm = TRUE),speechiness = mean(speechiness, na.rm = TRUE),acousticness = mean(acousticness, na.rm = TRUE),liveness = mean(liveness, na.rm = TRUE),valence = mean(valence, na.rm = TRUE),loudness = mean(loudness, na.rm = TRUE))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
mean_data_countrymusic <- as.data.frame(mean_data_countrymusic)

class(mean_data_countrymusic)
```

    ## [1] "data.frame"

``` r
p1 <- ggplot(data = mean_data_countrymusic, aes(x = year,group = 1)) + 
  geom_line(aes(y = danceability),color="blue") + 
  geom_line(aes(y = energy),color="green") +
  geom_line(aes(y = speechiness),color="purple") +
  geom_line(aes(y = acousticness),color="red") +
  geom_line(aes(y = liveness),color="orange") +
  geom_line(aes(y = valence)) +
  labs(y = "") # Delete or change y axis title if desired.
p1+theme(axis.text.x = element_text(angle = 90,vjust = .5))
```

![](report_files/figure-gfm/unnamed-chunk-70-1.png)<!-- -->

There is certainly much to work with here. There are interesting changes
in all three genres.

## Final Questions: Results of the EDA

Question 1: *How has the prevalence of genres on the top 100 songs
billboard changed over time?*

Sketch:

``` r
#topgenrebyyear %>% 
  #streamgraph(key="Genre", value="Count", date="year", interpolate="linear", sort=TRUE)%>%     #sg_fill_brewer("PuOr")%>%
  #sg_fill_manual(c("black", "#ffa500", "blue", "white", "#00ff00", "red")) %>% 
  #sg_axis_x(tick_interval=3, tick_units="year","%Y") 
  #sg_legend(FALSE, "Genre: ")
```

The streamplot shows the main genre makeup of the top 100 songs from
1959 through 2019. It is easy to see how rock has steadily become less
popular since 1986 as pop and hip hop have taken larger percentage of
the billboard. I can also be seen that the soul genre has become less
recognized since its surge of popularity in 1965 to 1980. Country,
however, has become more popular in the 2000s.

Question 2: *How has the pop, rock, and country genres changed over
time? *

Sketches: (I will most likely choose this question for my final
visualization.)

``` r
p1 <- ggplot(data = mean_data_rockmusic, aes(x = year,group = 1)) + 
  geom_line(aes(y = danceability),color="blue") + 
  geom_line(aes(y = energy),color="green") +
  geom_line(aes(y = speechiness),color="purple") +
  geom_line(aes(y = acousticness),color="red") +
  geom_line(aes(y = liveness),color="orange") +
  geom_line(aes(y = valence)) +
  labs(y = "") # Delete or change y axis title if desired.
p1+theme(axis.text.x = element_text(angle = 90,vjust = .5))
```

![](report_files/figure-gfm/unnamed-chunk-72-1.png)<!-- -->

``` r
p1 <- ggplot(data = mean_data_popmusic, aes(x = year,group = 1)) + 
  geom_line(aes(y = danceability),color="blue") + 
  geom_line(aes(y = energy),color="green") +
  geom_line(aes(y = speechiness),color="purple") +
  geom_line(aes(y = acousticness),color="red") +
  geom_line(aes(y = liveness),color="orange") +
  geom_line(aes(y = valence)) +
  labs(y = "") # Delete or change y axis title if desired.
p1+theme(axis.text.x = element_text(angle = 90,vjust = .5))
```

![](report_files/figure-gfm/unnamed-chunk-73-1.png)<!-- -->

``` r
p1 <- ggplot(data = mean_data_countrymusic, aes(x = year,group = 1)) + 
  geom_line(aes(y = danceability),color="blue") + 
  geom_line(aes(y = energy),color="green") +
  geom_line(aes(y = speechiness),color="purple") +
  geom_line(aes(y = acousticness),color="red") +
  geom_line(aes(y = liveness),color="orange") +
  geom_line(aes(y = valence)) +
  labs(y = "") # Delete or change y axis title if desired.
p1+theme(axis.text.x = element_text(angle = 90,vjust = .5))
```

![](report_files/figure-gfm/unnamed-chunk-74-1.png)<!-- -->

Music across all three genres has become less acoustic since the 1950s.
Over the last 5 years, new rock music has been become sadder, less
dancable, and more acoustic. The 2000s have meant more words and an
increase in acoustic qualities for pop music. Country music continues to
be more upbeat and less acoustic since the “Country Class of ’89.”

I could simplify the number of traits included on the visualization and
stack them in my final version or focus on just one genre.

## References:

<https://stackoverflow.com/questions/40663586/streamgraph-package-in-r-will-run-the-data-but-no-graph-will-be-shown>
<https://www.quora.com/How-do-I-get-a-frequency-count-based-on-two-columns-variables-in-an-R-dataframe>
<https://stackoverflow.com/questions/10758961/how-to-convert-a-table-to-a-data-frame>
<https://www.datanovia.com/en/lessons/rename-data-frame-columns-in-r/>
<https://www.datanovia.com/en/lessons/subset-data-frame-rows-in-r/>
<https://rdrr.io/github/hrbrmstr/streamgraph/man/sg_axis_x.html>
<https://www.r-graph-gallery.com/158-change-color-in-interactive-streamgraph.html>
<https://stackoverflow.com/questions/3171426/compare-two-data-frames-to-find-the-rows-in-data-frame-1-that-are-not-present-in>
<https://community.rstudio.com/t/converting-dates-year-month-day-to-3-separate-columns-year-month-day/8585/3>
<https://stackoverflow.com/questions/35920408/creating-a-line-chart-in-r-for-the-average-value-of-groups>
<https://stackoverflow.com/questions/30375600/how-to-plot-multiple-lines-for-each-column-of-a-data-matrix-against-one-column>
<https://stackoverflow.com/questions/27082601/ggplot2-line-chart-gives-geom-path-each-group-consist-of-only-one-observation>
<https://stackoverflow.com/questions/15215457/standardize-data-columns-in-r>
<https://www.earthdatascience.org/courses/earth-analytics/time-series-data/summarize-time-series-by-month-in-r/>
