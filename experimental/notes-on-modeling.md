 http://www.glicko.net/research/acjpaper.pdf
 Bradley-Terry model: rarely perform substantially worse than avg; more likely 
                          to perform substantially better
                          "extreme value distribution"
                      Assume logistic distribution for diff between players
                      Assume distribution of performances is entirely characterized
                           by avg, no diff in spread between players
 Elo model: assume normal distribution for performance
            Assume normal distribution for diff between players
            Thurstone-Mosteller
 Diff between logistic and normal distribution makes little difference, but
 logistic is mathematically easier
 Glicko models change in skill over time

 Expected score for a game 
 10^(A/400) / [10^(A/400) + 10^(B/400)]
 however this empirically consistently overestimates the expected score
 eg score 1900 playing against score half time 1600, half time 1800, with 
    estimate 1700 -> 0.85 or 0.64 scores, avg 0.745 but value based on 1700 
    estimate is 0.76 so inflates estimate systematically 

 Updating estimates
 pre-game estimate + weight(observed_score - expected_score)
   weight controls how much to value new information compared to old
   weight ("K") = 32 gives 94.7% weight to pre-estimate and 5.3% to new
              K = 24 -> 96.2 and 3.8
              K = 16 -> 97.5 and 2.5

 Professional chess association uses elo-like system but the pre-score is based
 on the last 100 games, with games weighted by recency so 5th most recent game
 is 10X heavier than 50th. Also calculates variance for each player, how erratic
 their performance is

