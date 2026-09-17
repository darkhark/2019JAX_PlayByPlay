# 2019 Jacksonville Jaguars: Play-by-Play Analysis

Graduate project for STA 6704 (Data Mining II), University of Central Florida, summer 2020.
Solo work by Josh Harkness.

The question: where were the 2019 Jaguars strong, where were they weak, and which of their
tendencies could an opponent exploit? The answers were written for a front office and a
coaching staff, not for a statistics class: they end in draft priorities and coaching
recommendations.

**Deliverables:** [Write-Up.docx](Write-Up.docx) (methodology and findings),
[ExecutiveSummary.docx](ExecutiveSummary.docx) (one page for decision makers), and
[Presentation.pptx](Presentation.pptx) (23 slides). The course assignments that built up to
the project live in
[2019JAX_PlayByPlay_Assignments](https://github.com/darkhark/2019JAX_PlayByPlay_Assignments).

## Findings

| Passing offense | Passing defense | Rushing offense | Rushing defense |
|---|---|---|---|
| More success throwing deep | Weak against short passes to the middle | Scrambled too often on third down without much success | Weak on the edge, especially to the left |
| More first downs on the road | Weak against passes not from shotgun | More first downs on the road | Weak at the right tackle gap |
| Threw right far more often on the road | Weak against short-left passes on third down, especially at home | Weak on the edges | Good at stopping first downs in the guard gaps |
| | Very strong against short passes to the right | Strong at the tackle and guard gaps | |
| | Strong against deep outside passes | | |

Recommendations that followed: protect the quarterback (he was most effective throwing deep
and was running too often), so add offensive line in the draft; evaluate the linebacker
group, which showed up as the weak spot in both the run game (gap play) and the pass game
(short middle); and, since the team played better on the road, find ways to fill the
stadium. The Jaguars finished the 2019 season near the bottom of the league in rush defense,
which lines up with what the run-defense clusters showed.

## Data

- Source: 2019 regular-season play-by-play from
  [nflscrapR-data](https://github.com/ryurko/nflscrapR-data). `data/reg_pbp_2019.csv` is the
  raw file: 45,546 plays league-wide and 256 columns (the write-up and slides say 156; the
  file has 256).
- Knowledge-based reduction: probability columns (`ep`, `epa`, `wp`, `wpa`, and the rest),
  redundant clocks (quarter and half seconds remaining, in favor of game seconds remaining),
  score-state columns, timeout columns, and the play description were dropped using football
  judgment before any modeling. Each partitioned CSV ends up with 17 named columns, 16
  game-state features plus the engineered target, and a row index (the slides count 18).
- Every play was re-expressed from Jacksonville's perspective (home/away flag, touchdown team
  recoded to none/JAX/opponent, an offense/defense flag) so one pipeline served both sides
  of the ball. See `data/DataPartitioner.R`.
- Four datasets: pass offense (587 plays), pass defense (514), run offense (375), run defense
  (416). Divisional-only splits were built and then dropped because the samples were too small
  to be useful.
- Cleaning decisions: up-the-middle runs with a missing gap were imputed to center (the only
  gap not already labeled); sacks were removed from the passing analysis because they say
  nothing about pass location; columns that were entirely null within a split were dropped;
  safeties and fourth-down plays in the pass defense set were dropped at modeling time because
  they were too rare to carry information (which is why the normalized pass defense file has
  509 rows); categoricals were dummy-coded; each dataset was min-max normalized before each
  model because each method had its own requirements.
- A 0 to 4 "play quality" target was engineered from yards gained, field position, first
  down, and turnovers, with the scale inverted for defense.

## Methods

1. **Dimension reduction** (`*/DimensionReduction/`): PCA, t-SNE, and non-negative matrix
   factorization. PCA showed that several attributes measured the same dimension (the clock
   columns were the clearest case) and was the method carried forward into the write-up and
   slides; t-SNE and NMF were run and plotted but not used further. These scripts also write
   the dummy-coded, normalized datasets that the clustering step reads.
2. **Bayesian networks** (`*/BayesianAnalysis/`, `bnlearn`): interval and quantile
   discretization, then constraint-based (IAMB-FDR), score-based (hill climbing), hybrid
   (H2PC), and local-discovery (ARACNE) structure learning scored on AIC, with blacklists to
   block trivial arcs (quality class to quality class, gap to gap, down to down) and arc
   direction reversed by hand so nothing points out of play quality. The write-up reports that
   interval discretization with the constraint-based learner scored best on every dataset.
   The fitted networks were checked by predicting one node at a time on an 80/20 split (a
   quality class for the passing sets, a run location for the rushing sets). Accuracy came in
   between 70 and 80 percent, but that was mostly class balance: the networks caught the
   extreme classes (quality 0 and 4) and missed the middle ones, which is why the analysis
   moved to unsupervised methods.
3. **K-means clustering** (`*/ClusterAnalysis/`): k chosen by the elbow method, average
   silhouette width, and the gap statistic (4 clusters for three datasets, 5 for pass
   defense, where the silhouette was unclear and 5 matched the number of quality classes).
   Each dataset was also run at a larger k (8, 9, 8, and 6) and the smaller k gave the
   cleaner split every time. Clusters were checked with CLARA silhouette plots and cluster
   scatter plots (`useful::plot.kmeans`, `fpc::plotcluster`), then read as football, which is
   where the findings table comes from.

R packages: `dplyr`, `fastDummies`, `ggplot2`, `ggfortify`, `ggforce`, `factoextra`, `Rtsne`,
`NMF`, `bnlearn`, `Rgraphviz`, `graph`, `caTools`, `caret`, `cluster`, `useful`, `fpc`. The
Bayesian scripts call `bnlearn` and `caTools` functions without loading them, so attach both
before running.

## Repository layout

```
data/                  DataPartitioner.R, the raw play-by-play, the four partitioned datasets,
                       and the four normalized datasets the dimension-reduction scripts write
Pass/                  DimensionReduction, BayesianAnalysis, ClusterAnalysis scripts and their
                       PNG output for the passing game
Run/                   the same three stages for the running game
Write-Up.docx          methodology and results
ExecutiveSummary.docx  one page of findings and recommendations
Presentation.pptx      23 slides
```

`Pass/BayesianAnalysis/` also holds stray copies of `jaxRunOffense.csv` and `jaxRunDefense.csv`
from an earlier cut of the partitioner (no `Quality` column); no script reads them.

Run `data/DataPartitioner.R` first from inside `data/` (it writes the partitioned CSVs
there), then each side's `DimensionReduction`, `BayesianAnalysis`, and `ClusterAnalysis`
scripts from inside their own folders; they read and write with paths relative to that
folder (`../../data/`). `ClusterAnalysis` depends on the normalized files that
`DimensionReduction` writes, so keep that order. `BayesianAnalysis` reads the partitioned
CSVs directly.

## How I would improve it today

Written in 2026, six years and a lot of football modeling later. If I rebuilt this
project now:

- **Use EPA.** The nflscrapR file already carried `ep`, `epa`, `wp`, and `wpa`. The
  partitioner's first step keeps columns 3 through 58 under the comment "Pick groupings that
  may be useful (ex. probabilities won't be)", which threw all of them away, and I hand-built
  a 0 to 4 play-quality score instead. My later NFL work is built on EPA, success rate, and
  win-probability context from the start.
- **Benchmark against the league.** Every finding was judged within the Jaguars' own data.
  Running the same pipeline on all 32 teams would have given each strength and weakness a
  league rank, and would have separated real weaknesses from league-wide base rates.
- **Put uncertainty on the cuts.** Some findings rest on a dozen plays. Bootstrap intervals,
  or a hierarchical model that pools each gap or pass location toward the league mean, would
  say which findings deserve belief.
- **Test whether tendencies persist.** One season, never checked for stability across halves
  of the year or against 2018. A derived metric is only signal if it predicts itself.
- **Skip the clustering.** The run-defense clusters mostly recovered the `run_gap` column.
  A grouped success rate by gap says the same thing with less machinery, and k-means on
  one-hot dummies is a poor fit anyway.
- **Check for leakage.** `yards_gained` and `first_down_pass` sit next to `Quality` in the
  partitioned CSVs, and `Quality` is computed from them. The Bayesian scripts did drop those
  two before fitting, but they kept `td_team`, `interception`, and `incomplete_pass`, which
  feed the same rule (a touchdown or an interception sets the class outright). That is the
  likely reason the networks nailed the extreme classes and missed the middle ones. The
  clustering kept `yards_gained` and the first-down flags as features, which is fine for
  describing plays but means the clusters partly sort by outcome rather than by situation.
- **Attribute plays to players.** The slides' own "future improvements" said it: personnel,
  injuries, first-string only. That data is available now (participation data, charting,
  tracking); it was not in 2020.
