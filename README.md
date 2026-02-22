# SpotiApp

A local Shiny app to analyze and reorganize your Spotify playlists using clustering.

## What it does

**Clustering tab**
- Select up to 2 of your playlists
- Run k-means or DBSCAN clustering on their tracks using artist and track features (popularity, duration, explicitness, release year, artist popularity, genre tags)
- Browse results by cluster, see a feature comparison chart, download clusters as CSV, or save them directly as new private playlists in your Spotify account

**Backup tab**
- Export any of your playlists as CSV
- Or download all of them at once as a ZIP (one CSV per playlist)

## Tech stack

| Package | Role | Why |
|---|---|---|
| `shiny` | Web framework | Reactive UI in pure R, no separate backend needed |
| `httr` | Spotify API + OAuth2 | Direct API access; `spotifyr`'s audio features endpoint returns 403 on new apps, so we call `/v1/artists` ourselves |
| `spotifyr` | Auth helpers only | `get_my_playlists()` and `get_my_profile()` — kept for convenience |
| `DT` | Interactive tables | Sortable, searchable tables with zero config |
| `plotly` | Lollipop chart | Interactive via `ggplotly()`, tooltip shows real values |
| `dbscan` | DBSCAN clustering | Density-based alternative to k-means; doesn't require choosing k, isolates noise tracks |
| `zip` | Multi-file export | Bundles multiple CSVs into a ZIP for the full backup |
| `renv` | Dependency management | Reproducible installs for anyone who clones the repo |

**Key decisions:**
- **No audio features endpoint**: Spotify deprecated `/v1/audio-features` (403 on apps created after ~2024). Features are derived from `/v1/artists` (popularity, genres) and track metadata (duration, explicit, release year).
- **Local only**: OAuth via `httr` works fine locally. No deployment needed.
- **k-means + DBSCAN**: k-means is the simple baseline (you choose k). DBSCAN is offered as an exploratory alternative when you don't know how many groups exist — tracks that don't fit any cluster are labeled "Unclassified" rather than forced into a group.

## Setup

### 1. Spotify credentials

Go to [developer.spotify.com](https://developer.spotify.com/dashboard), create an app, and copy the **Client ID** and **Client Secret**. Under the app settings, add this Redirect URI:

```
http://127.0.0.1:1410/
```

Create a `.Renviron` file in the project root (use `.Renviron.example` as reference):

```
SPOTIFY_CLIENT_ID=your_client_id
SPOTIFY_CLIENT_SECRET=your_client_secret
SPOTIFY_CLIENT_REDIRECT_URI=http://127.0.0.1:1410/
```

Restart your R session after saving.

### 2. Install dependencies

```r
renv::restore()
```

### 3. Run the app

```r
shiny::runApp("spotify_app")
```

Or open `spotify_app/app.R` in RStudio/Positron and click **Run App**.

The app opens at `http://127.0.0.1:3838`. A browser window will prompt you to authorize your Spotify account.
