# SpotiApp

A local Shiny app to analyze and reorganize your Spotify playlists using clustering techniques

## What it does

**Clustering tab**
- Select up to 2 of your playlists
- Run k-means or DBSCAN clustering on their tracks using artist and track features (popularity, duration, explicitness, release year, artist popularity, genre tags)
- Browse results by cluster, see a feature comparison chart, download clusters as CSV, or save them directly as new private playlists in your Spotify account

**Backup tab**
- Export any of your playlists as CSV
- Or download all of them at once as a ZIP (one CSV per playlist)

**Key decisions:**
- **No audio features endpoint**: Spotify deprecated `/v1/audio-features` (403 on apps created after ~2024). Features are derived from `/v1/artists` (popularity, genres) and track metadata (duration, explicit, release year).

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

The app opens at `http://127.0.0.1:3838`. A browser window will prompt you to authorize your Spotify account.
