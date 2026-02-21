# spotify

A Shiny app to analyze and reorganize your Spotify playlists using k-means clustering.

## What it does

Select up to 2 of your playlists and the app will regroup their tracks into 2–4 new playlists based on musical features (danceability, energy, valence, tempo, etc.). Results can be downloaded as CSV.

## Setup

### 1. Spotify credentials

Go to [developer.spotify.com](https://developer.spotify.com/), create an app, and grab your `Client ID` and `Client Secret`. Under the app settings, add the following Redirect URI:

```
http://127.0.0.1:1410/
```

Then create a `.Renviron` file in the project root (use `.Renviron.example` as reference):

```
SPOTIFY_CLIENT_ID=your_client_id
SPOTIFY_CLIENT_SECRET=your_client_secret
SPOTIFY_CLIENT_REDIRECT_URI=http://127.0.0.1:1410/
```

Restart your R session after creating the file so the variables are loaded.

### 2. Dependencies

This project uses `renv` for reproducibility. After cloning, run in your R console:

```r
renv::restore()
```

If you install or update packages during development, run `renv::snapshot()` before committing.

### 3. Run the app

Open `spotify_app/app.R` and click **Run App** in RStudio/Positron, or run:

```r
shiny::runApp("spotify_app")
```

A browser window will open asking you to authorize your Spotify account.
