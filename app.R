# Bird signal map: a password-protected live map. Signals are typed into a PRIVATE Google Sheet,
# never into this app. The sheet is read with a Google "service account" key kept in private
# settings (environment variables), never in the code.
#
# Settings (environment variables; on Connect Cloud add them under "Variables"):
#   SHEET_ID         the long code in your Google Sheet's address
#   GOOGLE_KEY_JSON  the full text of the service account key file (.json)
# If either is missing the app runs in DEMO MODE with the example entries.csv and password "demo".
#
# The sheet needs two tabs:
#   Signals   columns (row 1 exactly):  time  bird  station  bearing  strength  lat  lon
#   Settings  cell A1 = password, cell B1 = today's password
# Signals rows:
#   27/10/2026 07:45  13  Mt Sugarloaf  297  strong
#   27/10/2026 09:30  13                                  -32.81  151.42   <- found (lat/lon, nothing else)
#   27/10/2026 10:00  7                  120  weak        -32.85  151.40   <- bearing from a new spot
# strength: strong = 3 km line, medium = 5 km, weak or blank = 15 km (never longer than 15 km).
#
# Rules: each bird is drawn from its newest day only; the latest bearing per station is used;
# 2+ crossing bearings are triangulated; a found pin is replaced by any later bearing that day;
# opacity fades with time since the last signal; lines from earlier days show only if ticked.
# Changing the password in the sheet signs everyone out within about 20 seconds.

library(shiny)
library(leaflet)
library(DT)
library(googlesheets4)

SHEET_ID <- Sys.getenv("SHEET_ID")
KEY_JSON <- Sys.getenv("GOOGLE_KEY_JSON")
LIVE     <- nzchar(SHEET_ID) && nzchar(KEY_JSON)
DEMO_PASSWORD <- "demo"
CACHE_SEC     <- 8                       # all visitors share one read of the sheet every 8 seconds
MAX_TRIES     <- 5                       # wrong passwords allowed per visit
if (LIVE) {
  key_file <- tempfile(fileext = ".json")
  writeLines(KEY_JSON, key_file); Sys.chmod(key_file, "600")
  LIVE <- tryCatch({
    googlesheets4::gs4_auth(path = key_file, scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")
    TRUE
  }, error = function(e) { message("Google sign-in failed"); FALSE })
}
cache <- new.env()
cached <- function(name, fn) {
  now <- as.numeric(Sys.time()); hit <- cache[[name]]
  if (!is.null(hit) && now - hit$t < CACHE_SEC) return(hit$v)
  v <- tryCatch(fn(), error = function(e) { message("Sheet read failed: ", conditionMessage(e)); NULL })
  if (is.null(v) && !is.null(hit)) v <- hit$v        # keep the last good copy if a read fails
  cache[[name]] <- list(t = now, v = v)
  v
}
read_tab <- function(tab, ...) as.data.frame(googlesheets4::read_sheet(SHEET_ID, sheet = tab, col_types = "c", ...),
                                             stringsAsFactors = FALSE)
get_signals <- function() {
  if (!LIVE) return(list(d = read.csv("entries.csv", stringsAsFactors = FALSE, colClasses = "character"), note = NULL))
  d <- cached("signals", function() read_tab("Signals"))
  if (is.null(d)) list(d = NULL, note = "Can't read the signal sheet right now.") else list(d = d, note = NULL)
}
get_teams <- function() {                            # optional "Teams" tab: time, team, lat, lon
  if (!LIVE) return(if (file.exists("teams.csv")) read.csv("teams.csv", stringsAsFactors = FALSE, colClasses = "character") else NULL)
  cached("teams", function() read_tab("Teams"))
}
read_teams <- function(stations) {                  # one row per team: team + (lat, lon or a high point name)
  d <- get_teams()
  if (is.null(d) || !nrow(d)) return(NULL)
  names(d) <- tidy_names(names(d))
  if (!"team" %in% names(d)) return(NULL)
  if (!"highpoint" %in% names(d)) d$highpoint <- if ("station" %in% names(d)) d$station else NA_character_
  for (n in c("lat", "lon")) if (!n %in% names(d)) d[[n]] <- NA_character_
  num <- function(x) suppressWarnings(as.numeric(trimws(as.character(x))))
  clean <- function(x) { x <- trimws(as.character(x)); x[is.na(x)] <- ""; x }
  norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  t <- data.frame(row = seq_len(nrow(d)) + 1, team = clean(d$team), hp = clean(d$highpoint),
                  lat = num(d$lat), lon = num(d$lon), stringsAsFactors = FALSE)
  t <- t[nzchar(t$team) | nzchar(t$hp) | !is.na(t$lat) | !is.na(t$lon), , drop = FALSE]
  if (!nrow(t)) return(NULL)
  idx <- match(norm(t$hp), norm(stations$name))
  use_ll <- !is.na(t$lat) & !is.na(t$lon) & abs(t$lat) <= 90 & abs(t$lon) <= 180   # lat/lon wins if both are filled
  use_hp <- !use_ll & !is.na(idx)
  t$lat[use_hp] <- stations$lat[idx[use_hp]]; t$lon[use_hp] <- stations$lon[idx[use_hp]]
  t$area <- use_ll                                       # lat/lon = a general area (circle); high point = a dot
  ok <- nzchar(t$team) & (use_ll | use_hp)
  good <- t[ok, , drop = FALSE]
  good <- good[!duplicated(tolower(good$team), fromLast = TRUE), ]
  list(df = good[, c("team", "lat", "lon", "area")], ignored = t$row[!ok])
}
get_others <- function() {                           # optional "non-tx birds" tab: date, lat, lon, bands
  if (!LIVE) return(if (file.exists("others.csv")) read.csv("others.csv", stringsAsFactors = FALSE, colClasses = "character") else NULL)
  cached("others", function() read_tab("non-tx birds"))
}
read_others <- function() {                          # untracked birds: one row per sighting
  d <- get_others()
  if (is.null(d)) return(list(df = NULL, note = "Non-tx birds: can't read the tab (is it named exactly 'non-tx birds'?)."))
  if (!nrow(d)) return(list(df = NULL, note = NULL))
  names(d) <- tidy_names(names(d))
  names(d)[names(d) %in% c("colour bands", "color bands")] <- "bands"
  if (!all(c("date", "lat", "lon") %in% names(d)))
    return(list(df = NULL, note = paste0("Non-tx birds: row 1 needs the headings date, lat and lon (found: ", paste(names(d), collapse = ", "), ").")))
  if (!"bands" %in% names(d)) d$bands <- ""
  num <- function(x) suppressWarnings(as.numeric(trimws(as.character(x))))
  o <- data.frame(row = seq_len(nrow(d)) + 1, raw = trimws(as.character(d$date)), time = parse_time(d$date),
                  lat = num(d$lat), lon = num(d$lon),
                  bands = trimws(ifelse(is.na(d$bands), "", as.character(d$bands))), stringsAsFactors = FALSE)
  o <- o[!(is.na(o$raw) | o$raw == "") | !is.na(o$lat) | !is.na(o$lon), , drop = FALSE]      # skip fully blank rows
  ok <- !is.na(o$time) & !is.na(o$lat) & !is.na(o$lon) & abs(o$lat) <= 90 & abs(o$lon) <= 180
  why <- ifelse(is.na(o$time), paste0("date '", o$raw, "' unreadable"), "lat/lon")
  list(df = o[ok, c("time", "lat", "lon", "bands"), drop = FALSE], note = NULL,
       ignored_why = paste0(o$row[!ok], " (", why[!ok], ")"), n = sum(ok))
}
# "Stay signed in": after a correct password the phone keeps a code that is good until 4am the next morning (Sydney).
# The code can't be made without the password and the Google key, and it stops working if the password is changed.
make_token <- function(pw) {
  f <- tempfile(); on.exit(unlink(f))
  writeLines(paste(pw, KEY_JSON, SHEET_ID, format(Sys.time() - 4 * 3600, "%Y-%m-%d", tz = TZ)), f)
  unname(tools::md5sum(f))
}
get_password <- function() {                          # NULL means "no usable password": nobody gets in
  if (!LIVE) return(DEMO_PASSWORD)
  s <- cached("settings", function() read_tab("Settings", col_names = FALSE))
  if (is.null(s) || ncol(s) < 2) return(NULL)
  i <- which(tolower(trimws(s[[1]])) == "password")
  if (!length(i)) return(NULL)
  pw <- trimws(as.character(s[[2]][i[1]]))
  if (is.na(pw) || !nzchar(pw)) NULL else pw
}
STATION_KMZ <- "highpoints.kmz"        # optional alternative
STATION_CSV <- "highpoints.csv"        # columns: name, lon, lat (one row per high point)
FADE_DAYS   <- 7                      # signal this old is drawn at its faintest
MAP_HEIGHT <- "65vh"      # height of the map (percent of the screen height; smaller = shorter map)
SAT_OPACITY <- 1     # satellite image strength: 1 = full colour (lower = paler)
RUNNER_DAYS <- 7                      # no signal for this many days = "runner" (listed at the bottom)
LINE_KM     <- c(strong = 3, medium = 5, weak = 15)   # drawn line length (km) by signal strength
MAX_KM      <- 15                                      # nothing is drawn longer than this
line_km <- function(strength) {                        # no strength given = weak = 15 km
  k <- LINE_KM[tolower(trimws(as.character(strength)))]
  k[is.na(k)] <- LINE_KM[["weak"]]
  pmin(unname(k), MAX_KM)
}
REFRESH_SEC <- 10
TRAIL_N <- 5          # how many recent positions the bird trail shows
TEAM_AREA_M <- 500    # a team given by lat/lon is drawn as a circle of this radius (metres)
JITTER_PX  <- 16     # sideways gap (screen pixels) between birds' lines that would otherwise overlap
JITTER_DEG <- 3      # lines from the same spot within this many degrees count as overlapping
HIDE_LINES_WHEN_LOCATED <- FALSE  # once a bird has a triangulated point, show just the point (set FALSE to keep its lines too)
# High-contrast colours, handed out automatically to the birds that are currently being tracked.
# light, bright colours that stand out on the dark satellite image. No yellow, orange or gold: gold is for found birds.
BIRD_COLS <- c("#ffffff", "#ff5cc8", "#00f0ff", "#7dff3b", "#ff6b6b", "#c8a2ff", "#5b9dff", "#2ee6c0", "#c4c4c4", "#9a7bff")
TZ          <- "Australia/Sydney"
Sys.setenv(TZ = TZ)
M_PER_DEG   <- 111320

# ---------------------------------------------------------------- geometry
to_xy <- function(lon, lat, lon0, lat0) cbind((lon - lon0) * M_PER_DEG * cos(lat0 * pi / 180), (lat - lat0) * M_PER_DEG)
from_xy <- function(x, y, lon0, lat0) cbind(lon0 + x / (M_PER_DEG * cos(lat0 * pi / 180)), lat0 + y / M_PER_DEG)
end_point <- function(lon, lat, brg, km) {
  r <- from_xy(sin(brg * pi / 180) * km * 1000, cos(brg * pi / 180) * km * 1000, lon, lat)
  c(lon = r[1], lat = r[2])
}
# Drawn length (km) of each of one bird's lines: its signal-strength length, unless it crosses another of the bird's
# lines (both in the forward direction, within MAX_KM). Then it runs to its LAST such crossing, so every crossing
# is drawn on both lines (stopping at the nearest one could leave a gap when a bird has 3+ lines).
line_lengths <- function(L) {
  n <- nrow(L)
  len <- line_km(L$strength)
  if (n < 2) return(len)
  lon0 <- mean(L$lon); lat0 <- mean(L$lat)
  P <- to_xy(L$lon, L$lat, lon0, lat0)
  th <- L$bearing * pi / 180
  D <- cbind(sin(th), cos(th))
  x <- function(u, v) u[1] * v[2] - u[2] * v[1]
  best <- rep(-Inf, n)
  for (i in 1:n) for (j in 1:n) if (i != j) {
    cr <- x(D[i, ], D[j, ])
    if (abs(cr) <= sin(5 * pi / 180)) next            # parallel or nearly so: never meets
    w <- P[j, ] - P[i, ]
    a <- x(w, D[j, ]) / cr                              # metres along line i
    b <- x(w, D[i, ]) / cr                              # metres along line j
    if (a > 0 && b > 0 && a / 1000 <= MAX_KM) best[i] <- max(best[i], a / 1000)
  }
  ifelse(is.finite(best), best, len)
}
# Least-squares crossing of bearing lines (exact for two). NULL if near-parallel or behind a station.
triangulate <- function(lon, lat, brg) {
  n <- length(lon)
  if (n < 2) return(NULL)
  th <- brg * pi / 180
  crossing <- FALSE
  for (i in 1:(n - 1)) for (j in (i + 1):n) if (abs(sin(th[i] - th[j])) > sin(5 * pi / 180)) crossing <- TRUE
  if (!crossing) return(NULL)
  lon0 <- mean(lon); lat0 <- mean(lat)
  P <- to_xy(lon, lat, lon0, lat0)
  D <- cbind(sin(th), cos(th))
  A <- matrix(0, 2, 2); b <- c(0, 0)
  for (i in 1:n) {
    M <- diag(2) - D[i, ] %o% D[i, ]
    A <- A + M
    b <- b + as.vector(M %*% P[i, ])
  }
  pt <- tryCatch(solve(A, b), error = function(e) NULL)
  if (is.null(pt)) return(NULL)
  if (!all(vapply(1:n, function(i) sum((pt - P[i, ]) * D[i, ]) > 0, logical(1)))) return(NULL)
  miss <- vapply(1:n, function(i) { v <- pt - P[i, ]; abs(v[1] * D[i, 2] - v[2] * D[i, 1]) }, numeric(1))
  ll <- from_xy(pt[1], pt[2], lon0, lat0)
  list(lon = ll[1], lat = ll[2], miss_m = sqrt(mean(miss^2)))
}

# ---------------------------------------------------------------- stations
placeholder_stations <- function() {   # placeholder positions near Kurri Kurri; replaced by highpoints.csv
  p <- data.frame(name = c("Mt Tumb", "Hewsons", "Pinnacles", "Mt Sugar"),
                  x = c(291, 412, 175, 987), y = c(446, 665, 891, 837))
  data.frame(name = p$name, lon = 151.48 + (p$x - 585) * 0.000268, lat = -32.82 - (p$y - 613) * 0.000225,
             stringsAsFactors = FALSE)
}
load_stations <- function() {
  if (file.exists(STATION_KMZ) && requireNamespace("sf", quietly = TRUE)) {
    out <- tryCatch({
      dir <- tempfile(); dir.create(dir)
      utils::unzip(STATION_KMZ, exdir = dir)
      kml <- list.files(dir, pattern = "\\.kml$", full.names = TRUE, recursive = TRUE)[1]
      pts <- do.call(rbind, lapply(sf::st_layers(kml)$name, function(l) {
        x <- sf::st_read(kml, layer = l, quiet = TRUE)
        x <- x[as.character(sf::st_geometry_type(x)) == "POINT", ]
        if (!nrow(x)) return(NULL)
        xy <- sf::st_coordinates(sf::st_zm(x))
        data.frame(name = as.character(x$Name), lon = xy[, 1], lat = xy[, 2], stringsAsFactors = FALSE)
      }))
      if (is.null(pts) || !nrow(pts)) NULL else list(df = pts, source = "highpoints.kmz")
    }, error = function(e) NULL)
    if (!is.null(out)) return(out)
  }
  if (file.exists(STATION_CSV)) {
    out <- tryCatch({
      d <- read.csv(STATION_CSV, stringsAsFactors = FALSE)
      list(df = d[, c("name", "lon", "lat")], source = "highpoints.csv")
    }, error = function(e) NULL)
    if (!is.null(out)) return(out)
  }
  list(df = placeholder_stations(), source = "PLACEHOLDER positions (no highpoints.csv found)")
}

# ---------------------------------------------------------------- reading the sheet
# column headings are matched ignoring capitals and any note in brackets; long / lng / longitude all mean lon
tidy_names <- function(nm) {
  nm <- tolower(trimws(sub("\\(.*$", "", nm)))
  nm[nm %in% c("long", "lng", "longitude")] <- "lon"
  nm[nm == "latitude"] <- "lat"
  nm
}
parse_time <- function(x) {
  x <- trimws(as.character(x))
  out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = TZ)
  serial <- grepl("^[0-9]{5}(\\.[0-9]+)?$", x)                     # a spreadsheet date number, e.g. 46323.53
  if (any(serial)) {
    u <- as.POSIXct(as.numeric(x[serial]) * 86400, origin = "1899-12-30", tz = "UTC")
    out[serial] <- as.POSIXct(format(u, "%Y-%m-%d %H:%M:%S"), tz = TZ)
    x[serial] <- NA_character_
  }
  x <- sub("(Z|[+-][0-9]{2}:?[0-9]{2})$", "", x)                     # drop a trailing time-zone marker
  for (f in c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M",
              "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M", "%d/%m/%Y %I:%M:%S %p", "%d/%m/%Y %I:%M %p",
              "%d/%m/%y %H:%M", "%d/%m/%Y", "%Y-%m-%d")) {
    miss <- is.na(out)
    if (!any(miss)) break
    out[miss] <- as.POSIXct(x[miss], format = f, tz = TZ)
  }
  out
}
read_entries <- function(stations) {
  raw <- get_signals(); d <- raw$d
  if (is.null(d)) return(list(df = NULL, ignored = integer(), note = raw$note))
  names(d) <- tidy_names(names(d))                          # "time (autofills)" counts as "time"
  d[] <- lapply(d, function(x) trimws(as.character(x)))
  if (nrow(d) == 0)
    return(list(df = data.frame(time = as.POSIXct(character(), tz = TZ), bird = integer(), type = character(),
                                station = character(), lon = numeric(), lat = numeric(), bearing = numeric(),
                                strength = character(), stringsAsFactors = FALSE), ignored = integer(), note = NULL))
  for (n in setdiff(c("time", "bird", "station", "bearing", "strength", "lat", "lon"), names(d))) d[[n]] <- NA_character_
  d$row <- seq_len(nrow(d)) + 1                      # row number as seen in the spreadsheet
  cols <- c("time", "bird", "station", "bearing", "lat", "lon")
  blank_row <- apply(d[, cols, drop = FALSE], 1, function(r) all(is.na(r) | !nzchar(trimws(r))))
  d <- d[!blank_row, , drop = FALSE]
  num <- function(x) suppressWarnings(as.numeric(x))
  st_raw <- d$station; st_raw[is.na(st_raw)] <- ""
  norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))   # ignore capitals, spaces, apostrophes, dashes
  idx <- match(norm(st_raw), norm(stations$name))
  e <- data.frame(row = d$row, time = parse_time(d$time), bird = as.integer(num(d$bird)),
                  station = stations$name[idx], has_station = nzchar(st_raw), station_ok = !is.na(idx),
                  bearing = num(d$bearing), strength = ifelse(is.na(d$strength), "", tolower(trimws(d$strength))),
                  lat = num(d$lat), lon = num(d$lon), stringsAsFactors = FALSE)
  has_pos <- !is.na(e$lat) & !is.na(e$lon); has_b <- !is.na(e$bearing)
  e$type <- ifelse(has_b & e$station_ok, "station",
            ifelse(has_b & !e$has_station & has_pos, "point",
            ifelse(!has_b & !e$has_station & has_pos, "found", NA)))
  ok <- !is.na(e$type) & !is.na(e$time) & !is.na(e$bird) & e$bird >= 1 &
        (is.na(e$bearing) | (e$bearing >= 0 & e$bearing <= 360))
  why <- ifelse(is.na(e$time), paste0("time '", d$time[match(e$row, d$row)], "' unreadable"),
         ifelse(is.na(e$bird) | e$bird < 1, "bird number",
         ifelse(!is.na(e$bearing) & (e$bearing < 0 | e$bearing > 360), "bearing",
         ifelse(e$has_station & !e$station_ok, paste0("station '", st_raw[match(e$row, d$row)], "' not in list"), "station/bearing/lat/lon"))))
  list(df = e[ok, c("time", "bird", "type", "station", "lon", "lat", "bearing", "strength")],
       ignored = e$row[!ok], ignored_why = paste0(e$row[!ok], " (", why[!ok], ")"), note = NULL)
}

# ---------------------------------------------------------------- bird colours
# A bird takes the lowest free colour when it starts being tracked (its first signal, or its first signal after
# RUNNER_DAYS or more of silence) and keeps it while it stays active. RUNNER_DAYS after its last signal the colour is
# freed for the next new bird. Worked out from the sheet's history alone, so every phone sees the same colours.
assign_colours <- function(e, cols = BIRD_COLS) {
  if (is.null(e) || !nrow(e)) return(character())
  gap <- RUNNER_DAYS * 86400
  ev <- list(); last_seg <- list()
  for (b in unique(e$bird)) {
    t <- sort(as.numeric(e$time[e$bird == b]))
    sid <- cumsum(c(TRUE, diff(t) >= gap))
    for (k in unique(sid)) {
      tk <- t[sid == k]
      ev[[length(ev) + 1]] <- data.frame(t = min(tk), type = 1L, id = paste(b, k), stringsAsFactors = FALSE)
      ev[[length(ev) + 1]] <- data.frame(t = max(tk) + gap, type = 0L, id = paste(b, k), stringsAsFactors = FALSE)
    }
    last_seg[[as.character(b)]] <- paste(b, max(sid))
  }
  ev <- do.call(rbind, ev)
  ev <- ev[order(ev$t, ev$type), ]                       # at the same moment, colours are freed before they are handed out
  used <- rep(0L, length(cols)); seg_col <- list()
  for (i in seq_len(nrow(ev))) {
    if (ev$type[i] == 1L) { c1 <- which.min(used); used[c1] <- used[c1] + 1L; seg_col[[ev$id[i]]] <- c1 }
    else { c1 <- seg_col[[ev$id[i]]]; used[c1] <- used[c1] - 1L }
  }
  out <- vapply(last_seg, function(id) cols[seg_col[[id]]], "")
  names(out) <- names(last_seg)
  out
}

# ---------------------------------------------------------------- per-bird state
bird_state <- function(b, e, stations) {
  if (nrow(e) == 0) return(list(bird = b, never = TRUE))
  e$date <- as.Date(e$time, tz = TZ)
  d <- max(e$date)
  e <- e[e$date == d, , drop = FALSE]
  e <- e[order(e$time), , drop = FALSE]
  last_time <- max(e$time)
  fnd <- which(e$type == "found")
  if (length(fnd)) {
    lf <- max(fnd)
    if (lf == nrow(e))
      return(list(bird = b, never = FALSE, date = d, last_time = last_time, status = "Found",
                  point = c(e$lon[lf], e$lat[lf]), lines = NULL, detail = "confirmed location"))
    e <- e[(lf + 1):nrow(e), , drop = FALSE]        # bird moved after being found
  }
  idx <- match(e$station, stations$name)
  is_st <- e$type == "station"
  e$lon <- ifelse(is_st, stations$lon[idx], e$lon)
  e$lat <- ifelse(is_st, stations$lat[idx], e$lat)
  e <- e[!is.na(e$lon) & !is.na(e$lat), , drop = FALSE]
  if (nrow(e) == 0) return(list(bird = b, never = TRUE))
  e$key <- ifelse(e$type == "station", e$station, paste0("point ", round(e$lat, 4), ",", round(e$lon, 4)))
  e <- e[!duplicated(e$key, fromLast = TRUE), , drop = FALSE]   # latest per station / point
  n <- nrow(e)
  tri <- triangulate(e$lon, e$lat, e$bearing)
  if (n == 1) { status <- "Single bearing"; point <- NULL; detail <- "" }
  else if (!is.null(tri)) { status <- paste0("Triangulated (", n, ")"); point <- c(tri$lon, tri$lat)
                            detail <- sprintf("lines miss by ~%d m", round(tri$miss_m)) }
  else { status <- paste0("Bearings don't cross (", n, ")"); point <- NULL; detail <- "" }
  list(bird = b, never = FALSE, date = d, last_time = last_time, status = status, point = point,
       lines = if (!is.null(point) && HIDE_LINES_WHEN_LOCATED) NULL else e[, c("lon", "lat", "bearing", "time", "key", "strength")],
       detail = detail)
}

# ---------------------------------------------------------------- recent positions of one bird
# One position per sighting: every "found" row, plus, for each day, the point where that day's bearings cross
# (using the bearings after the day's last found row). The newest TRAIL_N are returned, newest first.
bird_trail <- function(b, e, stations, n = TRAIL_N) {
  if (is.null(e) || !nrow(e)) return(NULL)
  e <- e[order(e$time), , drop = FALSE]; e$date <- as.Date(e$time, tz = TZ)
  pos <- list()
  f <- e[e$type == "found", , drop = FALSE]
  for (i in seq_len(nrow(f)))
    pos[[length(pos) + 1]] <- data.frame(time = f$time[i], lon = f$lon[i], lat = f$lat[i], kind = "Found", stringsAsFactors = FALSE)
  for (d in unique(e$date)) {
    ed <- e[e$date == d, , drop = FALSE]
    fi <- which(ed$type == "found")
    rest <- if (length(fi)) ed[-seq_len(max(fi)), , drop = FALSE] else ed
    rest <- rest[rest$type != "found", , drop = FALSE]
    if (nrow(rest) < 2) next
    st <- bird_state(b, rest, stations)
    if (!isTRUE(st$never) && !is.null(st$point) && grepl("^Triangulated", st$status))
      pos[[length(pos) + 1]] <- data.frame(time = st$last_time, lon = st$point[1], lat = st$point[2],
                                           kind = paste0("Estimated from bearings: ", tolower(st$status)), stringsAsFactors = FALSE)
  }
  if (!length(pos)) return(NULL)
  df <- do.call(rbind, pos)
  df <- df[order(df$time, decreasing = TRUE), , drop = FALSE]
  head(df, n)
}

# ---------------------------------------------------------------- UI
main_ui <- function() fluidPage(
  title = "REHO live tracking",
  tags$style(HTML(".zoomed-out .station-label, .no-names .station-label { display: none; } .found-star, .chev { background: none; border: none; } .leaflet-container { background: #ffffff; } .map-full { position: fixed !important; top: 0; left: 0; width: 100vw !important; height: 100vh !important; height: 100dvh !important; z-index: 10000; } .full-btn { background: #fff; color: #222; font: bold 12px sans-serif; padding: 5px 9px; border: 2px solid rgba(0,0,0,0.35); border-radius: 6px; cursor: pointer; }")),
  h2("REHO live tracking", style = "margin: 10px 0 8px 0;"),
  leafletOutput("map", height = MAP_HEIGHT),
  if (!LIVE) div(class = "alert alert-warning", "DEMO MODE: example signals only. Set SHEET_ID and GOOGLE_KEY_JSON for real data."),
  div(style = "display: flex; flex-wrap: wrap; align-items: center; column-gap: 22px;",     # all tick boxes on one line
      checkboxInput("asof_on", "View from EOD yesterday", FALSE),
      checkboxInput("show_others", "Non-tx birds", FALSE),
      checkboxInput("show_labels", "Show high point names", TRUE)),
  h5("Birds (tick one or more birds to see their last five days; untick to clear)"),
  DTOutput("tbl"),
  div(style = "margin: 14px 0 24px 0; color: #666; font-size: 13px;", textOutput("status_note"))     # rows read / sheet read time
)
gate_ui <- function(msg = NULL) fluidPage(
  div(style = "max-width: 340px; margin: 18vh auto 0 auto;",
      h3("REHO live tracking"),
      passwordInput("pw", "Password", width = "100%"),
      tags$button(id = "login", type = "button", class = "btn btn-primary", "Open map"),
      if (!is.null(msg)) p(style = "margin-top: 12px; color: #b3261e;", msg))
)
ui <- fluidPage(
  tags$head(tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
            tags$script(HTML("
              function tryLogin(){ Shiny.setInputValue('login_try', {pw: $('#pw').val(), n: Math.random()}, {priority: 'event'}); }
              $(document).on('click', '#login', tryLogin);
              $(document).on('keydown', '#pw', function(e){ if (e.key === 'Enter') { e.preventDefault(); tryLogin(); } });
              window.rehoSel = [];
              window.rehoSync = function(a){ a = a || []; $('.bird-tick').each(function(){ this.checked = a.indexOf(Number($(this).data('bird'))) >= 0; }); };
              $(document).on('change', '.bird-tick', function(){
                var a = $('.bird-tick:checked').map(function(){ return Number($(this).data('bird')); }).get();
                if (window.rehoSelect) window.rehoSelect(a); else window.rehoSync([]);
              });
              $(document).on('shiny:connected', function(){
                Shiny.addCustomMessageHandler('save_token', function(t){ try { localStorage.setItem('reho_tok', t); } catch(e) {} });
                Shiny.addCustomMessageHandler('hp_labels', function(on){ var m = document.getElementById('map'); if (m) m.classList.toggle('no-names', !on); });
                try { var t = localStorage.getItem('reho_tok'); if (t) Shiny.setInputValue('saved_token', t, {priority: 'event'}); } catch(e) {}
              });"))),
  uiOutput("page")
)

# ---------------------------------------------------------------- server
server <- function(input, output, session) {
  st_info  <- load_stations()
  stations <- st_info$df

  tick <- reactiveTimer(REFRESH_SEC * 1000)

  # ---- password gate: the sheet is only read, and nothing is sent to the browser, after a correct password
  authed <- reactiveVal(FALSE); typed <- reactiveVal(NULL); fails <- reactiveVal(0); gate_msg <- reactiveVal(NULL)
  map_ready <- reactiveVal(FALSE)
  output$page <- renderUI(if (authed()) main_ui() else gate_ui(gate_msg()))
  observeEvent(input$show_labels, session$sendCustomMessage("hp_labels", isTRUE(input$show_labels)))
  observeEvent(input$saved_token, {                    # a phone that signed in earlier today comes back without the password
    tk <- input$saved_token
    if (isTRUE(authed()) || !is.character(tk) || !nzchar(tk)) return()
    pw <- get_password()
    if (!is.null(pw) && identical(tk, make_token(pw))) { typed(pw); gate_msg(NULL); fails(0); map_ready(FALSE); authed(TRUE) }
  })
  observeEvent(input$login_try, {
    if (fails() >= MAX_TRIES) { gate_msg("Too many wrong tries. Reload the page to try again."); return() }
    pw <- get_password()
    if (is.null(pw)) { gate_msg("The map is unavailable right now."); return() }
    if (identical(trimws(input$login_try$pw), pw)) { typed(pw); gate_msg(NULL); fails(0); map_ready(FALSE); authed(TRUE)
      session$sendCustomMessage("save_token", make_token(pw)) }
    else { fails(fails() + 1); Sys.sleep(1); gate_msg("Wrong password.") }
  })
  observe({                                            # the password changed in the sheet: sign everyone out
    tick()
    if (isolate(authed())) {
      pw <- get_password()
      if (is.null(pw) || !identical(pw, isolate(typed()))) { authed(FALSE); typed(NULL); map_ready(FALSE); gate_msg("The password has changed. Enter the new one.") }
    }
  })
  observeEvent(input$map_zoom, map_ready(TRUE))

  sheet <- reactive({ req(authed()); tick(); read_entries(stations) })
  asof_time <- reactive({
    if (isTRUE(input$asof_on)) as.POSIXct(paste(format(Sys.Date() - 1), "23:59:59"), tz = TZ)    # the end of yesterday
    else { tick(); Sys.time() }
  })
  bird_colours <- reactive({                              # which colour each bird has right now
    s <- sheet(); if (is.null(s$df)) return(character())
    assign_colours(s$df[s$df$time <= asof_time(), , drop = FALSE])
  })
  states <- reactive({
    s <- sheet(); if (is.null(s$df)) return(list())
    e <- s$df[s$df$time <= asof_time(), , drop = FALSE]
    birds <- sort(unique(e$bird))                       # any bird number that appears in the sheet
    out <- lapply(birds, function(b) bird_state(b, e[e$bird == b, , drop = FALSE], stations))
    names(out) <- as.character(birds)
    out
  })

  output$status_note <- renderText({
    s <- sheet()
    if (!is.null(s$note)) return(s$note)
    last <- cache$signals$t
    when <- if (is.null(last)) "" else paste0(" Sheet read at ", format(as.POSIXct(last, origin = "1970-01-01", tz = TZ), "%H:%M:%S"), ".")
    msg <- paste0("Stations: ", nrow(stations), ". Rows read: ", nrow(s$df), ".", when)
    if (length(s$ignored)) msg <- paste0(msg, " Ignored sheet rows (check station spelling/blank cells): ",
                                         paste(head(s$ignored_why, 6), collapse = "; "))
    ob <- read_others()
    if (!is.null(ob$note)) msg <- paste0(msg, " ", ob$note)
    else if (!is.null(ob$n)) {
      msg <- paste0(msg, " Non-tx rows read: ", ob$n, ".")
      if (length(ob$ignored_why)) msg <- paste0(msg, " Ignored non-tx rows: ", paste(head(ob$ignored_why, 6), collapse = "; "))
    }
    tg <- teams()
    if (!is.null(tg) && length(tg$ignored)) msg <- paste0(msg, " Ignored Teams rows (need a team name plus lat/lon or a high point): ",
                                                           paste(head(tg$ignored, 10), collapse = ", "))
    msg
  })

  output$map <- renderLeaflet({
    s <- stations; pad <- 0.04
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite", options = providerTileOptions(opacity = SAT_OPACITY)) %>%
      addTiles(group = "Street map") %>%
      addLayersControl(baseGroups = c("Satellite", "Street map"), options = layersControlOptions(collapsed = TRUE)) %>%
      addCircleMarkers(data = s, lng = ~lon, lat = ~lat, radius = 5, color = "white", weight = 2,
                       fillColor = "#000000", fillOpacity = 1, label = ~name,
                       labelOptions = labelOptions(noHide = TRUE, direction = "right", className = "station-label", textsize = "10px",
                                           style = list("padding" = "0px 3px", "line-height" = "1.2"))) %>%
      fitBounds(min(s$lon) - pad, min(s$lat) - pad, max(s$lon) + pad, max(s$lat) + pad) %>%
      # station names show only when zoomed in; zoomed out they pile up, so hide them
      htmlwidgets::onRender("function(el, x) {
        var map = this;
        function f() { el.classList.toggle('zoomed-out', map.getZoom() < 11); }
        map.on('zoomend', f); f();
        // bearing lines, direction chevrons, bird numbers and found stars are drawn here, in screen pixels, so that lines which
        // would overlap can be shifted sideways at any zoom. Ticking a bird in the list only shows or hides things here, so it is instant.
        var chev = [], founds = [], trails = {}, sel = [], layer = L.layerGroup().addTo(map), GAP = 42, JITTER = 16, JDEG = 3;
        window.rehoSel = [];
        var shadow = '0 0 3px #000, 0 0 3px #000, -1px -1px 0 #000, 1px -1px 0 #000, -1px 1px 0 #000, 1px 1px 0 #000';
        function visible(arr) { return !sel.length ? arr.filter(function(c) { return c.vis; }) : []; }       // a ticked bird is drawn by renderTrail instead
        function jitter(items) {            // lines from the same spot at (nearly) the same bearing are fanned out sideways
          items.forEach(function(c) { c.off = 0; });
          var byKey = {};
          items.forEach(function(c) { (byKey[c.key] = byKey[c.key] || []).push(c); });
          Object.keys(byKey).forEach(function(k) {
            var g = byKey[k].slice().sort(function(a, b) { return a.brg - b.brg; });
            var clusters = [], anchor = null;
            g.forEach(function(c) {
              var d = anchor === null ? 999 : Math.abs((((c.brg - anchor + 180) % 360) + 360) % 360 - 180);
              if (anchor === null || d > JDEG) { clusters.push([]); anchor = c.brg; }
              clusters[clusters.length - 1].push(c);
            });
            clusters.forEach(function(cl) {
              cl.sort(function(a, b) { return a.bird - b.bird; });
              if (cl.length > 1) cl.forEach(function(c, i) { c.off = (i - (cl.length - 1) / 2) * JITTER; });
            });
          });
        }
        function addStar(c, lyr) {                                      // found birds: a gold star with the bird number above it
          var star = '<svg width=\"22\" height=\"22\" viewBox=\"0 0 32 32\" style=\"display:block\"><polygon points=\"16.0,2.5 19.4,11.8 29.3,12.2 21.5,18.3 24.2,27.8 16.0,22.3 7.8,27.8 10.5,18.3 2.7,12.2 12.6,11.8\" fill=\"' + (c.col || '#FFC400') + '\" stroke=\"#111\" stroke-width=\"2\" stroke-linejoin=\"round\"/></svg>';
          var html = '<div style=\"position:relative;width:22px;height:22px\">' + star +
            '<div style=\"position:absolute;bottom:23px;left:50%;transform:translateX(-50%);text-align:center;white-space:nowrap;font:bold 16px/18px sans-serif;color:' + (c.col || '#FFC400') + ';text-shadow:' + shadow + '\">' + c.bird +
            (c.tag ? '<div style=\"font-size:11px;line-height:12px\">' + c.tag + '</div>' : '') + '</div></div>';
          L.marker([c.lat, c.lon], {icon: L.divIcon({className: 'chev', html: html, iconSize: [22, 22], iconAnchor: [11, 11]}), opacity: c.op})
            .bindPopup(c.popup).addTo(lyr);
        }
        function draw() {
          layer.clearLayers();
          var z = map.getZoom(), vis = visible(chev);
          jitter(vis);
          vis.forEach(function(c) {
            var a = map.project([c.lat, c.lon], z), b = map.project([c.elat, c.elon], z);
            var dx = b.x - a.x, dy = b.y - a.y, len = Math.sqrt(dx * dx + dy * dy);
            if (len < 1) return;
            var ux = dx / len, uy = dy / len, px = -uy, py = ux;          // along the line; to its right
            var a2 = L.point(a.x + px * c.off, a.y + py * c.off), b2 = L.point(b.x + px * c.off, b.y + py * c.off);
            var deg = Math.atan2(dx, -dy) * 180 / Math.PI;
            var ends = [map.unproject(a2, z), map.unproject(b2, z)];
            L.polyline(ends, {color: '#000', weight: 3, opacity: 0.45 * c.op, interactive: false}).addTo(layer);   // dark edge, so light colours show on bright ground
            L.polyline(ends, {color: c.col, weight: 2.5, opacity: c.op})
              .bindTooltip(c.label, {sticky: true}).addTo(layer);
            for (var d = GAP; d < len - 10; d += GAP) {
              var p = map.unproject(L.point(a2.x + ux * d, a2.y + uy * d), z);
              var svg = '<svg width=\"13\" height=\"13\" viewBox=\"0 0 18 18\" style=\"display:block;transform:rotate(' + deg + 'deg)\">' +
                '<path d=\"M3 12 L9 5 L15 12\" fill=\"none\" stroke=\"rgba(0,0,0,0.65)\" stroke-width=\"5\" stroke-linecap=\"round\" stroke-linejoin=\"round\"/>' +
                '<path d=\"M3 12 L9 5 L15 12\" fill=\"none\" stroke=\"#fff\" stroke-width=\"2.6\" stroke-linecap=\"round\" stroke-linejoin=\"round\"/></svg>';
              L.marker(p, {icon: L.divIcon({className: 'chev', html: svg, iconSize: [13, 13], iconAnchor: [6, 6]}),
                           interactive: false, keyboard: false, opacity: c.op}).addTo(layer);
            }
            // bird number just past the tip, a little to the side of the line
            var t = map.unproject(L.point(b2.x + ux * 10 + px * 11, b2.y + uy * 10 + py * 11), z);
            L.marker(t, {icon: L.divIcon({className: 'chev', iconSize: [90, 34], iconAnchor: [45, c.tag ? 17 : 10],
              html: '<div style=\"text-align:center;font:bold 16px/18px sans-serif;color:' + c.col + ';text-shadow:' + shadow + '\">' + c.bird +
                    (c.tag ? '<div style=\"font-size:11px;line-height:12px;white-space:nowrap\">' + c.tag + '</div>' : '') + '</div>'}),
              interactive: false, keyboard: false, opacity: c.op}).addTo(layer);
          });
          visible(founds).forEach(function(c) {                           // found birds: a gold star with the bird number above it
            addStar(c, layer);
          });
        }
        map.on('zoomend', draw);
        // untracked birds: a white diamond (date label under it); tap for the details
        var others = L.layerGroup().addTo(map);
        Shiny.addCustomMessageHandler('others', function(m) {
          others.clearLayers();
          (Array.isArray(m) ? m : []).forEach(function(o) {
            var shadow = '0 0 3px #000, 0 0 3px #000, -1px -1px 0 #000, 1px -1px 0 #000, -1px 1px 0 #000, 1px 1px 0 #000';
            var html = '<div style=\"position:relative;width:24px;height:24px\">' +
              '<div style=\"position:absolute;left:5px;top:5px;width:12px;height:12px;background:#fff;border:2px solid #000;transform:rotate(45deg)\"></div>' +
              (o.tag ? '<div style=\"position:absolute;top:24px;left:50%;transform:translateX(-50%);white-space:nowrap;font:bold 11px sans-serif;color:#fff;text-shadow:' + shadow + '\">' + o.tag + '</div>' : '') + '</div>';
            L.marker([o.lat, o.lon], {icon: L.divIcon({className: 'chev', html: html, iconSize: [24, 24], iconAnchor: [11, 11]}), opacity: o.op})
              .bindPopup(o.popup).addTo(others);
          });
        });
        // a ticked bird: its signal lines from its last few days as dotted lines with chevrons and an x-days-ago label,
        // and a gold star (with the same label) for every confirmed location
        var trailLayer = L.layerGroup().addTo(map);
        function hist(b) { return trails[String(b)] || {lines: [], stars: []}; }
        function trailLL(birds) {
          var pts = [];
          birds.forEach(function(b) {
            var h = hist(b);
            h.lines.forEach(function(c) { pts.push([c.lat, c.lon]); pts.push([c.elat, c.elon]); });
            h.stars.forEach(function(c) { pts.push([c.lat, c.lon]); });
          });
          return pts;
        }
        function renderTrail() {
          trailLayer.clearLayers();
          if (!sel.length) return;
          var z = map.getZoom(), items = [], stars = [];
          sel.forEach(function(b) { var h = hist(b); items = items.concat(h.lines); stars = stars.concat(h.stars); });
          jitter(items);
          items.forEach(function(c) {
            var a = map.project([c.lat, c.lon], z), b = map.project([c.elat, c.elon], z);
            var dx = b.x - a.x, dy = b.y - a.y, len = Math.sqrt(dx * dx + dy * dy);
            if (len < 1) return;
            var ux = dx / len, uy = dy / len, px = -uy, py = ux;
            var a2 = L.point(a.x + px * c.off, a.y + py * c.off), b2 = L.point(b.x + px * c.off, b.y + py * c.off);
            var deg = Math.atan2(dx, -dy) * 180 / Math.PI, ends = [map.unproject(a2, z), map.unproject(b2, z)];
            L.polyline(ends, {color: '#000', weight: 5, opacity: 0.5, dashArray: '1,9', lineCap: 'round', interactive: false}).addTo(trailLayer);
            L.polyline(ends, {color: c.col, weight: 3, dashArray: '1,9', lineCap: 'round'}).bindTooltip(c.label, {sticky: true}).addTo(trailLayer);
            for (var d = GAP; d < len - 10; d += GAP) {
              var p = map.unproject(L.point(a2.x + ux * d, a2.y + uy * d), z);
              var svg = '<svg width=\"13\" height=\"13\" viewBox=\"0 0 18 18\" style=\"display:block;transform:rotate(' + deg + 'deg)\">' +
                '<path d=\"M3 12 L9 5 L15 12\" fill=\"none\" stroke=\"rgba(0,0,0,0.65)\" stroke-width=\"5\" stroke-linecap=\"round\" stroke-linejoin=\"round\"/>' +
                '<path d=\"M3 12 L9 5 L15 12\" fill=\"none\" stroke=\"#fff\" stroke-width=\"2.6\" stroke-linecap=\"round\" stroke-linejoin=\"round\"/></svg>';
              L.marker(p, {icon: L.divIcon({className: 'chev', html: svg, iconSize: [13, 13], iconAnchor: [6, 6]}), interactive: false, keyboard: false}).addTo(trailLayer);
            }
            var t = map.unproject(L.point(b2.x + ux * 10 + px * 11, b2.y + uy * 10 + py * 11), z);
            L.marker(t, {icon: L.divIcon({className: 'chev', iconSize: [90, 20], iconAnchor: [45, 10],
              html: '<div style=\"text-align:center;white-space:nowrap;font:bold 12px/14px sans-serif;color:' + c.col + ';text-shadow:' + shadow + '\">' + c.tag + '</div>'}),
              interactive: false, keyboard: false}).addTo(trailLayer);
          });
          stars.forEach(function(c) { addStar(c, trailLayer); });
        }
        map.on('zoomend', renderTrail);
        Shiny.addCustomMessageHandler('trails', function(m) { trails = (m && !Array.isArray(m)) ? m : {}; renderTrail(); });
        function toast(t) {
          var d = document.createElement('div');
          d.style.cssText = 'position:absolute;left:50%;bottom:30px;transform:translateX(-50%);z-index:1200;background:rgba(20,20,20,0.9);color:#fff;padding:8px 12px;border-radius:8px;font:13px sans-serif';
          d.textContent = t; el.appendChild(d); setTimeout(function() { d.remove(); }, 3500);
        }
        // ticking birds in the list (the list calls this with the bird numbers ticked; an empty list clears)
        var resetBtn = null;
        window.rehoSelect = function(a) {
          var was = sel;
          sel = Array.isArray(a) ? a : [];
          window.rehoSel = sel;
          if (window.rehoSync) window.rehoSync(sel);
          renderTrail(); draw();
          if (!sel.length) return;
          var pts = trailLL(sel);
          if (pts.length) map.fitBounds(L.latLngBounds(pts), {padding: [50, 50], maxZoom: 15});
          sel.forEach(function(b) { if (was.indexOf(b) < 0 && !hist(b).lines.length && !hist(b).stars.length) toast('Bird ' + b + ' has no entries yet'); });
        };
        // long press (or right-click) on the map: a popup with a link that opens Google Maps directions to that spot
        (function() {
          var timer = null, sx = 0, sy = 0, downs = 0;
          function stop() { if (timer) { clearTimeout(timer); timer = null; } }
          function openAt(cx, cy) {
            var r = el.getBoundingClientRect();
            var ll = map.containerPointToLatLng(L.point(cx - r.left, cy - r.top));
            var box = document.createElement('div');
            var t = document.createElement('div'); t.style.marginBottom = '6px'; t.textContent = ll.lat.toFixed(5) + ', ' + ll.lng.toFixed(5);
            var a = document.createElement('a');
            a.href = 'https://www.google.com/maps/dir/?api=1&destination=' + ll.lat.toFixed(6) + ',' + ll.lng.toFixed(6) + '&travelmode=driving';
            a.target = '_blank'; a.rel = 'noopener'; a.textContent = 'Directions in Google Maps';
            a.style.cssText = 'display:inline-block;padding:8px 10px;background:#1a73e8;color:#fff;border-radius:6px;font-weight:bold;text-decoration:none';
            box.appendChild(t); box.appendChild(a);
            L.popup().setLatLng(ll).setContent(box).openOn(map);
            try { if (navigator.vibrate) navigator.vibrate(25); } catch (e) {}
          }
          function onControl(e) { return e.target && e.target.closest && e.target.closest('.leaflet-control, .leaflet-popup, .leaflet-marker-icon'); }
          el.addEventListener('pointerdown', function(e) {
            downs++; stop();
            if (downs > 1 || onControl(e)) return;                       // pinch zoom, or a button / marker
            sx = e.clientX; sy = e.clientY;
            timer = setTimeout(function() { timer = null; openAt(sx, sy); }, 650);
          });
          el.addEventListener('pointermove', function(e) { if (timer && Math.abs(e.clientX - sx) + Math.abs(e.clientY - sy) > 10) stop(); });
          ['pointerup', 'pointercancel'].forEach(function(n) { el.addEventListener(n, function() { downs = Math.max(0, downs - 1); stop(); }); });
          map.on('movestart zoomstart', stop);
          el.addEventListener('contextmenu', function(e) {
            if (onControl(e)) return;
            e.preventDefault(); stop(); openAt(e.clientX, e.clientY);
          });
        })();
        // full-screen button on the map (CSS based, so it also works on iPhones)
        // legend panel (opened with the Legend button)
        var legend = document.createElement('div');
        legend.style.cssText = 'display:none;position:absolute;bottom:150px;left:10px;z-index:1000;max-width:270px;max-height:calc(100% - 170px);overflow:auto;' +
          'background:rgba(20,20,20,0.9);color:#fff;border-radius:8px;padding:10px 12px;font:13px/1.3 sans-serif;box-shadow:0 2px 8px rgba(0,0,0,0.5)';
        L.DomEvent.disableClickPropagation(legend); L.DomEvent.disableScrollPropagation(legend);
        function mk(tag, css, text) { var e = document.createElement(tag); if (css) e.style.cssText = css; if (text) e.textContent = text; return e; }
        function lrow(sample, text) {
          var r = mk('div', 'display:flex;align-items:center;gap:10px;margin:7px 0');
          var sb = mk('div', 'width:56px;flex:none;display:flex;justify-content:center;align-items:center'); sb.appendChild(sample);
          r.appendChild(sb); r.appendChild(mk('div', 'flex:1', text)); return r;
        }
        function head(t) { return mk('div', 'font-weight:bold;margin:10px 0 2px;color:#fff', t); }
        legend.appendChild(head('Tracked birds'));
        var sLine = mk('div', 'width:38px;height:3px;background:#ff5cc8;border-radius:2px;box-shadow:0 0 0 1.5px rgba(0,0,0,0.6);position:relative');
        sLine.appendChild(mk('span', 'position:absolute;left:11px;top:-11px;font:bold 16px sans-serif;color:#fff;text-shadow:0 0 3px #000,0 0 3px #000', '\\u203A'));
        sLine.appendChild(mk('span', 'position:absolute;left:25px;top:-11px;font:bold 16px sans-serif;color:#fff;text-shadow:0 0 3px #000,0 0 3px #000', '\\u203A'));
        legend.appendChild(lrow(sLine, 'Signal line'));
        var ns = 'http://www.w3.org/2000/svg', star = document.createElementNS(ns, 'svg');
        star.setAttribute('width', '22'); star.setAttribute('height', '22'); star.setAttribute('viewBox', '0 0 32 32');
        var poly = document.createElementNS(ns, 'polygon');
        poly.setAttribute('points', '16.0,2.5 19.4,11.8 29.3,12.2 21.5,18.3 24.2,27.8 16.0,22.3 7.8,27.8 10.5,18.3 2.7,12.2 12.6,11.8');
        poly.setAttribute('fill', '#FFC400'); poly.setAttribute('stroke', '#111'); poly.setAttribute('stroke-width', '2'); poly.setAttribute('stroke-linejoin', 'round');
        star.appendChild(poly);
        legend.appendChild(lrow(star, 'Eyes-on'));
        legend.appendChild(head('Ticked birds from list'));
        var dl = mk('div', 'width:40px;height:0;border-top:3px dotted #00f0ff;position:relative');
        dl.appendChild(mk('span', 'position:absolute;left:14px;top:-13px;font:bold 16px sans-serif;color:#fff;text-shadow:0 0 3px #000,0 0 3px #000', '\\u203A'));
        legend.appendChild(lrow(dl, 'Most recent five days'));
        var star2 = star.cloneNode(true); star2.firstChild.setAttribute('fill', '#00f0ff');
        legend.appendChild(lrow(star2, 'Confirmed location'));
        legend.appendChild(head('Teams'));
        legend.appendChild(lrow(mk('div', 'width:14px;height:14px;border-radius:50%;background:#1e6bff;border:2px solid #fff'), 'At high point'));
        legend.appendChild(lrow(mk('div', 'width:26px;height:26px;border-radius:50%;background:rgba(30,107,255,0.28);border:2px dashed #fff'), 'General location'));
        legend.appendChild(head('Other'));
        legend.appendChild(lrow(mk('div', 'width:10px;height:10px;border-radius:50%;background:#000;border:2px solid #fff'), 'High point (tick names on/off)'));
        legend.appendChild(lrow(mk('div', 'width:11px;height:11px;background:#fff;border:2px solid #000;transform:rotate(45deg)'), 'Non-tx bird (tap for date and colours)'));
        legend.appendChild(lrow(mk('div', 'font:bold 11px sans-serif;color:#fff;border:1px solid #fff;border-radius:6px;padding:3px 5px;text-align:center', 'Reset'), 'Reset button'));
        legend.appendChild(mk('div', 'margin:8px 0 2px', 'Press and hold for Google Maps directions'));
        el.appendChild(legend);
        function fitSightings() {                                       // whatever is showing now (or the ticked bird's trail)
          var pts = [];
          if (sel.length) pts = trailLL(sel);
          else {
            visible(chev).forEach(function(c) { pts.push([c.lat, c.lon]); pts.push([c.elat, c.elon]); });
            visible(founds).forEach(function(c) { pts.push([c.lat, c.lon]); });
          }
          if (pts.length) map.fitBounds(L.latLngBounds(pts), {padding: [40, 40], maxZoom: 15});
        }
        var Full = L.Control.extend({ options: { position: 'bottomleft' }, onAdd: function() {
          var wrap = L.DomUtil.create('div', ''); wrap.style.display = 'flex'; wrap.style.flexDirection = 'column'; wrap.style.alignItems = 'flex-start'; wrap.style.gap = '6px';
          var b = L.DomUtil.create('div', 'full-btn', wrap); b.innerHTML = 'Full screen';
          var f = L.DomUtil.create('div', 'full-btn', wrap); f.innerHTML = 'Fit sightings';
          var lg = L.DomUtil.create('div', 'full-btn', wrap); lg.innerHTML = 'Legend';
          var rs = L.DomUtil.create('div', 'full-btn', wrap); rs.innerHTML = 'Reset'; resetBtn = rs;
          L.DomEvent.on(rs, 'click', function() { window.rehoSelect([]); fitSightings(); });
          L.DomEvent.on(lg, 'click', function() { legend.style.display = legend.style.display === 'none' ? 'block' : 'none'; });
          L.DomEvent.disableClickPropagation(wrap);
          L.DomEvent.on(f, 'click', fitSightings);
          function set(on) {
            el.classList.toggle('map-full', on);
            document.body.style.overflow = on ? 'hidden' : '';
            b.innerHTML = on ? 'Exit full screen' : 'Full screen';
            setTimeout(function() { map.invalidateSize(); draw(); }, 50);
          }
          L.DomEvent.on(b, 'click', function() { set(!el.classList.contains('map-full')); });
          document.addEventListener('keydown', function(e) { if (e.key === 'Escape' && el.classList.contains('map-full')) set(false); });
          return wrap;
        } });
        map.addControl(new Full());
        Shiny.addCustomMessageHandler('chevrons', function(m) { chev = Array.isArray(m) ? m : []; draw(); });
        Shiny.addCustomMessageHandler('founds', function(m) { founds = Array.isArray(m) ? m : []; draw(); });
      }")
  })

  observe({
    req(authed(), map_ready())
    st <- states(); asof <- asof_time(); today <- as.Date(asof, tz = TZ); bcols <- bird_colours()
    chev <- list()                                     # every bird's lines; the browser shows the ones it should (vis = shown without a tick)
    founds <- list()                                   # every found bird's star
    now_day <- as.Date(Sys.time(), tz = TZ)
    for (s in st) {
      if (isTRUE(s$never)) next
      vis <- isTRUE(s$date == today)
      dago <- as.integer(now_day - s$date)
      tag <- if (dago >= 1 && !isTRUE(input$asof_on)) sprintf("%d day%s ago", dago, if (dago == 1) "" else "s") else ""   # no tag in the yesterday view
      op <- max(0.15, 1 - as.numeric(difftime(asof, s$last_time, units = "days")) / FADE_DAYS)
      col <- unname(bcols[as.character(s$bird)]); if (length(col) != 1 || is.na(col)) col <- "#555555"   # plain text, no names attached
      if (!is.null(s$lines)) {
        len <- line_lengths(s$lines)
        for (i in seq_len(nrow(s$lines))) {
          l <- s$lines[i, ]
          ep <- end_point(l$lon, l$lat, l$bearing, len[i])
          chev[[length(chev) + 1]] <- list(
            lon = l$lon, lat = l$lat, elon = ep[["lon"]], elat = ep[["lat"]], op = op, col = col, vis = vis,
            bird = s$bird, brg = l$bearing, key = l$key, tag = tag,
            label = sprintf("Bird %d: %d\u00b0 from %s (%s, %.1f km), signal %s", s$bird, round(l$bearing),
                            htmltools::htmlEscape(l$key), if (nzchar(l$strength)) l$strength else "strength not given", round(len[i], 1),
                            format(l$time, "%a %d %b %H:%M")))
        }
      }
      if (!is.null(s$point) && identical(s$status, "Found"))
        founds[[length(founds) + 1]] <- list(
          lon = s$point[1], lat = s$point[2], op = op, vis = vis, bird = s$bird, tag = tag,
          popup = sprintf("<b>Bird %d</b><br>%s %s<br>Last signal %s", s$bird, s$status,
                          if (nzchar(s$detail)) paste0("(", s$detail, ")") else "", format(s$last_time, "%a %d %b %H:%M")))
    }
    session$sendCustomMessage("chevrons", unname(chev))
    session$sendCustomMessage("founds", unname(founds))
  })

  teams <- reactive({ req(authed()); tick(); read_teams(stations) })
  observe({                                              # field teams: current position each, own layer
    req(authed(), map_ready())
    proxy <- leafletProxy("map") %>% clearGroup("teams")
    tm <- teams()$df
    if (is.null(tm) || !nrow(tm)) return()
    lab <- list("font-weight" = "bold", "font-size" = "14px", color = "#ffffff",
                "text-shadow" = "0 0 3px #000, 0 0 3px #000, -1px -1px 0 #000, 1px -1px 0 #000, -1px 1px 0 #000, 1px 1px 0 #000")
    for (i in seq_len(nrow(tm))) {
      if (isTRUE(tm$area[i])) {                          # general area: a circle, with the team name in the middle
        proxy <- proxy %>%
          addCircles(lng = tm$lon[i], lat = tm$lat[i], radius = TEAM_AREA_M, group = "teams", color = "#ffffff", weight = 3,
                     dashArray = "7,7", opacity = 1, fillColor = "#1e6bff", fillOpacity = 0.28) %>%
          addLabelOnlyMarkers(lng = tm$lon[i], lat = tm$lat[i], group = "teams", label = tm$team[i],
                              labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "center", style = lab))
      } else {                                           # at a high point: a dot
        proxy <- proxy %>%
          addCircleMarkers(lng = tm$lon[i], lat = tm$lat[i], radius = 9, color = "white", weight = 3,
                           fillColor = "#1e6bff", fillOpacity = 1, opacity = 1, group = "teams") %>%
          addLabelOnlyMarkers(lng = tm$lon[i], lat = tm$lat[i], group = "teams", label = tm$team[i],
                              labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "bottom",
                                                          offset = c(0, 6), style = lab))
      }
    }
  })

  observe({                                              # non-tx birds: shown whenever the tick box is on
    req(authed(), map_ready())
    tick(); asof <- asof_time()
    o <- if (isTRUE(input$show_others)) read_others()$df else NULL
    out <- list()
    if (!is.null(o) && nrow(o)) {
      o <- o[o$time <= asof, , drop = FALSE]            # every sighting up to the day being viewed, whatever its date
      for (i in seq_len(nrow(o)))
        out[[i]] <- list(lat = o$lat[i], lon = o$lon[i], tag = "", op = 1,
                         popup = sprintf("<b>Non-tx bird</b><br>Found %s<br>Colour bands: %s<br>%.5f, %.5f",
                                         format(o$time[i], "%a %d %b %Y"),
                                         if (nzchar(o$bands[i])) htmltools::htmlEscape(o$bands[i]) else "not recorded", o$lat[i], o$lon[i]))
    }
    session$sendCustomMessage("others", unname(out))
  })

  tbl <- reactive({                                      # one row per bird, in bird-number order
    st <- states()
    if (!length(st)) return(data.frame(Bird = integer(), Eyes = character(), Days = character(), stringsAsFactors = FALSE))
    now_day <- as.Date(Sys.time(), tz = TZ)
    bird <- vapply(st, function(s) as.integer(s$bird), integer(1))
    eyes <- vapply(st, function(s) if (!isTRUE(s$never) && identical(s$status, "Found")) "Eyes-on" else "Signals", character(1))
    dago <- vapply(st, function(s) if (isTRUE(s$never)) NA_integer_ else as.integer(now_day - as.Date(s$last_time, tz = TZ)), integer(1))
    df <- data.frame(Bird = unname(bird), Eyes = unname(eyes), Days = ifelse(is.na(dago), "-", ifelse(dago <= 0, "today", as.character(dago))),
                     stringsAsFactors = FALSE)
    df[order(df$Bird), , drop = FALSE]
  })
  output$tbl <- renderDT({
    d <- tbl()
    box <- sprintf("<input type='checkbox' class='bird-tick' data-bird='%d' style='width:20px;height:20px;margin:0'>", d$Bird)
    datatable(data.frame(` ` = box, Bird = d$Bird, `Eyes-on` = d$Eyes, `Days since detected` = d$Days, check.names = FALSE),
              rownames = FALSE, escape = FALSE, selection = "none",
              options = list(dom = "t", pageLength = 100, ordering = FALSE,
                             drawCallback = JS("function() { if (window.rehoSync) window.rehoSync(window.rehoSel); }")))
  })

  # every bird's last positions are sent to the phone at once, so ticking a bird needs no round trip to the server
  trails_all <- reactive({
    s <- sheet(); asof <- asof_time(); bcols <- bird_colours()
    out <- list(); now_day <- as.Date(Sys.time(), tz = TZ)
    if (is.null(s$df)) return(out)
    for (b in sort(unique(s$df$bird))) {
      e <- s$df[s$df$bird == b & s$df$time <= asof, , drop = FALSE]
      if (!nrow(e)) next
      e$date <- as.Date(e$time, tz = TZ)
      days <- head(sort(unique(e$date), decreasing = TRUE), TRAIL_N)       # the bird's last TRAIL_N days with entries
      col <- unname(bcols[as.character(b)]); if (length(col) != 1 || is.na(col)) col <- "#555555"
      lines <- list(); stars <- list()
      for (d in days) {
        d <- as.Date(d, origin = "1970-01-01")
        ago <- as.integer(now_day - d)
        tag <- if (ago <= 0) "today" else sprintf("%d day%s ago", ago, if (ago == 1) "" else "s")
        ed <- e[e$date == d, , drop = FALSE]
        f <- ed[ed$type == "found", , drop = FALSE]
        for (i in seq_len(nrow(f)))
          stars[[length(stars) + 1]] <- list(lat = f$lat[i], lon = f$lon[i], bird = b, tag = tag, op = 1, col = col,
            popup = sprintf("<b>Bird %d</b><br>Found %s<br>%.5f, %.5f", b, format(f$time[i], "%a %d %b %Y %H:%M"), f$lat[i], f$lon[i]))
        stt <- bird_state(b, ed, stations)
        if (isTRUE(stt$never) || is.null(stt$lines) || !nrow(stt$lines)) next
        len <- line_lengths(stt$lines)
        for (i in seq_len(nrow(stt$lines))) {
          l <- stt$lines[i, ]; ep <- end_point(l$lon, l$lat, l$bearing, len[i])
          lines[[length(lines) + 1]] <- list(lon = l$lon, lat = l$lat, elon = ep[["lon"]], elat = ep[["lat"]], col = col, bird = b,
            brg = l$bearing, key = l$key, tag = tag,
            label = sprintf("Bird %d: %d\u00b0 from %s (%s), %s", b, round(l$bearing), htmltools::htmlEscape(l$key),
                            if (nzchar(l$strength)) l$strength else "strength not given", format(l$time, "%a %d %b %H:%M")))
        }
      }
      if (length(lines) || length(stars)) out[[as.character(b)]] <- list(lines = unname(lines), stars = unname(stars))
    }
    out
  })
  observe({
    req(authed(), map_ready())
    session$sendCustomMessage("trails", trails_all())
  })
}

shinyApp(ui, server)
