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
N_BIRDS     <- 15
FADE_DAYS   <- 7                      # signal this old is drawn at its faintest
RUNNER_DAYS <- 7                      # no signal for this many days = "runner" (listed at the bottom)
LINE_KM     <- c(strong = 3, medium = 5, weak = 15)   # drawn line length (km) by signal strength
MAX_KM      <- 15                                      # nothing is drawn longer than this
line_km <- function(strength) {                        # no strength given = weak = 15 km
  k <- LINE_KM[tolower(trimws(as.character(strength)))]
  k[is.na(k)] <- LINE_KM[["weak"]]
  pmin(unname(k), MAX_KM)
}
REFRESH_SEC <- 10
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
parse_time <- function(x) {
  x <- trimws(as.character(x))
  out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = TZ)
  for (f in c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M",
              "%Y-%m-%dT%H:%M:%S", "%d/%m/%Y")) {
    miss <- is.na(out)
    if (!any(miss)) break
    out[miss] <- as.POSIXct(x[miss], format = f, tz = TZ)
  }
  out
}
read_entries <- function(stations) {
  raw <- get_signals(); d <- raw$d
  if (is.null(d)) return(list(df = NULL, ignored = integer(), note = raw$note))
  names(d) <- tolower(trimws(names(d)))
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
  ok <- !is.na(e$type) & !is.na(e$time) & !is.na(e$bird) & e$bird >= 1 & e$bird <= N_BIRDS &
        (is.na(e$bearing) | (e$bearing >= 0 & e$bearing <= 360))
  list(df = e[ok, c("time", "bird", "type", "station", "lon", "lat", "bearing", "strength")],
       ignored = e$row[!ok], note = NULL)
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
       lines = e[, c("lon", "lat", "bearing", "time", "key", "strength")], detail = detail)
}

# ---------------------------------------------------------------- UI
main_ui <- function() fluidPage(
  titlePanel("REHO live tracking"),
  if (!LIVE) div(class = "alert alert-warning", "DEMO MODE: example signals only. Set SHEET_ID and GOOGLE_KEY_JSON for real data."),
  fluidRow(
    column(3, checkboxInput("show_old", "Show lines from earlier days", FALSE)),
    column(5, checkboxInput("asof_on", "Replay: view as of the end of a past day", FALSE),
              conditionalPanel("input.asof_on", dateInput("asof_date", NULL, value = Sys.Date(), weekstart = 1))),
    column(4, textOutput("status_note"))
  ),
  leafletOutput("map", height = "82vh"),
  h5("Birds, longest unseen first; runners (never seen, or 7+ days) at the bottom (click a row to zoom)"),
  DTOutput("tbl")
)
gate_ui <- function(msg = NULL) fluidPage(
  div(style = "max-width: 340px; margin: 18vh auto 0 auto;",
      h3("Bird signal map"),
      passwordInput("pw", "Password", width = "100%"),
      tags$button(id = "login", type = "button", class = "btn btn-primary", "Open map"),
      if (!is.null(msg)) p(style = "margin-top: 12px; color: #b3261e;", msg))
)
ui <- fluidPage(
  tags$head(tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
            tags$script(HTML("
              function tryLogin(){ Shiny.setInputValue('login_try', {pw: $('#pw').val(), n: Math.random()}, {priority: 'event'}); }
              $(document).on('click', '#login', tryLogin);
              $(document).on('keydown', '#pw', function(e){ if (e.key === 'Enter') { e.preventDefault(); tryLogin(); } });"))),
  uiOutput("page")
)

# ---------------------------------------------------------------- server
server <- function(input, output, session) {
  st_info  <- load_stations()
  stations <- st_info$df
  pal <- grDevices::hcl.colors(N_BIRDS, "Dynamic")

  tick <- reactiveTimer(REFRESH_SEC * 1000)

  # ---- password gate: the sheet is only read, and nothing is sent to the browser, after a correct password
  authed <- reactiveVal(FALSE); typed <- reactiveVal(NULL); fails <- reactiveVal(0); gate_msg <- reactiveVal(NULL)
  map_ready <- reactiveVal(FALSE)
  output$page <- renderUI(if (authed()) main_ui() else gate_ui(gate_msg()))
  observeEvent(input$login_try, {
    if (fails() >= MAX_TRIES) { gate_msg("Too many wrong tries. Reload the page to try again."); return() }
    pw <- get_password()
    if (is.null(pw)) { gate_msg("The map is unavailable right now."); return() }
    if (identical(trimws(input$login_try$pw), pw)) { typed(pw); gate_msg(NULL); fails(0); map_ready(FALSE); authed(TRUE) }
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
    if (isTRUE(input$asof_on) && !is.null(input$asof_date)) as.POSIXct(paste(format(input$asof_date), "23:59:59"), tz = TZ)
    else { tick(); Sys.time() }
  })
  states <- reactive({
    s <- sheet(); if (is.null(s$df)) return(lapply(1:N_BIRDS, function(b) list(bird = b, never = TRUE)))
    e <- s$df[s$df$time <= asof_time(), , drop = FALSE]
    lapply(seq_len(N_BIRDS), function(b) bird_state(b, e[e$bird == b, , drop = FALSE], stations))
  })

  output$status_note <- renderText({
    s <- sheet()
    if (!is.null(s$note)) return(s$note)
    last <- cache$signals$t
    when <- if (is.null(last)) "" else paste0(" Sheet read at ", format(as.POSIXct(last, origin = "1970-01-01", tz = TZ), "%H:%M:%S"), ".")
    msg <- paste0("Stations: ", nrow(stations), ". Rows read: ", nrow(s$df), ".", when)
    if (length(s$ignored)) msg <- paste0(msg, " Ignored sheet rows (check station spelling/blank cells): ",
                                         paste(head(s$ignored, 10), collapse = ", "))
    msg
  })

  output$map <- renderLeaflet({
    s <- stations; pad <- 0.04
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addTiles(group = "Street map") %>%
      addLayersControl(baseGroups = c("Satellite", "Street map"), options = layersControlOptions(collapsed = TRUE)) %>%
      addCircleMarkers(data = s, lng = ~lon, lat = ~lat, radius = 7, color = "white", weight = 2,
                       fillColor = "#d81b8c", fillOpacity = 1, label = ~name,
                       labelOptions = labelOptions(noHide = TRUE, direction = "right")) %>%
      fitBounds(min(s$lon) - pad, min(s$lat) - pad, max(s$lon) + pad, max(s$lat) + pad)
  })

  observe({
    req(authed(), map_ready())
    st <- states(); asof <- asof_time(); today <- as.Date(asof, tz = TZ)
    proxy <- leafletProxy("map") %>% clearGroup("birds")
    for (s in st) {
      if (isTRUE(s$never)) next
      op <- max(0.15, 1 - as.numeric(difftime(asof, s$last_time, units = "days")) / FADE_DAYS)
      col <- pal[s$bird]
      if (!is.null(s$lines) && (s$date == today || isTRUE(input$show_old))) {
        for (i in seq_len(nrow(s$lines))) {
          l <- s$lines[i, ]
          ep <- end_point(l$lon, l$lat, l$bearing, line_km(l$strength))
          proxy <- proxy %>%
            addPolylines(lng = c(l$lon, ep[["lon"]]), lat = c(l$lat, ep[["lat"]]), color = col, weight = 3,
                         opacity = op, group = "birds",
                         label = sprintf("Bird %d: %d° from %s (%s, %d km)", s$bird, round(l$bearing), l$key,
                                         if (nzchar(l$strength)) l$strength else "strength not given", round(line_km(l$strength)))) %>%
            addLabelOnlyMarkers(lng = ep[["lon"]], lat = ep[["lat"]], group = "birds", label = as.character(s$bird),
                                labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "center",
                                  opacity = op, style = list(color = col, "font-weight" = "bold", "font-size" = "16px")))
        }
      }
      if (!is.null(s$point)) {
        is_found <- identical(s$status, "Found")
        proxy <- proxy %>%
          addCircleMarkers(lng = s$point[1], lat = s$point[2], radius = if (is_found) 10 else 8, color = "white",
                           weight = 2, fillColor = if (is_found) "#27ae60" else "#e67e22", fillOpacity = op,
                           opacity = op, group = "birds", label = as.character(s$bird),
                           labelOptions = labelOptions(noHide = TRUE, direction = "top", opacity = op),
                           popup = sprintf("<b>Bird %d</b><br>%s %s<br>Last signal %s", s$bird, s$status,
                                           if (nzchar(s$detail)) paste0("(", s$detail, ")") else "",
                                           format(s$last_time, "%a %d %b %H:%M")))
      }
    }
  })

  tbl <- reactive({
    st <- states(); asof <- asof_time()
    last <- do.call(c, lapply(st, function(s) if (isTRUE(s$never)) as.POSIXct(NA, tz = TZ) else s$last_time))
    df <- data.frame(Bird = vapply(st, function(s) s$bird, integer(1)),
                     Status = vapply(st, function(s) if (isTRUE(s$never)) "Never seen (runner)" else s$status, character(1)),
                     Last = last, stringsAsFactors = FALSE)
    days <- as.numeric(difftime(asof, df$Last, units = "days"))
    # runner = never seen, or no signal for RUNNER_DAYS or more
    runner <- is.na(days) | days >= RUNNER_DAYS
    df$Status <- ifelse(runner & !is.na(days), "Runner (no signal in 7+ days)", df$Status)
    df$Days <- ifelse(is.na(days), "-", sprintf("%.1f", pmax(days, 0)))
    df$LastTxt <- ifelse(is.na(df$Last), "-", format(df$Last, "%a %d %b %H:%M"))
    df <- df[order(runner, df$Last, na.last = TRUE), ]   # active birds first (longest unseen on top), runners last
    df
  })
  output$tbl <- renderDT({
    d <- tbl()
    datatable(data.frame(Bird = d$Bird, Status = d$Status, `Last signal` = d$LastTxt,
                         `Days since last seen` = d$Days, check.names = FALSE),
              rownames = FALSE, selection = "single", options = list(dom = "t", pageLength = N_BIRDS, ordering = FALSE))
  })
  observeEvent(input$tbl_rows_selected, {
    d <- tbl(); b <- d$Bird[input$tbl_rows_selected]; s <- states()[[b]]
    if (isTRUE(s$never)) return(showNotification(paste0("Bird ", b, " has no entries yet"), duration = 3))
    pos <- if (!is.null(s$point)) s$point else c(s$lines$lon[1], s$lines$lat[1])
    leafletProxy("map") %>% flyTo(pos[1], pos[2], zoom = 12)
  })
}

shinyApp(ui, server)
