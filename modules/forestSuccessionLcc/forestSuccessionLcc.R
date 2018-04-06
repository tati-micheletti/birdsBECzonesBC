stopifnot(packageVersion("SpaDES") >= "1.2.0.9006")

defineModule(sim, list(
  name = "forestSuccessionLcc",
  description = "This is a simple example model of forest succesion and aging based on Canada Land Cover Classes 2005. It is based on the LCC05 **without** fire and caribou modules.",
  keywords = c("forest", "succession", "vegetation dynamics", "LCC05", "land cover classification 2005"),
  authors = c(person("Eliot", "McIntire", email="eliot.mcintire@canada.ca", role=c("cre")),
              person("Alex", "Chubaty", email="alexander.chubaty@canada.ca", role=c("cre")),
              person("Tati", "Micheletti", email="tati.silva@ubc.ca", role=c("aut"))),
  childModules = c("cropReprojectLccAge", "forestAge", "LccToBeaconsReclassify", "forestSuccessionBeacons"),
  version = list(SpaDES.core = "0.1.0", forestSuccessionLcc = "0.0.1", cropReprojectLccAge = "1.1.3", forestAge = "1.1.1", LccToBeaconsReclassify = "1.1.1", forestSuccessionBeacons = "1.1.0"),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "forestSuccessionLcc.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", 1, NA, NA,
                    "simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 2, NA, NA,
                    "simulation time interval when plot event should occur after the first"),
    defineParameter(".saveInitialTime", "numeric", 1, NA, NA,
                    "simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", 1, NA, NA,
                    "simulation time interval when save event should occur after the first")
  ),
  inputObjects = data.frame(
    objectName = NA_character_, objectClass = NA_character_,
    sourceURL = NA_character_, other = NA_character_, stringsAsFactors = FALSE),
  outputObjects = data.frame(
    objectName = NA_character_, objectClass = NA_character_,
    other = NA_character_, stringsAsFactors = FALSE)
))

### no other code is needed for this module group ###
