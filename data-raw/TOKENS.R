## code to prepare `TOKENS` dataset goes here

TOKENS <- list(
  osm = "5b3ce3597851110001cf624836e4829567104525aabed7070d9599d5",
  locationiq = "pk.b484b0c3b982913db4382afd1830d4ef",
  geoapify = "717a18ca415e47a1bccf1a87d2cf65e5"
)
usethis::use_data(TOKENS, overwrite = TRUE)
