zug_build <- buildings_in_village
zug_hh <- project(hh_vect[hh_vect$village %in% villages[i]$location, ], utm_nigeria_CRS)

rast_vil <- rast(ext(zug_hh), res = 49, crs = utm_nigeria_CRS)

rast_1 <- rasterize(zug_build, rast_vil)
rast_1[is.na(rast_1)] <- 0
plot(rast_1)
rast_2 <- rasterize(zug_hh, rast_vil)
rast_2[is.na(rast_2)] <- 0
plot(rast_2)

library("spatstat")
pp_1 <- as.ppp(st_as_sf(zug_build))
pp_2 <- as.ppp(st_as_sf(zug_hh))

sigma <- 31
sigma_1 <- bw.ppl(pp_1)
sigma_2 <- bw.ppl(pp_2)

kde_1 <- density(pp_1, sigma = sigma)
kde_2 <- density(pp_2, sigma = sigma)

# Convert pixel image to data frame
kde_1_df <- as.data.frame(kde_1)
kde_2_df <- as.data.frame(kde_2)
# Rename columns
names(kde_1_df) <- c("x", "y", "density")
names(kde_2_df) <- c("x", "y", "density")

# Plot KDE using ggplot2
ggplot() +
  geom_raster(data = kde_1_df, aes(x = x, y = y, fill = density)) +
  geom_contour(data = kde_2_df, aes(x = x, y = y, z = density, colour = "Sampled Household Density")) +
  scale_fill_viridis_c() +
  scale_color_manual(values = "red", labels = "Sampled Household Density") + 
  labs(title = "Kernel Density Estimate for Buildings vs. Households",
       x = element_blank(),
       y = element_blank(),
       fill = "Building density",
       colour = element_blank()) +
  theme_minimal()

# Normalize the KDE values for kde_1 and kde_2
max_density_kde1 <- max(kde_1_df$density)
max_density_kde2 <- max(kde_2_df$density)

kde_1_df$normalized_density <- kde_1_df$density / max_density_kde1
kde_2_df$normalized_density <- kde_2_df$density / max_density_kde2

# Calculate relative difference between normalized densities
kde_difference <- kde_1_df$normalized_density - kde_2_df$normalized_density
hist(kde_difference)
# Calculate relative density ratio between kde_1 and kde_2
kde_rel_dif <-  kde_1_df$normalized_density / kde_2_df$normalized_density

wilcox.test(kde_difference)

autocor(rast_1, method = "moran")
autocor(rast_2, method = "moran")
