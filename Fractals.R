# Load necessary libraries
library(ggplot2)
library(magick)
library(paletteer)
library(tidyverse)
library(stringr)

pals <- palettes_c_names %>% filter(package == "grDevices")

# Define the Mandelbrot set function
# mandelbrot <- function(xlim, ylim, res = 500, itermax = 100) {
#   x <- seq(xlim[1], xlim[2], length.out = res)
#   y <- seq(ylim[1], ylim[2], length.out = res)
#   C <- outer(x, y*1i, '+') # Create the complex grid
#   Z <- matrix(0, res, res)
#   counts <- matrix(0, res, res)
#   
#   for (k in 1:itermax) {
#     within_bounds <- Mod(Z) < 2
#     counts <- counts + within_bounds
#     Z <- Z^2 + C
#   }
#   
#   return(as.data.frame(as.table(counts)))
# }

mandelbrot <- function(xlim, ylim, res = 500, itermax = 100, coloring_algorithm = "escape") {
  x <- seq(xlim[1], xlim[2], length.out = res)
  y <- seq(ylim[1], ylim[2], length.out = res)
  C <- outer(x, y*1i, '+')
  Z <- matrix(0, res, res)
  counts <- matrix(0, res, res)
  color_values <- matrix(0, res, res)
  
  for (k in 1:itermax) {
    within_bounds <- Mod(Z) < 2
    Z[within_bounds] <- Z[within_bounds]^2 + C[within_bounds]
    counts[within_bounds] <- counts[within_bounds] + 1
    
    # Apply coloring algorithm
    if (coloring_algorithm == "escape") {
      color_values[within_bounds] <- counts[within_bounds]
    } else if (coloring_algorithm == "smooth") {
      color_values[within_bounds] <- k - log(log(Mod(Z[within_bounds])))/log(2)
    } else if (coloring_algorithm == "distance") {
      color_values[within_bounds] <- abs(Mod(Z[within_bounds]))
    } else if (coloring_algorithm == "orbit") {
      trap_distance <- sqrt((Re(Z)^2 + Im(Z)^2))
      color_values[within_bounds] <- trap_distance
    } else if (coloring_algorithm == "phase") {
      color_values[within_bounds] <- Arg(Z[within_bounds])
    }
  }
  
  return(as.data.frame(as.table(color_values)))
}



# Generate and save multiple frames for animation with a specific zoom target
create_frames <- function(zoom_levels = 300, itermax = 100, zoom_target_x = -0.75, zoom_target_y = 0.1) {
  images <- list()
  
  # Define the initial boundaries of the Mandelbrot set, now focused on a specific area
  xlim <- c(-2, 1)
  ylim <- c(-1.5, 1.5)
  imax <- 100
  
  for (i in 200:zoom_levels) {
    # Calculate new zoom limits by narrowing in on the fractal tail region, avoiding the center
    zoom_factor <- 0.95 ^ i
    xlim_new <- c(zoom_target_x - (diff(xlim) * zoom_factor) / 2, zoom_target_x + (diff(xlim) * zoom_factor) / 2)
    ylim_new <- c(zoom_target_y - (diff(ylim) * zoom_factor) / 2, zoom_target_y + (diff(ylim) * zoom_factor) / 2)
    
    # Generate Mandelbrot data for the new zoom level
    if(imax <= itermax){
      imax <- imax + 11
    }
    mandelbrot_data <- mandelbrot(xlim_new, ylim_new, res = 2000, itermax = 2300, coloring_algorithm = "distance")
    for(k in 1:nrow(pals)){
      pal <- str_remove(paste("grDevices::",pals[k,2]), " ")
      print(paste("pal: ", pal))
      # Plot and save the image
      p <- ggplot(mandelbrot_data, aes(x = Var1, y = Var2, fill = Freq)) +
        geom_tile() +
        paletteer::scale_fill_paletteer_c(pal) +
        coord_equal() +
        theme_void() +
        theme(legend.position = "none")  + 
        ggtitle(paste("Color Palette: ", pal)) 
    
      # Save each frame as a PNG
      frame <- paste0("mandelbrot_frame_distance_", k, ".png")
      ggsave(frame, p, width = 6, height = 6, dpi = 200)
    }
    # Read the frame into magick for the animation
    # images[[i]] <- image_read(frame)
  }
  
  # Return the list of images
  return(images)
}
# 
# # Set your new zoom target (focusing on a tail region instead of the center)
zoom_target_x <- -0.7437588 # This is a known area with rich fractal structures
zoom_target_y <- 0.10593   # Slight offset to avoid convergence to a single color
# 
# # Create frames with a specific zoom target in the fractal tail region
images <- create_frames(zoom_levels = 200, itermax = 1400, zoom_target_x = zoom_target_x, zoom_target_y = zoom_target_y)
# 
# # Create animation from frames
# animation <- image_animate(image_join(images), fps = 24)
# 
# # Save the animation as a GIF
# image_write(animation, "mandelbrot_zoom_animation_tail.gif")
# 
# # Display the animation in RStudio viewer
# print(animation)