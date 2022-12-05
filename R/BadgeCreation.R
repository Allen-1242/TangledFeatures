#Sticker creation

library(magick)
## Linking to ImageMagick 6.9.9.14
## Enabled features: cairo, freetype, fftw, ghostscript, lcms, pango, rsvg, webp
## Disabled features: fontconfig, x11
library(dplyr)
##
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
##
##     filter, lag
## The following objects are masked from 'package:base':
##
##     intersect, setdiff, setequal, union
library(hexSticker)

img <- image_read("images/Yarn.png")

img %>%
  image_convert("png") %>%
  image_resize("1080 x 200") -> res
  #image_fill(color="#062047", point="+45") #%>%
  #image_annotate("d?ta", size=38, location = "+47+58", color="black")


final_res<-sticker(res, package="TangledFeatures", p_size=10, p_color = "#000000",
                   p_y = 1.5,
                   s_x=1, s_y=0.8, s_width=0.9,
                   s_height = 14,
                   filename="TangledFeatures.png",h_fill="#FFFFFF",h_color = "#000000")

plot(final_res)
