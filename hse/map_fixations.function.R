library(intervals)

map_fixations <- function(fixations, rois, xbuffer=0, ybuffer=0) {

  # Preserve data type of roi id:
  target <- rep(rois$wn[[1]], nrow(fixations))
  target[1:length(target)] <- NA
  
  word <- rep(rois$word[[1]], nrow(fixations))
  word[1:length(word)] <- NA

  fx <- as.numeric(fixations$gavx)
  fy <- as.numeric(fixations$gavy)

  ints.x <- Intervals(cbind(rois$x1-xbuffer, rois$x2+xbuffer))
  ints.y <- Intervals(cbind(rois$y1-ybuffer, rois$y2+ybuffer))

  for (t in unique(rois$eyetrial)) {

    fi <- fixations$eyetrial==t
    ri <- rois$eyetrial==t

    ix <- interval_overlap(ints.x[ri,,drop=F], fx[fi])
    iy <- interval_overlap(ints.y[ri,,drop=F], fy[fi])

    # iterates over the words in that item:
    roi <- rois$wn[ri]
    w   <- rois$word[ri]
    fi <- which(fi)
    for (i in 1:length(ix)) {
      hits <- intersect(ix[[i]], iy[[i]])
      target[fi[hits]] <- roi[i]
      word[fi[hits]] <- w[i]
    }
  }
  data.frame(wn=target, word=word, stringsAsFactors=F)
}

