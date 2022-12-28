#!/bin/bash

while read fn
do

  slide=`basename $fn`
  output_prefix=dzi/$slide
  python /opt/software/github/openslide-python/examples/deepzoom/deepzoom_tile.py -j 8 -r -o $output_prefix $fn

done < /media/storage1/shenhr/project/TCIA/vis_interpretation/select_TCIA/tif.txt


