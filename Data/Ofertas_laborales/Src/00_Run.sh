#!/bin/sh
for i in {0..39}
do
  echo "Looping ... number $i"
  Rscript --vanilla 01a_procesar_datos_elempleo.R $i
done
