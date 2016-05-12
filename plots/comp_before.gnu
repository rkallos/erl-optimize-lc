set boxwidth 0.9 absolute
set style fill solid 1.00 border lt -1
set style histogram clustered gap 1 title textcolor lt -1
set term svg enhanced mouse size 800,600
set termoption noenhanced
set datafile missing '-'
set style data histograms
set xtics rotate by -45 scale 0
set logscale y 2
set key autotitle columnhead
set title "Heap Usage Before Test - R18 and Optimized Compiler"

plot for [COL=2:5] '../data/comp_before.dat' using COL:xticlabels(1)