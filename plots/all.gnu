set boxwidth 0.9 absolute
set style fill solid 1.00 border lt -1
set style histogram clustered gap 1 title textcolor lt -1
set datafile missing '-'
set style data histograms
#set xtics border in scale 0,0 nomirror rotate by -45 autojustify
#set xtics norangelimit
#set xtics ()
set logscale y 2
set title "Pre- and post-optimization Heap Usage"
x = 0.0

plot for [COL=2:5] '../data/all.dat' using COL:xticlabels(1)