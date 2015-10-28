read -p "What file to plot?" file
gnuplot <<-EOF
set autoscale
set t wxt persist
set title "Approximation factor over cut"
plot "$file" using 1:2 title 'C avg' with linespoints
EOF
