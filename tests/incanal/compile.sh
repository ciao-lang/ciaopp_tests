#!/bin/sh

bench_driver=incanal_intermod_bench_driver
sum_driver=summarize_stat
norm_driver=prolog_to_table
gather_driver=gather_stats

echo "Compiling..."
ciaoc -x $bench_driver
ciaoc -x $sum_driver
ciaoc -x $norm_driver
ciaoc -x $gather_driver
