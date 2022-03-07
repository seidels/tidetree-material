module load java

# combine logs
$HOME/beast/bin/logcombiner -log all_trees_rho_origin_reparScarClock_treeInit25_PriorOnScarringRatesI_scarringStart_clock_ExpPriorScarringStart.*.log -o all_trees_rho_origin_reparScarClock_treeInit25_PriorOnScarringRatesI_scarringStart_clock_ExpPriorScarringStart_combined.log -b 10 -resample 1000000


# tree processing
for file in all_trees_rho_origin_reparScarClock_treeInit25_PriorOnScarringRatesI_scarringStart_clock_ExpPriorScarringStart.tree.*.1.trees
do
    name="${file%.*}"; secname="${name%.*}"
    echo $secname

    # combine tree logs
    /cluster/home/seidels/beast/bin/logcombiner -b 10 -log ${secname}.1.trees -log ${secname}.2.trees -log ${secname}.3.trees -log ${secname}.4.trees -log ${secname}.5.trees -o ${secname}.combined.trees

    #construct 95% HPD tree interval
    java -cp /cluster/home/seidels/beast/lib/beast.jar beast.evolution.tree.TreeTraceAnalysis ${secname}.combined.trees > ${secname}.hpd.trees

    # extract MCC tree
    $HOME/beast/bin/treeannotator -heights mean ${secname}.combined.trees ${secname}.mcc.tree
done

