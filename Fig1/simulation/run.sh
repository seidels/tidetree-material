#draw simulation parameters
for seed in `seq 1 1000`; do Rscript draw_simulation_params.R $seed
done                                          

# run simulation
for seed in `seq 1 1000`
do
rho=$(cat ./simParams/simParams_${seed}.csv| cut -d , -f54 | tail -1)
#echo $rho
# all other simulation parameters (ie scarring rates, birth rate) are read using read statements from within the xml file
java -jar tidetree.jar -overwrite -seed ${seed} -D rho=${rho},seqLength=10,outputDir="./simulationOutput/" realistic_simulation_1type.xml
done
