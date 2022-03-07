# run simulation
for simulationNr in `seq 1 100`
do
    
    for seed in `seq 0 9`
    do
	combined="${simulationNr}${seed}"
	echo $combined
	
	java -jar beast.jar -overwrite -seed ${combined} -D seqLength=10,outputDir="./simulationOutput/" simulateInputTrees.xml
    done
done
