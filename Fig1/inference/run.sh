#!/bin/sh

#analysis file
analysis=realistic_inference_1type

# path to alignment
for SEED in `seq 1 1000`
do
    #submit this on a cluster
    alignment="../realistic_simulation_1type_10trees_min2Samples.${SEED}.1.alignment.nexus"
    java -jar /cluster/home/seidels/trees-in-devBio/organoid/beast_origin.jar -overwrite -statefile ${analysis}.${SEED}.state -seed $SEED -D input_alignment=${alignment} ${analysis}.xml > ${analysis}.${SEED}.out
    
done
