#!/bin/sh
#BSUB -W 24:00
#BSUB -R "rusage[mem=6144]"
#BSUB -J "all_trees_rho_origin[1-5]"
module load java
SEED=$LSB_JOBINDEX

#analysis of the complete dataset
analysis=all_trees_rho_origin_dreamChallengeRange


#run analyses
java -jar /cluster/home/seidels/trees-in-devBio/organoid/origin_varScarHeight.jar -overwrite -statefile ${analysis}.${SEED}.state -seed $SEED  ${analysis}.xml > ${analysis}.${SEED}.out

