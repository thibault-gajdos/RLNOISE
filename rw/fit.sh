#!/bin/bash
#SBATCH --mem-per-cpu=8G
#SBATCH --ntasks-per-node=4
#SBATCH --array=1-30  # nous demandons ici à SLURM de créer n jobs (notre script sera lancé n fois)
#SBATCH --output=./output/rw_%a.log
#SBATCH --error=./error/rw_%a.err
#SBATCH --mail-user=thibault.gajdos@gmail.com   # Where to send mail
#SBATCH --mail-type=END,FAIL          # Mail events (NONE, BEGIN, END, FAIL, ALL)

# lancement de l'analyse
srun Rscript --vanilla rw.r $SLURM_ARRAY_TASK_ID

