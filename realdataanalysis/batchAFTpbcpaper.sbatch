#!/bin/bash
#SBATCH --job-name=PBCpaper                   # Job name
#SBATCH --cpus-per-task=1
#SBATCH --account=statistics
#SBATCH --qos=statistics
#SBATCH --mail-type=ALL                            # Mail events (NONE, BEGIN, END, FAIL, AL$
#SBATCH --mail-user=woojung.bae@ufl.edu    # Where to send mail
#SBATCH --mem=5G                                   # Memory limit
#SBATCH --partition=hpg-default
#SBATCH --time=10-00:00:00                          # Time limit hrs:min:sec
#SBATCH --output=outfiles/array_%A-%a_pbc.log  # the SLURM outputs will be created i$

date; hostname; pwd

module load gcc R

#Run R script
Rscript paper_pbc.R
