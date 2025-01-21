#!/bin/bash

#SBATCH --job-name=warm-fall01
#SBATCH --nodes=1
#SBATCH --ntasks=94
#SBATCH --time=01:30:00
#SBATCH --account=gornish
#SBATCH --partition=standard
#SBATCH -o %x_%A.out

# Set working directory
cd /groups/egornish/lossanna/RestoreNet/scripts/

# Load R
module load R/4.4.0

# Run R script
Rscript warm-fall01.R
