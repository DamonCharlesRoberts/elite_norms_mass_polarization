# Title: Power analysis for polarization and norms experiment

# Notes:
	#* Description: Script used to conduct power analysis for the proposed experimental design examining the role of norms on polarization
	#* Updated: 2021-11-18
	#* Updated by: dcr
# Setup:
	#* Load libraries
import os
from statsmodels.stats.power import TTestIndPower
from matplotlib import pyplot as plt
import seaborn as sns
from numpy import arange
from numpy import array
	#* Working Directory
os.chdir('/home/damoncroberts/Dropbox/current_projects/dcr_elite_norms_mass_polarization')
	#* Set plot theme
sns.set_theme(style = 'whitegrid')
sns.color_palette('Greys_r')
# Power analysis:
effect_sizes = list(arange(0.1, 5, .5))
sample_sizes = array(arange(5, 200))
analysis = TTestIndPower()
analysis.plot_power(dep_var ='nobs', nobs=sample_sizes, effect_size = effect_sizes)
plt.savefig('figures/power_analysis.jpeg')