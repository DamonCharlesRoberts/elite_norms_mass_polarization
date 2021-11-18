# Title: Power analysis for polarization and norms experiment

# Notes:
	#* Description: Script used to conduct power analysis for the proposed experimental design examining the role of norms on polarization
	#* Updated: 2021-11-18
	#* Updated by: dcr
# Setup:
	#* Load libraries
from statsmodels.stats.power import TTestIndPower
from matplotlib import pyplot as plt
from numpy import array
# Power analysis:
effect_sizes = array([0.2, 0.5, 0.95])
sample_sizes = array(range(5, 200))
analysis = TTestIndPower()
analysis.plot_power(dep_var ='nobs', nobs=sample_sizes, effect_size = effect_sizes)
plt.savefig('/Users/damonroberts/Dropbox/current_projects/dcr_elite_norms_mass_polarization/figures/power_analysis.jpeg')