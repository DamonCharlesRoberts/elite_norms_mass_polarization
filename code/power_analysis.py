# Title: Power analysis for polarization and norms experiment

# Notes:
	#* Description: Script used to conduct power analysis for the proposed experimental design examining the role of norms on polarization
	#* Updated: 2021-11-18
	#* Updated by: dcr
# Setup:
	#* Load libraries
from statsmodels.stats.power import TTestIndPower

# Power analysis:
effect = 0.8
alpha = 0.01
power = 0.8
analysis = TTestIndPower()
result = analysis.solve_power(effect, power=power, nobs1=None, ratio = 1.0, alpha = alpha)
