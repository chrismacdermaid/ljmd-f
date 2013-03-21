import ljmd
import numpy as np

pyfname = np.array('argon_108')

ljmd.interface.callopen(pyfname)

#ljmd.interface.set_natoms(108)		# natoms
#ljmd.interface.set_mass(39.948)	# mass in AMU
#ljmd.interface.set_epsilon(0.2379)	# epsilon in kcal/mol
#ljmd.interface.set_sigma(3.405)	# sigma in angstrom
#ljmd.interface.set_rcut(8.5)		# rcut in angstrom
#ljmd.interface.set_box(17.1580) 	# box length (in angstrom)
#ljmd.interface.set_nsteps(10000) 	# nr MD steps
#ljmd.interface.set_dt(5.0) 		# MD time step (in fs)
#ljmd.interface.set_nprint(100) 	# output print frequency

ljmd.interface.run()
