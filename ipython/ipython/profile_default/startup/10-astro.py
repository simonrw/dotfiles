ip = get_ipython()

def import_astropy(self, arg):
    ip.ex('import astropy')
    ip.ex('from astropy import coordinates as coords')
    ip.ex('import astropy.units as u')
    ip.ex('from astropy.io import fits')

ip.define_magic('astro', import_astropy)
