"""DCMIP 2012 Test 1-1 daily equatorial cross-sections of Q, Q5.

Reads native-GLL NetCDF outputs and produces a multi-page PDF per case
with one page per daily snapshot showing side-by-side (lon, eta)
cross-sections of Q (SL) and Q5 (partmcsl) along the equator.

The unstructured GLL columns are regridded onto a small 2D strip
centred on the equator (lat = -strip .. +strip, 5 rows) via a cached
barycentric interpolator; the middle row (lat = 0) is extracted for
the plot.  The 2D strip avoids the Qhull precision failure that hits
when target points are colinear at lat = 0.

Usage:
    python3 plot_dcmip_xsections.py FILE [--out FILE.pdf] \\
                                    [--nlon 360] [--lat-strip 6.0]
"""

import argparse
import numpy as np
import xarray as xr
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages

from _gll_regrid import GllRegridder


def _fmt_day(d):
    d = float(d)
    if abs(d - round(d)) < 1e-3:
        return f'day {int(round(d))}'
    return f't = {d:.3f} d'


def plot_xsections(fn, output=None, times=None,
                   nlon=360, lat_strip=6.0, cmap='viridis',
                   ncontour=21, per_page_scale=True):
    with xr.open_dataset(fn, decode_timedelta=False) as ds:
        nlev = ds.sizes['lev']
        ne   = int(ds.attrs.get('ne', -1))
        if output is None:
            output = f'dcmip_xsection_ne{ne}.pdf'

        lon_src = ds['lon'].values % 360.0                    # already degrees_east
        lat_src = ds['lat'].values                             # already degrees_north
        lon_tgt = np.linspace(0.0, 360.0, nlon)
        lat_tgt = np.linspace(-lat_strip, lat_strip, 5)
        LON, LAT = np.meshgrid(lon_tgt, lat_tgt)
        eq_row = LON.shape[0] // 2
        R = GllRegridder(lon_src, lat_src, LON, LAT)

        etam = ds['hyam'].values + ds['hybm'].values          # ps = p0 in this test

        all_times = ds['time'].values
        if times is None:
            round_days = np.round(all_times).astype(int)
            keep = np.unique(round_days)
            times = [float(all_times[np.argmin(np.abs(all_times - t))])
                     for t in keep]

        # Regrid every (t, k) once and take the equator row.
        def _stack_eq(varname):
            out = np.empty((len(times), nlev, nlon))
            for it, t in enumerate(times):
                V = ds[varname].sel(time=t, method='nearest').values   # (lev, ncol)
                for k in range(nlev):
                    out[it, k] = R.interp(V[k])[eq_row]
            return out

        Q_all  = _stack_eq('Q')
        Q5_all = _stack_eq('Q5')

        vmin_g = float(np.nanmin([Q_all.min(), Q5_all.min()]))
        vmax_g = float(np.nanmax([Q_all.max(), Q5_all.max()]))

        print(f'{fn}: ne={ne}, nlev={nlev}, lat_strip=+/-{lat_strip}deg, '
              f'{len(times)} snapshots, global vrange=[{vmin_g:.4f}, {vmax_g:.4f}], '
              f'per-page scale={per_page_scale}')
        print(f'  writing {output}')

        LON2D, ETA2D = np.meshgrid(lon_tgt, etam)
        with PdfPages(output) as pdf:
            for it, t in enumerate(times):
                if per_page_scale:
                    vmin = float(min(Q_all[it].min(), Q5_all[it].min()))
                    vmax = float(max(Q_all[it].max(), Q5_all[it].max()))
                    if vmax - vmin < 1e-9:
                        vmax = vmin + 1e-9
                else:
                    vmin, vmax = vmin_g, vmax_g
                contour_levels = np.linspace(vmin, vmax, ncontour)
                fig, axes = plt.subplots(1, 2, figsize=(13, 4.6),
                                         sharex=True, sharey=True)
                im0 = axes[0].contourf(LON2D, ETA2D, Q_all[it],
                                       levels=contour_levels,
                                       cmap=cmap, vmin=vmin, vmax=vmax,
                                       extend='both')
                im1 = axes[1].contourf(LON2D, ETA2D, Q5_all[it],
                                       levels=contour_levels,
                                       cmap=cmap, vmin=vmin, vmax=vmax,
                                       extend='both')
                axes[0].contour(LON2D, ETA2D, Q_all[it],
                                levels=contour_levels,
                                colors='k', linewidths=0.3, alpha=0.3)
                axes[1].contour(LON2D, ETA2D, Q5_all[it],
                                levels=contour_levels,
                                colors='k', linewidths=0.3, alpha=0.3)
                for ax, name in zip(axes, ('Q (SL)', r'$Q_5$ (partmcsl)')):
                    ax.set_xlim(0, 360)
                    ax.set_xticks([0, 60, 120, 180, 240, 300, 360])
                    ax.set_ylim(etam.max(), etam.min())     # eta increases downward
                    ax.set_xlabel('longitude (deg)')
                    ax.set_title(name)
                    ax.grid(True, alpha=0.3)
                axes[0].set_ylabel(r'$\eta$ (surface at bottom)')
                fig.suptitle(f'DCMIP 2012 1-1  ne={ne}, nlev={nlev}, '
                             f'equatorial cross-section  '
                             f'({_fmt_day(t)}, vmax={vmax:.3f})',
                             fontsize=11)
                cb = fig.colorbar(im1, ax=axes, orientation='vertical',
                                  fraction=0.025, pad=0.02, shrink=0.95)
                cb.set_label('mixing ratio')
                pdf.savefig(fig)
                plt.close(fig)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('files', nargs='+')
    ap.add_argument('--out', default=None,
                    help='output PDF (default: dcmip_xsection_ne{NE}.pdf)')
    ap.add_argument('--nlon', type=int, default=360)
    ap.add_argument('--lat-strip', type=float, default=6.0,
                    help='half-width in degrees of the equatorial band '
                         'used for the triangulation target (default: 6.0)')
    ap.add_argument('--shared-scale', action='store_true',
                    help='use one color scale across all pages (default: '
                         'per-page scale, so decaying peaks stay visible)')
    args = ap.parse_args()
    for fn in args.files:
        out = args.out if len(args.files) == 1 else None
        plot_xsections(fn, output=out, nlon=args.nlon,
                       lat_strip=args.lat_strip,
                       per_page_scale=not args.shared_scale)


if __name__ == '__main__':
    main()
