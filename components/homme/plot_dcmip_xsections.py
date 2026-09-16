"""DCMIP 2012 Test 1-1 daily equatorial cross-sections of Q, Q5.

Reads native-GLL NetCDF outputs and produces a multi-page PDF per case
with one page per daily snapshot showing side-by-side (lon, eta)
cross-sections of Q (SL) and Q5 (partmcsl) along the equator.

The unstructured GLL columns are interpolated onto a uniform longitude
target line at lat = 0 for every level via scipy.griddata (restricted
to a narrow lat strip around the equator to keep triangulation local).

Usage:
    python3 plot_dcmip_xsections.py FILE [--out FILE.pdf] \\
                                    [--nlon 360] [--lat-strip 6.0]
"""

import argparse
import numpy as np
import xarray as xr
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
from scipy.interpolate import griddata


def _fmt_day(d):
    d = float(d)
    if abs(d - round(d)) < 1e-3:
        return f'day {int(round(d))}'
    return f't = {d:.3f} d'


def _make_regrid(lon_src, lat_src, nlon, lat_strip):
    """Build source-point array (with periodic-lon extension across the
    dateline) and a target grid that spans lat = [-lat_strip, +lat_strip]
    with 5 rows.  Interpolation is done on the full 2D strip (not a 1D
    equator line), then row 2 (lat = 0) is extracted -- this avoids the
    Qhull degeneracy that hits when all sample points sit at lat = 0."""
    band = 20.0
    dup_w = lon_src < band
    dup_e = lon_src > (360.0 - band)
    lon_all = np.concatenate([lon_src,
                              lon_src[dup_w] + 360.0,
                              lon_src[dup_e] - 360.0])
    lat_all = np.concatenate([lat_src, lat_src[dup_w], lat_src[dup_e]])
    idx_map = np.concatenate([np.arange(len(lon_src)),
                              np.nonzero(dup_w)[0],
                              np.nonzero(dup_e)[0]])
    points = np.column_stack([lon_all, lat_all])
    lon_tgt = np.linspace(0.0, 360.0, nlon)
    lat_tgt = np.linspace(-lat_strip, lat_strip, 5)
    LON, LAT = np.meshgrid(lon_tgt, lat_tgt)
    return points, idx_map, LON, LAT, lat_tgt


def _regrid_equator(values_all_levels, points, idx_map, LON, LAT):
    """values_all_levels shape (lev, ncol) -> (lev, nlon) at lat=0.

    Interpolates each level onto a small (nlon, 5) strip centred on the
    equator and returns the centre row (lat = 0)."""
    nlev, _ = values_all_levels.shape
    nlon = LON.shape[1]
    out = np.empty((nlev, nlon))
    for k in range(nlev):
        v = values_all_levels[k][idx_map]
        Z = griddata(points, v, (LON, LAT), method='linear')
        out[k] = Z[LON.shape[0] // 2]              # lat = 0 row (middle of 5)
    return out


def plot_xsections(fn, output=None, times=None,
                   nlon=360, lat_strip=6.0, cmap='viridis',
                   ncontour=21):
    with xr.open_dataset(fn, decode_timedelta=False) as ds:
        nlev = ds.sizes['lev']
        ne   = int(ds.attrs.get('ne', -1))
        if output is None:
            output = f'dcmip_xsection_ne{ne}.pdf'

        lon_src = np.rad2deg(ds['lon'].values) % 360.0
        lat_src = np.rad2deg(ds['lat'].values)
        points, idx_map, LON, LAT, _ = _make_regrid(
            lon_src, lat_src, nlon, lat_strip)

        # Mid-level eta for the y-axis (etam = hyam + hybm, ps = p0 in this test)
        etam = (ds['hyam'].values + ds['hybm'].values)

        lon_tgt = LON[0]

        all_times = ds['time'].values
        if times is None:
            round_days = np.round(all_times).astype(int)
            keep = np.unique(round_days)
            times = [float(all_times[np.argmin(np.abs(all_times - t))])
                     for t in keep]

        Q_all  = np.stack([_regrid_equator(
                              ds['Q' ].sel(time=t, method='nearest').values,
                              points, idx_map, LON, LAT)
                           for t in times])                       # (nt, lev, nlon)
        Q5_all = np.stack([_regrid_equator(
                              ds['Q5'].sel(time=t, method='nearest').values,
                              points, idx_map, LON, LAT)
                           for t in times])
        vmin = float(np.nanmin([Q_all.min(), Q5_all.min()]))
        vmax = float(np.nanmax([Q_all.max(), Q5_all.max()]))
        contour_levels = np.linspace(vmin, vmax, ncontour)

        print(f'{fn}: ne={ne}, nlev={nlev}, lat_strip=+/-{lat_strip}deg, '
              f'{len(times)} snapshots, vrange=[{vmin:.4f}, {vmax:.4f}]')
        print(f'  writing {output}')

        LON2D, ETA2D = np.meshgrid(lon_tgt, etam)
        with PdfPages(output) as pdf:
            for it, t in enumerate(times):
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
                    ax.set_ylim(etam.max(), etam.min())   # eta increases downward
                    ax.set_xlabel('longitude (deg)')
                    ax.set_title(name)
                    ax.grid(True, alpha=0.3)
                axes[0].set_ylabel(r'$\eta$ (surface at bottom)')
                fig.suptitle(f'DCMIP 2012 1-1  ne={ne}, nlev={nlev}, '
                             f'equatorial cross-section  ({_fmt_day(t)})',
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
                    help='half-width in degrees of the equatorial band used '
                         'for the triangulation (default: 6.0)')
    args = ap.parse_args()
    for fn in args.files:
        out = args.out if len(args.files) == 1 else None
        plot_xsections(fn, output=out, nlon=args.nlon, lat_strip=args.lat_strip)


if __name__ == '__main__':
    main()
