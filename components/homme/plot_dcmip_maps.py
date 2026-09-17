"""DCMIP 2012 Test 1-1 daily mid-level maps of Q (SL) and Q5 (partmcsl).

Reads native-GLL NetCDF outputs and produces a multi-page PDF per case
with one page per daily snapshot showing side-by-side (lat, lon)
contourf plots of Q and Q5 at a chosen vertical level (default =
mid-level).  Regrids the unstructured GLL columns onto a uniform
(nlon, nlat) mesh via a cached barycentric interpolator so the PDF
size is independent of the source resolution.

Usage:
    python3 plot_dcmip_maps.py FILE [--level K] [--out FILE.pdf] \\
                              [--nlon 360] [--nlat 181]
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


def plot_maps(fn, level=None, output=None, times=None,
              nlon=360, nlat=181, cmap='viridis', ncontour=21,
              per_page_scale=True):
    with xr.open_dataset(fn, decode_timedelta=False) as ds:
        nlev = ds.sizes['lev']
        ne   = int(ds.attrs.get('ne', -1))
        # Default level: the level where the IC tracer peaks (rather than
        # nlev//2, which for the DCMIP test can sit above the tracer's
        # vertical envelope and give an empty-looking slice).
        if level is None:
            Q0 = ds['Q'].isel(time=0).values
            lev_k = int(np.argmax(Q0.max(axis=1)))
        else:
            lev_k = int(level)
        if output is None:
            output = f'dcmip_maps_ne{ne}_lev{lev_k}.pdf'

        lon_src = ds['lon'].values % 360.0                    # already degrees_east
        lat_src = ds['lat'].values                             # already degrees_north
        lon_tgt = np.linspace(0.0, 360.0, nlon)
        lat_tgt = np.linspace(-90.0, 90.0, nlat)
        LON, LAT = np.meshgrid(lon_tgt, lat_tgt)
        R = GllRegridder(lon_src, lat_src, LON, LAT)

        all_times = ds['time'].values
        if times is None:
            round_days = np.round(all_times).astype(int)
            keep = np.unique(round_days)
            times = [float(all_times[np.argmin(np.abs(all_times - t))])
                     for t in keep]

        Q_all = R.interp_many(np.stack([
            ds['Q' ].sel(time=t, method='nearest').isel(lev=lev_k).values
            for t in times]))
        Q5_all = R.interp_many(np.stack([
            ds['Q5'].sel(time=t, method='nearest').isel(lev=lev_k).values
            for t in times]))
        vmin_g = float(np.nanmin([Q_all.min(), Q5_all.min()]))
        vmax_g = float(np.nanmax([Q_all.max(), Q5_all.max()]))

        eta_mid = float(ds['hyam'].isel(lev=lev_k) + ds['hybm'].isel(lev=lev_k))
        print(f'{fn}: ne={ne}, nlev={nlev}, lev_k={lev_k} '
              f'(eta_mid={eta_mid:.4f}), '
              f'{len(times)} snapshots, global vrange=[{vmin_g:.4f}, {vmax_g:.4f}], '
              f'per-page scale={per_page_scale}')
        print(f'  writing {output}')

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
                im0 = axes[0].contourf(LON, LAT, Q_all[it],  levels=contour_levels,
                                       cmap=cmap, vmin=vmin, vmax=vmax,
                                       extend='both')
                im1 = axes[1].contourf(LON, LAT, Q5_all[it], levels=contour_levels,
                                       cmap=cmap, vmin=vmin, vmax=vmax,
                                       extend='both')
                axes[0].contour(LON, LAT, Q_all[it],  levels=contour_levels,
                                colors='k', linewidths=0.3, alpha=0.3)
                axes[1].contour(LON, LAT, Q5_all[it], levels=contour_levels,
                                colors='k', linewidths=0.3, alpha=0.3)
                for ax, name in zip(axes, ('Q (SL)', r'$Q_5$ (partmcsl)')):
                    ax.set_xlim(0, 360); ax.set_ylim(-90, 90)
                    ax.set_xticks([0, 60, 120, 180, 240, 300, 360])
                    ax.set_yticks([-90, -45, 0, 45, 90])
                    ax.set_xlabel('longitude (deg)')
                    ax.set_title(name)
                    ax.grid(True, alpha=0.3)
                axes[0].set_ylabel('latitude (deg)')
                fig.suptitle(f'DCMIP 2012 1-1  ne={ne}, nlev={nlev}, '
                             f'lev={lev_k} (eta={eta_mid:.3f})  '
                             f'({_fmt_day(t)}, vmax={vmax:.3f})', fontsize=11)
                cb = fig.colorbar(im1, ax=axes, orientation='vertical',
                                  fraction=0.025, pad=0.02, shrink=0.95)
                cb.set_label('mixing ratio')
                pdf.savefig(fig)
                plt.close(fig)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('files', nargs='+')
    ap.add_argument('--level', type=int, default=None,
                    help='0-indexed vertical level (default: nlev/2)')
    ap.add_argument('--out', default=None,
                    help='output PDF (default: dcmip_maps_ne{NE}_lev{K}.pdf)')
    ap.add_argument('--nlon', type=int, default=360)
    ap.add_argument('--nlat', type=int, default=181)
    ap.add_argument('--shared-scale', action='store_true',
                    help='use one color scale across all pages (default: '
                         'per-page scale, so decaying peaks stay visible)')
    args = ap.parse_args()
    for fn in args.files:
        out = args.out if len(args.files) == 1 else None
        plot_maps(fn, level=args.level, output=out,
                  nlon=args.nlon, nlat=args.nlat,
                  per_page_scale=not args.shared_scale)


if __name__ == '__main__':
    main()
