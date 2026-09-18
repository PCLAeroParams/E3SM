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
              per_page_scale=True, per_panel_scale=False):
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

        has_q5 = 'Q5' in ds.variables
        Q_all = R.interp_many(np.stack([
            ds['Q' ].sel(time=t, method='nearest').isel(lev=lev_k).values
            for t in times]))
        if has_q5:
            Q5_all = R.interp_many(np.stack([
                ds['Q5'].sel(time=t, method='nearest').isel(lev=lev_k).values
                for t in times]))
            vmin_g = float(np.nanmin([Q_all.min(), Q5_all.min()]))
            vmax_g = float(np.nanmax([Q_all.max(), Q5_all.max()]))
        else:
            Q5_all = None
            vmin_g = float(Q_all.min())
            vmax_g = float(Q_all.max())

        eta_mid = float(ds['hyam'].isel(lev=lev_k) + ds['hybm'].isel(lev=lev_k))
        print(f'{fn}: ne={ne}, nlev={nlev}, lev_k={lev_k} '
              f'(eta_mid={eta_mid:.4f}), '
              f'{len(times)} snapshots, global vrange=[{vmin_g:.4f}, {vmax_g:.4f}], '
              f'per-page scale={per_page_scale}')
        print(f'  writing {output}')

        with PdfPages(output) as pdf:
            for it, t in enumerate(times):
                # Per-panel scale: each panel gets its own vmin/vmax + colorbar,
                # so structural detail stays visible regardless of the other
                # panel's peak.  Only meaningful when has_q5.
                if per_panel_scale and has_q5:
                    vminQ, vmaxQ = float(Q_all[it].min()),  float(Q_all[it].max())
                    vmin5, vmax5 = float(Q5_all[it].min()), float(Q5_all[it].max())
                    if vmaxQ - vminQ < 1e-9: vmaxQ = vminQ + 1e-9
                    if vmax5 - vmin5 < 1e-9: vmax5 = vmin5 + 1e-9
                    header_vmax = f'vmax: Q={vmaxQ:.3f}, Q5={vmax5:.3f}'
                else:
                    if per_page_scale:
                        if has_q5:
                            vmin = float(min(Q_all[it].min(), Q5_all[it].min()))
                            vmax = float(max(Q_all[it].max(), Q5_all[it].max()))
                        else:
                            vmin = float(Q_all[it].min())
                            vmax = float(Q_all[it].max())
                        if vmax - vmin < 1e-9:
                            vmax = vmin + 1e-9
                    else:
                        vmin, vmax = vmin_g, vmax_g
                    vminQ, vmaxQ = vmin, vmax
                    vmin5, vmax5 = vmin, vmax
                    header_vmax = f'vmax={vmax:.3f}'
                levelsQ = np.linspace(vminQ, vmaxQ, ncontour)
                levels5 = np.linspace(vmin5, vmax5, ncontour)
                ncols = 2 if has_q5 else 1
                fig, axes = plt.subplots(1, ncols, figsize=(13 if has_q5 else 7, 4.6),
                                         sharex=True, sharey=True, squeeze=False)
                axes = axes[0]
                im0 = axes[0].contourf(LON, LAT, Q_all[it], levels=levelsQ,
                                       cmap=cmap, vmin=vminQ, vmax=vmaxQ,
                                       extend='both')
                axes[0].contour(LON, LAT, Q_all[it], levels=levelsQ,
                                colors='k', linewidths=0.3, alpha=0.3)
                if has_q5:
                    im1 = axes[1].contourf(LON, LAT, Q5_all[it], levels=levels5,
                                           cmap=cmap, vmin=vmin5, vmax=vmax5,
                                           extend='both')
                    axes[1].contour(LON, LAT, Q5_all[it], levels=levels5,
                                    colors='k', linewidths=0.3, alpha=0.3)
                    panel_names = ('Q (SL)', r'$Q_5$ (partmcsl)')
                else:
                    panel_names = ('Q (SL)',)
                for ax, name in zip(axes, panel_names):
                    ax.set_xlim(0, 360); ax.set_ylim(-90, 90)
                    ax.set_xticks([0, 60, 120, 180, 240, 300, 360])
                    ax.set_yticks([-90, -45, 0, 45, 90])
                    ax.set_xlabel('longitude (deg)')
                    ax.set_title(name)
                    ax.grid(True, alpha=0.3)
                axes[0].set_ylabel('latitude (deg)')
                fig.suptitle(f'DCMIP 2012 1-1  ne={ne}, nlev={nlev}, '
                             f'lev={lev_k} (eta={eta_mid:.3f})  '
                             f'({_fmt_day(t)}, {header_vmax})', fontsize=11)
                if per_panel_scale and has_q5:
                    fig.colorbar(im0, ax=axes[0], orientation='vertical',
                                 fraction=0.05, pad=0.02, shrink=0.9,
                                 label='Q')
                    fig.colorbar(im1, ax=axes[1], orientation='vertical',
                                 fraction=0.05, pad=0.02, shrink=0.9,
                                 label=r'$Q_5$')
                else:
                    cb_source = im1 if has_q5 else im0
                    cb = fig.colorbar(cb_source, ax=axes, orientation='vertical',
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
    ap.add_argument('--per-panel-scale', action='store_true',
                    help='give Q and Q5 their own vmin/vmax + colorbar per '
                         'page, so the panel with the smaller peak is not '
                         'washed out')
    args = ap.parse_args()
    for fn in args.files:
        out = args.out if len(args.files) == 1 else None
        plot_maps(fn, level=args.level, output=out,
                  nlon=args.nlon, nlat=args.nlat,
                  per_page_scale=not args.shared_scale,
                  per_panel_scale=args.per_panel_scale)


if __name__ == '__main__':
    main()
