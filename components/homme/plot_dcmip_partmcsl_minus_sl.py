"""Plot the PartMCSL-minus-SL difference field for the DCMIP 1-1 tracer pairs.

Since Q5, Q6, Q7 (PartMCSL) share their initial condition and prescribed
flow with Q, Q2, Q3 (SL), the difference `Q_pmcsl - Q_sl` at any snapshot
removes the physical signal and leaves only scheme-scheme differences.

Interpretation:
  - Smooth broad structure that peaks where the tracer has strong
    gradients: expected first-order truncation error.
  - Cell-scale speckle aligned with element or sub-element (nphys=2)
    spacing: fingerprint of an ordering / indexing / halo-exchange bug.

Usage:
    python3 plot_dcmip_partmcsl_minus_sl.py FILE [--level K] [--out FILE.pdf]
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


PAIRS = [('Q',  'Q5', 'cosine bell'),
         ('Q2', 'Q6', 'correlated 0.9-0.8*q1**2'),
         ('Q3', 'Q7', 'slotted ellipse')]


def plot_diffs(fn, level=None, output=None, times=None,
               nlon=360, nlat=181, ncontour=21):
    with xr.open_dataset(fn, decode_timedelta=False) as ds:
        nlev = ds.sizes['lev']
        ne   = int(ds.attrs.get('ne', -1))
        if level is None:
            Q0 = ds['Q'].isel(time=0).values
            lev_k = int(np.argmax(Q0.max(axis=1)))
        else:
            lev_k = int(level)
        if output is None:
            output = f'dcmip_partmcsl_minus_sl_ne{ne}_lev{lev_k}.pdf'

        lon_src = ds['lon'].values % 360.0
        lat_src = ds['lat'].values
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

        # Regrid every field once per time.
        diffs = {}   # name -> (nt, nlat, nlon) diff field
        sls   = {}   # for optional reference plotting
        pms   = {}
        for (sl_name, pm_name, _) in PAIRS:
            SL = R.interp_many(np.stack([
                ds[sl_name].sel(time=t, method='nearest').isel(lev=lev_k).values
                for t in times]))
            PM = R.interp_many(np.stack([
                ds[pm_name].sel(time=t, method='nearest').isel(lev=lev_k).values
                for t in times]))
            diffs[pm_name] = PM - SL
            sls  [sl_name] = SL
            pms  [pm_name] = PM

        eta_mid = float(ds['hyam'].isel(lev=lev_k) + ds['hybm'].isel(lev=lev_k))
        print(f'{fn}: ne={ne}, nlev={nlev}, lev_k={lev_k} '
              f'(eta_mid={eta_mid:.4f}), {len(times)} snapshots')
        print(f'  writing {output}')

        # Per-page diverging color scale, symmetric about zero.
        with PdfPages(output) as pdf:
            for it, t in enumerate(times):
                fig, axes = plt.subplots(1, 3, figsize=(15.5, 4.5),
                                         sharex=True, sharey=True)
                for ax, (sl_name, pm_name, desc) in zip(axes, PAIRS):
                    D = diffs[pm_name][it]
                    vabs = float(np.abs(D).max())
                    if vabs < 1e-12:
                        vabs = 1e-12
                    levels = np.linspace(-vabs, vabs, ncontour)
                    im = ax.contourf(LON, LAT, D, levels=levels,
                                     cmap='RdBu_r', vmin=-vabs, vmax=vabs,
                                     extend='both')
                    ax.contour(LON, LAT, D, levels=levels,
                               colors='k', linewidths=0.25, alpha=0.25)
                    ax.set_xlim(0, 360); ax.set_ylim(-90, 90)
                    ax.set_xticks([0, 60, 120, 180, 240, 300, 360])
                    ax.set_yticks([-90, -45, 0, 45, 90])
                    ax.set_xlabel('longitude (deg)')
                    ax.set_title(f'{pm_name} - {sl_name}  ({desc})\n'
                                 f'peak |diff|={vabs:.3g}',
                                 fontsize=10)
                    ax.grid(True, alpha=0.3)
                    cb = fig.colorbar(im, ax=ax, orientation='vertical',
                                      fraction=0.05, pad=0.02, shrink=0.9)
                    cb.set_label(f'{pm_name} - {sl_name}')
                axes[0].set_ylabel('latitude (deg)')
                fig.suptitle(f'DCMIP 2012 1-1  ne={ne}, nlev={nlev}, '
                             f'lev={lev_k} (eta={eta_mid:.3f})  '
                             f'PartMCSL - SL residual  ({_fmt_day(t)})',
                             fontsize=11)
                fig.tight_layout()
                pdf.savefig(fig)
                plt.close(fig)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('files', nargs='+')
    ap.add_argument('--level', type=int, default=None,
                    help='0-indexed vertical level (default: level where '
                         'IC Q1 peaks)')
    ap.add_argument('--out', default=None,
                    help='output PDF (default: '
                         'dcmip_partmcsl_minus_sl_ne{NE}_lev{K}.pdf)')
    ap.add_argument('--nlon', type=int, default=360)
    ap.add_argument('--nlat', type=int, default=181)
    args = ap.parse_args()
    for fn in args.files:
        out = args.out if len(args.files) == 1 else None
        plot_diffs(fn, level=args.level, output=out,
                   nlon=args.nlon, nlat=args.nlat)


if __name__ == '__main__':
    main()
