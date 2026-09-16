"""Regenerate the horizontal and vertical convergence plots (PMCSL Q5).

Produces:
  convergence_horizontal_sbr.pdf   -- ||Q5-Q7||_L2/||Q7||_L2 at t=6h vs ne
  convergence_vertical_vt.pdf      -- ||Q5-Q_ex||_L2/||Q_ex||_L2 at t=1h vs nlev

Q7 is the analytic SBR-rotated Gaussian-hills IC evaluated at FV cell
centroids and projected to GLL (see set_pg_q7_analytic_exact in
dcmip12_wrapper.F90).  Q_ex is the analytic Vertical-Translation solution
from vertical_translation_exact.cell_averaged_q_exact.
"""

import math
import numpy as np
import xarray as xr
import matplotlib.pyplot as plt

from vertical_translation_exact import cell_averaged_q_exact


HORIZ_FILES = [
    ('movies_sbr_3h/dcmip2012_test1_11_sweep_ne16.nc',  16),
    ('movies_sbr_3h/dcmip2012_test1_11_sweep_ne30.nc',  30),
    ('movies_sbr_3h/dcmip2012_test1_11_sweep_ne60.nc',  60),
    ('movies_sbr_3h/dcmip2012_test1_11_sweep_ne120.nc', 120),
]

VERT_FILES = [
    ('movies_vt_nlev20/dcmip2012_test1_vt1.nc',  20),
    ('movies_vt_nlev64/dcmip2012_test1_vt1.nc',  64),
    ('movies_vt_nlev128/dcmip2012_test1_vt1.nc', 128),
    ('movies_vt_nlev256/dcmip2012_test1_vt1.nc', 256),
]

HORIZ_HOUR = 6.0        # error time
VERT_SEC   = 3600.0     # error time


def horizontal_convergence(files=HORIZ_FILES,
                           output='convergence_horizontal_sbr.pdf'):
    pts = []                                                # (ne, err, dx)
    for fn, ne_expected in files:
        with xr.open_dataset(fn, decode_timedelta=False) as ds:
            ne   = int(ds.attrs['ne'])
            npts = int(ds.attrs['np'])
            assert ne == ne_expected
            dx   = 2.0 * math.pi / (ne * npts)
            wv   = (np.diff(ds['hyai'].values)
                    + np.diff(ds['hybi'].values))[:, None]
            w    = wv * (ds['area'].values[None, :] if 'area' in ds.variables
                         else 1.0)
            snap = ds.sel(time=HORIZ_HOUR / 24.0, method='nearest',
                          tolerance=0.02)
            Q5   = snap['Q5'].values
            Q7   = snap['Q7'].values                        # analytic exact
            num  = float(np.sqrt(np.sum(w * (Q5 - Q7) ** 2)))
            den  = float(np.sqrt(np.sum(w *  Q7       ** 2)))
            pts.append((ne, num / den, dx))
    pts.sort()

    fig, ax = plt.subplots(figsize=(6, 4.8))
    xs = np.array([p[0]  for p in pts])
    ys = np.array([p[1]  for p in pts])
    ax.loglog(xs, ys, 'o-', color='C0', lw=2, ms=8,
              label='partmcsl $Q_5$')
    # first-order reference through the coarsest point
    ax.loglog(xs, ys[0] * (xs[0] / xs), 'k--', alpha=0.5,
              label='slope $-1$')
    ax.set_xticks(xs); ax.set_xticklabels([str(int(x)) for x in xs])
    ax.set_xlabel('ne (spectral elements per cube edge)')
    ax.set_ylabel(r'$\|Q_5 - Q_{\rm exact}\|_{L^2}\,/\,'
                  r'\|Q_{\rm exact}\|_{L^2}$')
    ax.set_title('SBR horizontal convergence  (t = 6 h, nlev = 20)')
    ax.grid(True, which='both', alpha=0.3)
    ax.legend()
    # annotate pairwise rates
    for (n0, e0, dx0), (n1, e1, dx1) in zip(pts[:-1], pts[1:]):
        rate = math.log(e0 / e1) / math.log(dx0 / dx1)
        ax.annotate(f'rate = {rate:.2f}',
                    xy=(math.sqrt(n0*n1), math.sqrt(e0*e1)),
                    xytext=(6, 6), textcoords='offset points',
                    fontsize=8, alpha=0.7)
    fig.tight_layout()
    fig.savefig(output, dpi=150)
    plt.close(fig)
    print(f'wrote {output}')
    print('  ' + '   '.join(f'ne={n}: err={e:.3e}' for n, e, _ in pts))


def vertical_convergence(files=VERT_FILES,
                         output='convergence_vertical_vt.pdf'):
    pts = []                                                # (nlev, err)
    for fn, nlev_expected in files:
        with xr.open_dataset(fn, decode_timedelta=False) as ds:
            nlev = ds.sizes['lev']
            assert nlev == nlev_expected
            hyai = ds['hyai'].values
            hybi = ds['hybi'].values
            dp_w = np.diff(hyai) + np.diff(hybi)            # (lev,)
            snap = ds.sel(time=VERT_SEC / 86400.0, method='nearest',
                          tolerance=1e-3)
            Q5   = snap['Q5'].mean('ncol').values           # all columns identical
            Q_ex = cell_averaged_q_exact(hyai, hybi, VERT_SEC)
            num  = float(np.sqrt(np.sum(dp_w * (Q5 - Q_ex) ** 2)))
            den  = float(np.sqrt(np.sum(dp_w *  Q_ex        ** 2)))
            pts.append((nlev, num / den))
    pts.sort()

    fig, ax = plt.subplots(figsize=(6, 4.8))
    xs = np.array([p[0] for p in pts])
    ys = np.array([p[1] for p in pts])
    ax.loglog(xs, ys, 'o-', color='C0', lw=2, ms=8,
              label='partmcsl $Q_5$')
    # second-order reference through the second-coarsest point (nlev=64)
    ref_i = 1
    ax.loglog(xs, ys[ref_i] * (xs[ref_i] / xs) ** 2,
              'k--', alpha=0.5, label='slope $-2$')
    ax.set_xticks(xs); ax.set_xticklabels([str(int(x)) for x in xs])
    ax.set_xlabel('nlev (vertical levels)')
    ax.set_ylabel(r'$\|Q_5 - Q_{\rm exact}\|_{L^2}\,/\,'
                  r'\|Q_{\rm exact}\|_{L^2}$')
    ax.set_title('Vertical Translation convergence  (t = 1 h, ne = 16)')
    ax.grid(True, which='both', alpha=0.3)
    ax.legend()
    for (n0, e0), (n1, e1) in zip(pts[:-1], pts[1:]):
        rate = math.log(e0 / e1) / math.log(n1 / n0)
        ax.annotate(f'rate = {rate:.2f}',
                    xy=(math.sqrt(n0*n1), math.sqrt(e0*e1)),
                    xytext=(6, 6), textcoords='offset points',
                    fontsize=8, alpha=0.7)
    fig.tight_layout()
    fig.savefig(output, dpi=150)
    plt.close(fig)
    print(f'wrote {output}')
    print('  ' + '   '.join(f'nlev={n}: err={e:.3e}' for n, e in pts))


if __name__ == '__main__':
    horizontal_convergence()
    vertical_convergence()
