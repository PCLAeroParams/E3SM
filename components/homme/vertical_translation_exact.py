"""Analytic-exact convergence check for the Vertical Translation Test.

The prescribed flow

    d eta / dt = w_amp * sin(2 * pi * eta_norm),
    eta_norm  = (eta - eta_top) / (1 - eta_top),

is separable, so a parcel initially at eta_norm_0 in the lower lobe
(0.5, 1) satisfies

    eta_norm(t) = 1 + arctan(tan(pi * eta_norm_0) * exp(alpha * t)) / pi,
    alpha       = 2 * pi * w_amp / (1 - eta_top).

Since the ODE is autonomous, the backward map is just t -> -t:

    eta_norm_0(eta_norm, t) = 1 + arctan(tan(pi * eta_norm) * exp(-alpha * t)) / pi.

The tracer mixing ratio is preserved along Lagrangian trajectories, so
the exact solution at (eta_norm, t) is the initial Gaussian evaluated at
eta_norm_0.  All columns are identical (horizontally uniform flow + IC),
so we work on a single column of interface eta values and broadcast.

Usage:
    python vertical_translation_exact.py FILE [FILE ...]
    python vertical_translation_exact.py --plot vt_convergence.pdf FILE...

Prints a table: file, snapshot time, nlev, L2 err(Q vs exact), L2 err(Q5
vs exact).  With --plot, also emits an nlev-vs-error plot combining all
files.

The flow parameters below must match the Fortran-side constants in
src/test_src/dcmip12_wrapper.F90.
"""

import argparse
import numpy as np
import xarray as xr


# --- must match Fortran (dcmip12_wrapper.F90 vt parameter block) --------
W_AMP_VT       = 2.0e-5
ETA_NORM0_VT   = 0.75
GAUSS_WIDTH_VT = 0.06
ETA_TOP        = 0.2549944          # matches vtop in partmcsl_vt.nl
# ------------------------------------------------------------------------


def backward_map(eta_norm, t):
    """eta_norm at time 0 given eta_norm at time t (lower lobe branch)."""
    alpha = 2.0 * np.pi * W_AMP_VT / (1.0 - ETA_TOP)
    return 1.0 + np.arctan(np.tan(np.pi * eta_norm) * np.exp(-alpha * t)) / np.pi


def analytic_q(eta_norm, t):
    """Analytic exact tracer at (eta_norm, t) — Gaussian in eta_norm_0."""
    eta_norm_0 = backward_map(eta_norm, t)
    return np.exp(-((eta_norm_0 - ETA_NORM0_VT) / GAUSS_WIDTH_VT) ** 2)


def cell_center_eta_norm(hyai, hybi):
    """FV cell-center eta_norm from midpoint eta = (etai_k + etai_{k+1})/2.

    Assumes ps = p0 so that eta = hyai + hybi (holds at t=0 and stays true
    because ps is prescribed constant in this test).
    """
    etai = hyai + hybi                            # (nlevp,)
    etam = 0.5 * (etai[1:] + etai[:-1])           # (nlev,)
    return (etam - ETA_TOP) / (1.0 - ETA_TOP)


def cell_averaged_q_exact(hyai, hybi, t, nsub=8):
    """Cell-average analytic q over each vertical cell (better than centroid
    for measuring donor-cell convergence, since the scheme itself represents
    cell means).  nsub sub-quadrature points per cell (Simpson-style)."""
    etai = hyai + hybi
    eta_norm_i = (etai - ETA_TOP) / (1.0 - ETA_TOP)     # (nlevp,)
    nlev = eta_norm_i.size - 1
    q_avg = np.empty(nlev)
    for k in range(nlev):
        lo, hi = eta_norm_i[k], eta_norm_i[k + 1]
        xs = np.linspace(lo, hi, nsub + 1)               # trap rule; smooth IC so plenty
        qs = analytic_q(xs, t)
        q_avg[k] = np.trapezoid(qs, xs) / (hi - lo)
    return q_avg


def l2_error(q_num, q_exact, w):
    """Relative L2 error with vertical weight w (proportional to dp)."""
    num = float(np.sqrt(np.sum(w * (q_num - q_exact) ** 2)))
    den = float(np.sqrt(np.sum(w *  q_exact         ** 2)))
    return num / den if den > 0 else num


def process_file(fn, verbose=True):
    """Return list of (nlev, t_seconds, err_Q, err_Q5) for one file."""
    rows = []
    with xr.open_dataset(fn, decode_timedelta=False) as ds:
        nlev = ds.sizes['lev']
        hyai = ds['hyai'].values
        hybi = ds['hybi'].values
        dp_w = (np.diff(hyai) + np.diff(hybi))                # (lev,); ps=p0 => = deta
        # time in seconds — output_timeunits=1 in the namelist is minutes;
        # xarray reads it as-is (float64) so we convert.
        time_units = ds['time'].attrs.get('units', 'days')
        if 'minute' in time_units:
            t_sec = ds['time'].values * 60.0
        elif 'hour' in time_units:
            t_sec = ds['time'].values * 3600.0
        elif 'day' in time_units:
            t_sec = ds['time'].values * 86400.0
        else:
            t_sec = ds['time'].values  # assume seconds

        for it, t in enumerate(t_sec):
            q_exact = cell_averaged_q_exact(hyai, hybi, float(t))
            # All columns identical; take the first, but also verify.
            Q  = ds['Q' ].isel(time=it).values      # (lev, ncol) after interp
            Q5 = ds['Q5'].isel(time=it).values
            # Reduce to per-column error then average (columns should be identical).
            Q_col  = Q .mean(axis=-1) if Q .ndim > 1 else Q
            Q5_col = Q5.mean(axis=-1) if Q5.ndim > 1 else Q5
            err_Q  = l2_error(Q_col,  q_exact, dp_w)
            err_Q5 = l2_error(Q5_col, q_exact, dp_w)
            rows.append((nlev, float(t), err_Q, err_Q5))

    if verbose:
        for nlev_, t, eQ, eQ5 in rows:
            print(f'  {fn}  nlev={nlev_:4d}  t={t:7.1f}s  err(Q)={eQ:.3e}  err(Q5)={eQ5:.3e}')
    return rows


def make_plot(all_rows, output):
    import matplotlib.pyplot as plt
    times = sorted({r[1] for r in all_rows})
    fig, ax = plt.subplots(1, 2, figsize=(11, 5), sharey=True)
    for a, col, label in [(ax[0], 2, 'Q (SL)'), (ax[1], 3, 'Q5 (partmcsl)')]:
        for t in times:
            pts = sorted([(r[0], r[col]) for r in all_rows if r[1] == t])
            xs = [p[0] for p in pts]
            ys = [p[1] for p in pts]
            a.loglog(xs, ys, 'o-', label=f't={t:.0f}s')
        a.set_xlabel('nlev'); a.set_title(label); a.grid(True, which='both', alpha=0.3)
        a.legend()
    ax[0].set_ylabel('relative L2 error vs analytic exact')
    # slope-1 reference line on the partmcsl panel
    if len(times) > 0:
        xs = np.array([20, 256], dtype=float)
        ref = 0.5 * xs**-1 * (all_rows[0][3] * all_rows[0][0])
        ax[1].loglog(xs, ref, 'k--', alpha=0.5, label='slope -1')
        ax[1].legend()
    fig.tight_layout()
    fig.savefig(output, dpi=150)
    print(f'wrote {output}')


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('files', nargs='+', help='NetCDF outputs (one per nlev)')
    ap.add_argument('--plot', default=None, help='write nlev-vs-err PDF here')
    args = ap.parse_args()

    all_rows = []
    for fn in args.files:
        all_rows.extend(process_file(fn))
    if args.plot:
        make_plot(all_rows, args.plot)


if __name__ == '__main__':
    main()
