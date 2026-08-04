"""Time convergence study: ||Q5 - Q||_L2 vs tstep at fixed mesh.

Companion to ``convergence_horiz.py``.  Same per-snapshot L2 metric and
the same set of reference modes; the only differences are:

- The x-axis is ``tstep`` (dynamics time step, seconds) rather than
  ``ne``.  ``tstep`` is **not** stored in the NetCDF, so it must be
  passed alongside each filename.
- Pairwise convergence rates are computed in ``tstep`` space:

      rate = log(err_coarse / err_fine) / log(tstep_coarse / tstep_fine).

- The sweep is meant to be run at fixed ``ne`` and (typically) fixed
  ``ndays``.  We warn if the input files report differing ``ne``.
"""

import math
import warnings
import numpy as np
import xarray as xr
import matplotlib.pyplot as plt


def time_convergence(files_tsteps,
                     days=tuple(range(13)),
                     reference='Q',
                     time_tol=0.1,
                     output='time_convergence.pdf'):
    """Plot relative L2 error of Q5 against a reference vs time step.

    Args:
        files_tsteps: iterable of (filename, tstep_seconds) pairs.  Each
            file must carry global ``ne`` and ``np`` attributes (used
            only to confirm the mesh is fixed across the sweep).
        days: model days at which to evaluate the error.  Days not
            present in a given file (within ``time_tol``) are skipped
            for that file.
        reference: 'Q' or 'ic' (see ``convergence_horiz.py``).
        time_tol: max |t - day| (in the file's time units) when locating
            a target day.
        output: path for the saved figure.

    Returns:
        dict {day: {'points': [(tstep, err, ne), ...],
                    'rates':  [((tstep_coarse, tstep_fine), rate), ...]}}.
    """
    if reference not in ('Q', 'ic'):
        raise ValueError(f"reference must be 'Q' or 'ic', got {reference!r}")
    results = {d: {'points': []} for d in days}
    seen_ne = set()

    for fn, tstep in files_tsteps:
        with xr.open_dataset(fn, decode_timedelta=False) as ds:
            ne = int(ds.attrs['ne'])
            seen_ne.add(ne)
            wv = (np.diff(ds['hyai'].values)
                  + np.diff(ds['hybi'].values))[:, None]   # (lev, 1)
            if 'area' in ds.variables:
                w = wv * ds['area'].values[None, :]        # (lev, ncol)
            else:
                w = wv                                     # (lev, 1)

            if reference == 'ic':
                ic_snap = ds.sel(time=0, method='nearest', tolerance=time_tol)
                Q_ic = ic_snap['Q'].values

            for day in days:
                try:
                    snap = ds.sel(time=day, method='nearest',
                                  tolerance=time_tol)
                except (KeyError, ValueError):
                    continue
                Qref = Q_ic if reference == 'ic' else snap['Q'].values
                Q5 = snap['Q5'].values
                num = float(np.sqrt(np.sum(w * (Q5 - Qref) ** 2)))
                den = float(np.sqrt(np.sum(w * Qref ** 2)))
                err = num / den if den > 0 else num
                results[day]['points'].append((tstep, err, ne))

    if len(seen_ne) > 1:
        warnings.warn(
            f"time_convergence: input files report multiple ne values "
            f"{sorted(seen_ne)}; time-only convergence assumes fixed mesh."
        )

    for day in days:
        # sort by tstep descending (coarse-to-fine reads naturally left-to-right
        # on a log axis, but pairwise rate is the same in either order).
        pts = sorted(results[day]['points'], key=lambda p: -p[0])
        results[day]['points'] = pts
        rates = []
        for (t0, e0, _), (t1, e1, _) in zip(pts[:-1], pts[1:]):
            if e0 > 0 and e1 > 0 and t0 != t1:
                rate = math.log(e0 / e1) / math.log(t0 / t1)
            else:
                rate = float('nan')
            rates.append(((t0, t1), rate))
        results[day]['rates'] = rates

    fig, ax = plt.subplots(figsize=(6, 5))
    for day in days:
        pts = results[day]['points']
        if not pts:
            continue
        xs = [p[0] for p in pts]
        ys = [p[1] for p in pts]
        ax.loglog(xs, ys, marker='o', label=f'day {day}')
    ax.set_xlabel('tstep (seconds)')
    ref_label = 'Q(t=0)' if reference == 'ic' else 'Q'
    ax.set_ylabel(rf'$\|Q_5 - \mathrm{{{ref_label}}}\|_{{L^2}}'
                  rf' \,/\, \|\mathrm{{{ref_label}}}\|_{{L^2}}$')
    ne_label = (f'ne={next(iter(seen_ne))}' if len(seen_ne) == 1
                else f'ne={sorted(seen_ne)}')
    ax.set_title(f'PartMCSL time convergence ({ne_label}, ref = {ref_label})')
    ax.grid(True, which='both', linestyle=':')
    ax.legend()
    fig.tight_layout()
    fig.savefig(output)
    plt.close(fig)
    return results


if __name__ == '__main__':
    import argparse
    p = argparse.ArgumentParser(
        description="Time convergence: pass FILE TSTEP pairs.",
        epilog="example: convergence_time.py "
               "run33/dcmip2012_test1_11.nc 33 "
               "run165/dcmip2012_test1_11.nc 16.5 "
               "run0825/dcmip2012_test1_11.nc 8.25"
    )
    p.add_argument('args', nargs='+',
                   help='alternating FILE TSTEP FILE TSTEP ...')
    p.add_argument('--reference', choices=('Q', 'ic'), default='Q')
    p.add_argument('--output', default='time_convergence.pdf')
    args = p.parse_args()

    if len(args.args) % 2 != 0:
        p.error('args must be alternating FILE TSTEP pairs (even count).')
    files_tsteps = [(args.args[i], float(args.args[i + 1]))
                    for i in range(0, len(args.args), 2)]

    out = time_convergence(files_tsteps, reference=args.reference,
                           output=args.output)
    for day, info in out.items():
        pts = info['points']
        if not pts:
            continue
        print(f"day {day}: "
              + ", ".join(f"(tstep={t}, err={e:.3e})" for t, e, _ in pts))
        for (t0, t1), r in info['rates']:
            print(f"  rate tstep={t0}->{t1}: {r:.2f}")
