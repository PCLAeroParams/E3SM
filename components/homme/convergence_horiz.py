"""Horizontal convergence study: ||Q5 - Qref||_L2 vs ne (mesh size dx = 2*pi/(ne*np))."""

import math
import numpy as np
import xarray as xr
import matplotlib.pyplot as plt


def _fmt_day(d):
    """Format a time value (in the file's units, typically fractional days)
    as a readable label.  Integer days -> 'day N'; sub-day values that land
    on a whole hour -> 't = N h'; else 't = X.XX d'."""
    d = float(d)
    if d.is_integer():
        return f'day {int(d)}'
    hours = d * 24.0
    if 0.0 < hours < 24 and abs(hours - round(hours)) < 1e-3:
        return f't = {int(round(hours))} h'
    return f't = {d:.4g} d'


def horizontal_convergence(filenames,
                           days=tuple(range(13)),
                           reference='Q',
                           time_tol=0.1,
                           output='horizontal_convergence.pdf'):
    """Plot relative L2 error of Q5 against a reference vs horizontal resolution.

    Same per-snapshot L2 metric as vertical_convergence:

        err = sqrt( sum w * (Q5 - Qref)^2 ) / sqrt( sum w * Qref^2 )

    with w_{k,c} = (d hyai_k + d hybi_k) * area_c when the 'area' variable
    is present in the file, else d hyai_k + d hybi_k (uniform across
    columns).  ne and np are read from each file's global attributes.
    The approximate horizontal mesh size is dx = 2*pi / (ne * np); the
    plot's x-axis is ne, but pairwise convergence rates are computed in
    dx space:

        rate = log(err_coarse / err_fine) / log(dx_coarse / dx_fine).

    Reference modes:
        'Q'  -- Qref = Q at the same day (SL-advected q1).  Code-vs-code
                diff; only useful when SL is known to be closer to the
                exact solution than partmcsl.
        'Q7' -- Qref = Q7 at the same day.  Under the Q7 analytic-exact
                override in dcmip12_wrapper.F90 (grep
                'DIAGNOSTIC ONLY (Q7 analytic-exact)'), Q7 is the
                analytic SBR-rotated cosine-bell IC evaluated at FV
                centroids and projected back to GLL, so ||Q5 - Q7|| is
                partmcsl's true convergence error against the analytic
                exact solution.
        'ic' -- Qref = Q at day 0; meaningful at day 12 (round-trip
                error under DCMIP 1.1's non-divergent deformation),
                deformation magnitude at intermediate days.

    Args:
        filenames: NetCDF paths.  Each file must carry global 'ne' and
            'np' attributes.
        days: target time values (in the file's `time` variable units,
            typically fractional days for HOMME) at which to evaluate
            the error.
        reference: 'Q', 'Q7', or 'ic' (see above).
        time_tol: max |t - target| (in the file's time units) when
            locating a target time.  For sub-day snapshots (e.g. hourly
            output), reduce below 0.05 to avoid collapsing distinct
            snapshots onto the same target.
        output: path for the saved figure.

    Returns:
        dict {day: {'points': [(ne, err, dx), ...],
                    'rates':  [((ne_coarse, ne_fine), rate), ...]}}.
    """
    if reference not in ('Q', 'Q7', 'ic'):
        raise ValueError(f"reference must be 'Q', 'Q7', or 'ic', got {reference!r}")
    results = {d: {'points': []} for d in days}

    for fn in filenames:
        with xr.open_dataset(fn, decode_timedelta=False) as ds:
            ne = int(ds.attrs['ne'])
            npts = int(ds.attrs['np'])
            dx = 2.0 * math.pi / (ne * npts)
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
                    snap = ds.sel(time=day, method='nearest', tolerance=time_tol)
                except KeyError:
                    # No snapshot within tolerance -- silently skip this
                    # (ne, day) pair so a truncated file (e.g. wall-time-cut
                    # ne=60 run) doesn't abort the whole sweep.
                    continue
                if reference == 'ic':
                    Qref = Q_ic
                elif reference == 'Q7':
                    Qref = snap['Q7'].values
                else:
                    Qref = snap['Q'].values
                Q5   = snap['Q5'].values
                num = float(np.sqrt(np.sum(w * (Q5 - Qref) ** 2)))
                den = float(np.sqrt(np.sum(w *  Qref       ** 2)))
                err = num / den if den > 0 else num
                results[day]['points'].append((ne, err, dx))

    for day in days:
        pts = sorted(results[day]['points'], key=lambda p: p[0])
        results[day]['points'] = pts
        rates = []
        for (n0, e0, dx0), (n1, e1, dx1) in zip(pts[:-1], pts[1:]):
            if e0 > 0 and e1 > 0 and dx0 != dx1:
                rate = math.log(e0 / e1) / math.log(dx0 / dx1)
            else:
                rate = float('nan')
            rates.append(((n0, n1), rate))
        results[day]['rates'] = rates

    fig, ax = plt.subplots(figsize=(6, 5))
    for day in days:
        xs = [p[0] for p in results[day]['points']]
        ys = [p[1] for p in results[day]['points']]
        ax.loglog(xs, ys, marker='o', label=_fmt_day(day))
    ax.set_xlabel('ne (elements per cube edge)')
    if reference == 'ic':
        ref_label = 'Q(t=0)'
    elif reference == 'Q7':
        ref_label = 'Q_7'
    else:
        ref_label = 'Q'
    ax.set_ylabel(rf'$\|Q_5 - \mathrm{{{ref_label}}}\|_{{L^2}}'
                  rf' \,/\, \|\mathrm{{{ref_label}}}\|_{{L^2}}$')
    ax.set_title(f'PartMCSL horizontal convergence (ref = {ref_label})')
    ax.grid(True, which='both', linestyle=':')
    ax.legend()
    fig.tight_layout()
    fig.savefig(output)
    plt.close(fig)
    return results


if __name__ == '__main__':
    import argparse
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('files', nargs='+')
    p.add_argument('--reference', choices=('Q', 'Q7', 'ic'), default='ic',
                   help="Reference field for the L2 error.  'Q' is the SL "
                        "tracer (unreliable under the Test S SBR override); "
                        "'Q7' is the code-side analytic-exact (requires the "
                        "Q7 override in dcmip12_wrapper.F90); 'ic' compares "
                        "against Q at t=0 (only meaningful at day 12 for "
                        "DCMIP 1.1 round-trip).")
    p.add_argument('--days', type=float, nargs='+', default=None,
                   help="Target times (file's time units, typically fractional "
                        "days for HOMME) at which to evaluate the error.  "
                        "Default: 0..12 integer days.")
    p.add_argument('--hours', type=float, nargs='+', default=None,
                   help="Alternative to --days: target times in hours.  "
                        "Converted internally via /24 for HOMME's day units.")
    p.add_argument('--time-tol', type=float, default=0.1,
                   help="Max |t - target| in file's time units for snapshot "
                        "selection.  Reduce (e.g. 0.01) for sub-day targets.")
    p.add_argument('--output', default='horizontal_convergence.pdf')
    args = p.parse_args()

    if args.hours is not None and args.days is not None:
        p.error('pass at most one of --days / --hours')
    if args.hours is not None:
        days = tuple(h / 24.0 for h in args.hours)
    elif args.days is not None:
        days = tuple(args.days)
    else:
        days = tuple(range(13))

    out = horizontal_convergence(args.files, days=days,
                                 reference=args.reference,
                                 time_tol=args.time_tol,
                                 output=args.output)
    for day, info in out.items():
        pts = info['points']
        print(f"{_fmt_day(day)}: "
              + ", ".join(f"(ne={n}, err={e:.3e})" for n, e, _ in pts))
        for (n0, n1), r in info['rates']:
            print(f"  rate ne={n0}->{n1}: {r:.2f}")
