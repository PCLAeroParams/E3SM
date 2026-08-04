"""Vertical convergence study: ||Q5 - Q||_L2 vs nlev."""

import numpy as np
import xarray as xr
import matplotlib.pyplot as plt


def vertical_convergence(filenames,
                         days=tuple(range(13)),
                         reference='Q',
                         time_tol=0.1,
                         output='vertical_convergence.pdf'):
    """Plot relative L2 error of Q5 against a reference vs vertical resolution.

    For each file, selects the time nearest each requested day, then
    computes

        err(day) = sqrt( sum_{k,c} w_{k,c} * (Q5 - Qref)^2 )
                 / sqrt( sum_{k,c} w_{k,c} *       Qref^2  )

    with w_{k,c} = (d(hyai_k) + d(hybi_k)) * area_c when the 'area'
    variable is present, else just d(hyai_k) + d(hybi_k) (uniform across
    columns).  The vertical factor is proportional to dp under the
    ps = ps0 assumption that holds for the DCMIP 1.1 idealized ps field.

    Reference modes:
        'Q'  -- Qref = Q at the same day.  Measures partmcsl drift from
                the dycore tracer; bakes in both schemes' errors.
        'ic' -- Qref = Q at day 0 (the IC).  For DCMIP 1.1's non-divergent
                deformation, the analytic truth at day 12 is the IC, so
                err at day 12 is partmcsl's round-trip error.  For
                intermediate days the field is deformed, so the value is
                "distance from the original shape" rather than a true
                error.

    Args:
        filenames: iterable of NetCDF paths, each with Q, Q5, hyai, hybi,
            time, and a 'lev' dimension.
        days: model days at which to evaluate the error.
        reference: 'Q' or 'ic' (see above).
        time_tol: max |t - day| (in the file's time units) when locating
            a target day.
        output: path for the saved figure.

    Returns:
        dict {day: list of (nlev, err)}, sorted by nlev.
    """
    if reference not in ('Q', 'ic'):
        raise ValueError(f"reference must be 'Q' or 'ic', got {reference!r}")
    results = {d: [] for d in days}

    for fn in filenames:
        with xr.open_dataset(fn, decode_timedelta=False) as ds:
            nlev = ds.sizes['lev']
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
                snap = ds.sel(time=day, method='nearest', tolerance=time_tol)
                Qref = Q_ic if reference == 'ic' else snap['Q'].values
                Q5   = snap['Q5'].values
                num = float(np.sqrt(np.sum(w * (Q5 - Qref) ** 2)))
                den = float(np.sqrt(np.sum(w *  Qref       ** 2)))
                err = num / den if den > 0 else num
                results[day].append((nlev, err))

    for day in days:
        results[day].sort(key=lambda p: p[0])

    fig, ax = plt.subplots(figsize=(6, 5))
    for day in days:
        xs = [p[0] for p in results[day]]
        ys = [p[1] for p in results[day]]
        ax.loglog(xs, ys, marker='o', label=f'day {day}')
    ax.set_xlabel('number of vertical levels')
    ref_label = 'Q(t=0)' if reference == 'ic' else 'Q'
    ax.set_ylabel(rf'$\|Q_5 - \mathrm{{{ref_label}}}\|_{{L^2}}'
                  rf' \,/\, \|\mathrm{{{ref_label}}}\|_{{L^2}}$')
    ax.set_title(f'PartMCSL vertical convergence (ref = {ref_label})')
    ax.grid(True, which='both', linestyle=':')
    ax.legend()
    fig.tight_layout()
    fig.savefig(output)
    plt.close(fig)
    return results


if __name__ == '__main__':
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument('files', nargs='+')
    p.add_argument('--reference', choices=('Q', 'ic'), default='Q')
    p.add_argument('--output', default='vertical_convergence.pdf')
    args = p.parse_args()
    out = vertical_convergence(args.files, reference=args.reference,
                               output=args.output)
    for day, pts in out.items():
        print(f"day {day}: " + ", ".join(f"(nlev={n}, err={e:.3e})" for n, e in pts))
