"""Horizontal convergence study: ||Q5 - Q||_L2 vs ne (mesh size dx = 2*pi/(ne*np))."""

import math
import numpy as np
import xarray as xr
import matplotlib.pyplot as plt


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
        'Q'  -- Qref = Q at the same day.
        'ic' -- Qref = Q at day 0; meaningful at day 12 (round-trip
                error under DCMIP 1.1's non-divergent deformation),
                deformation magnitude at intermediate days.

    Args:
        filenames: NetCDF paths.  Each file must carry global 'ne' and
            'np' attributes.
        days: model days at which to evaluate the error.
        reference: 'Q' or 'ic' (see above).
        time_tol: max |t - day| (in the file's time units) when locating
            a target day.
        output: path for the saved figure.

    Returns:
        dict {day: {'points': [(ne, err, dx), ...],
                    'rates':  [((ne_coarse, ne_fine), rate), ...]}}.
    """
    if reference not in ('Q', 'ic'):
        raise ValueError(f"reference must be 'Q' or 'ic', got {reference!r}")
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
                snap = ds.sel(time=day, method='nearest', tolerance=time_tol)
                Qref = Q_ic if reference == 'ic' else snap['Q'].values
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
        ax.loglog(xs, ys, marker='o', label=f'day {day}')
    ax.set_xlabel('ne (elements per cube edge)')
    ref_label = 'Q(t=0)' if reference == 'ic' else 'Q'
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
    p = argparse.ArgumentParser()
    p.add_argument('files', nargs='+')
    p.add_argument('--reference', choices=('Q', 'ic'), default='ic')
    p.add_argument('--output', default='horizontal_convergence.pdf')
    args = p.parse_args()
    out = horizontal_convergence(args.files, reference=args.reference,
                                 output=args.output)
    for day, info in out.items():
        pts = info['points']
        print(f"day {day}: "
              + ", ".join(f"(ne={n}, err={e:.3e})" for n, e, _ in pts))
        for (n0, n1), r in info['rates']:
            print(f"  rate ne={n0}->{n1}: {r:.2f}")
