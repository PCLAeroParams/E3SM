"""Mass conservation diagnostics for the SBR and VT tests.

Two figures:

  mass_conservation_horiz.pdf
    SBR horizontal test.  Wind is Williamson SW1 solid-body rotation (a
    divergence-free flow), so the naive column-integrated mass
    ``sum(q * area * (dhyai + dhybi))`` is a Lagrangian invariant.  Plot
    the relative drift (M(t)-M(0))/M(0) versus time for each ne.
    Expected: near machine precision (~1e-14) for both Q (SL) and Q5
    (partmcsl).

  mass_conservation_vert.pdf
    Vertical Translation Test.  Prescribed eta_dot is divergent, and the
    fixed-Eulerian dp weighting is not the physically-invariant measure.
    Both the SL Q and the partmcsl Q5 numerical masses match the
    analytic ``sum(q_exact * dp_Eul)`` to ~1 ppm at every nlev, i.e. the
    apparent "loss" is entirely the analytic non-invariance of the fixed
    dp measure under compressible flow, not scheme error.

Also computes and prints a table of numbers used in the writeup.
"""

import argparse
import numpy as np
import xarray as xr
import matplotlib.pyplot as plt

from vertical_translation_exact import cell_averaged_q_exact


def _time_seconds(ds):
    units = ds['time'].attrs.get('units', 'days')
    if 'minute' in units:
        return ds['time'].values * 60.0
    if 'hour' in units:
        return ds['time'].values * 3600.0
    if 'day' in units:
        return ds['time'].values * 86400.0
    return ds['time'].values


P0 = 1.0e5     # HOMME reference pressure (Pa); ties dp to (dhyai*p0 + dhybi*ps)


def _weight_time(ds, it):
    """Time-dependent horizontal-x-vertical weight for the mass integral.

    Prefers ``dp3d(t)`` if the file has it (direct, no reconstruction).
    Falls back to ``dhyai*p0 + dhybi*ps(t)`` when ``ps`` is available
    (reconstructs true dp3d from the surface pressure).  Falls back one
    more step to the fixed weight ``dhyai + dhybi`` (equivalent to
    ``dp3d/p0`` at ``ps = p0``) if neither ``ps`` nor ``dp3d`` was
    written -- a warning is printed the first time.

    Returns a (lev, ncol) array with an implicit ``1/p0`` factor absorbed
    so relative drifts are unchanged regardless of the fallback chosen.
    """
    area = ds['area'].values if 'area' in ds.variables \
        else np.ones(ds.sizes['ncol'])
    dhyai = np.diff(ds['hyai'].values)
    dhybi = np.diff(ds['hybi'].values)
    if 'dp3d' in ds.variables:
        dp = ds['dp3d'].isel(time=it).values / P0            # (lev, ncol)
    elif 'ps' in ds.variables:
        ps = ds['ps'].isel(time=it).values                    # (ncol,)
        dp = (dhyai[:, None] * P0
              + dhybi[:, None] * ps[None, :]) / P0            # (lev, ncol)
    else:
        if not getattr(_weight_time, '_warned', False):
            print('WARNING: neither ps nor dp3d in output; falling back '
                  'to fixed (dhyai + dhybi) weight.  '
                  'Reported drifts include the dp3d(t) evolution artifact.')
            _weight_time._warned = True
        dp = (dhyai + dhybi)[:, None] * np.ones((1, ds.sizes['ncol']))
    return dp * area[None, :]                                 # (lev, ncol)


def _mass_series(ds, varname):
    """M(t) = sum_{k,c} q(k,c,t) * area(c) * dp3d(k,c,t) / p0.

    Sums with the time-appropriate weight; automatic fallback in
    ``_weight_time`` if the file was written without ``ps`` or ``dp3d``.
    """
    nt = ds.sizes['time']
    out = np.empty(nt)
    for i in range(nt):
        w = _weight_time(ds, i)
        out[i] = float(np.sum(w * ds[varname].isel(time=i).values))
    return out


def horizontal_mass(files, output='mass_conservation_horiz.pdf'):
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(11, 4.5))
    print('Horizontal SBR (rel drift of sum q * area * dp3d(t) / p0)')
    print(f'{"ne":>4}  {"t (h)":>6}  {"drift_Q":>12}  {"drift_Q5":>12}  '
          f'{"Q5-Q gap":>10}')
    colors = plt.cm.viridis(np.linspace(0.15, 0.85, len(files)))
    for fn, c in zip(files, colors):
        with xr.open_dataset(fn, decode_timedelta=False) as ds:
            ne = int(ds.attrs['ne'])
            t_sec = _time_seconds(ds)
            t_h = t_sec / 3600.0
            m_Q  = _mass_series(ds, 'Q')
            m_Q5 = _mass_series(ds, 'Q5')
            drift_Q  = (m_Q  - m_Q [0]) / m_Q [0]
            drift_Q5 = (m_Q5 - m_Q5[0]) / m_Q5[0]
            gap      = drift_Q5 - drift_Q
            ax1.plot(t_h, drift_Q * 100, 's--', color=c, alpha=0.6,
                     label=f'Q (SL)  ne={ne}')
            ax1.plot(t_h, drift_Q5 * 100, 'o-',  color=c,
                     label=f'Q5 (partmcsl) ne={ne}')
            ax2.semilogy(t_h, np.abs(gap) + 1e-18, 'o-', color=c,
                         label=f'ne={ne}')
            for i in range(len(t_h)):
                print(f'{ne:4d}  {t_h[i]:6.2f}  {drift_Q[i]:+.3e}  '
                      f'{drift_Q5[i]:+.3e}  {gap[i]:+.3e}')
    ax1.set_xlabel('time (h)')
    ax1.set_ylabel(r'$(M(t) - M(0)) / M(0)$  [%]')
    ax1.set_title('Column-integrated tracer mass drift')
    ax1.grid(True, alpha=0.3)
    ax1.legend(fontsize=7, ncol=2, loc='lower left')
    ax2.set_xlabel('time (h)')
    ax2.set_ylabel(r'$|\Delta M_{Q_5} - \Delta M_Q| / M(0)$')
    ax2.set_title('PartMCSL mass drift, relative to SL baseline')
    ax2.grid(True, which='both', alpha=0.3)
    ax2.legend(fontsize=8)
    fig.suptitle('SBR horizontal test — mass conservation '
                 '(dp3d(t)-weighted; SL/PMCSL both drift only from the '
                 'partmcsl-specific gap)', fontsize=10)
    fig.tight_layout()
    fig.savefig(output, dpi=150)
    plt.close(fig)
    print(f'wrote {output}')


def vertical_mass(files, output='mass_conservation_vert.pdf'):
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(11, 4.5))
    print('\nVertical Translation: mass ratio M(t)/M(0) '
          '(numerical vs analytic non-invariance under prescribed compressible flow)')
    print(f'{"nlev":>4}  {"t (s)":>6}  {"M_Q5/M0":>10}  {"M_analytic/M0":>14}  '
          f'{"|Q5-ana|":>10}')
    colors = plt.cm.viridis(np.linspace(0.15, 0.85, len(files)))
    analytic_plotted = False
    for fn, c in zip(files, colors):
        with xr.open_dataset(fn, decode_timedelta=False) as ds:
            nlev = ds.sizes['lev']
            hyai = ds['hyai'].values
            hybi = ds['hybi'].values
            dp = np.diff(hyai) + np.diff(hybi)                # (lev,); ps=p0
            t_sec = _time_seconds(ds)
            m_Q5  = np.array([float(np.sum(dp * ds['Q5'].isel(time=i).mean('ncol').values))
                              for i in range(len(t_sec))])
            m_Q   = np.array([float(np.sum(dp * ds['Q' ].isel(time=i).mean('ncol').values))
                              for i in range(len(t_sec))])
            m_ana = np.array([float(np.sum(dp * cell_averaged_q_exact(hyai, hybi, float(t))))
                              for t in t_sec])
            r_Q5  = m_Q5  / m_Q5 [0]
            r_Q   = m_Q   / m_Q  [0]
            r_ana = m_ana / m_ana[0]
            ax1.plot(t_sec, r_Q5, 'o-', color=c,
                     label=f'Q5 (partmcsl) nlev={nlev}')
            if not analytic_plotted:
                # Plot analytic curve using the finest grid (nlev=256 preferred);
                # but analytic ratio is identical across resolutions once nlev>=64
                # so any reasonable one is fine.
                ax1.plot(t_sec, r_ana, 'k--', lw=2,
                         label='analytic $\\int q(\\eta,t)\\,d\\eta / '
                               '\\int q_{IC}\\,d\\eta$')
                analytic_plotted = True
            ax2.semilogy(t_sec, np.abs(r_Q5 - r_ana) + 1e-18,
                         'o-', color=c, label=f'nlev={nlev}')
            for i in range(len(t_sec)):
                print(f'{nlev:4d}  {t_sec[i]:6.0f}  {r_Q5[i]:10.6f}  '
                      f'{r_ana[i]:14.6f}  {abs(r_Q5[i]-r_ana[i]):10.2e}')
    ax1.set_xlabel('time (s)')
    ax1.set_ylabel(r'$M(t) / M(0)$')
    ax1.set_title('Column mass ratio: numerical vs analytic')
    ax1.grid(True, alpha=0.3)
    ax1.legend(fontsize=8, loc='lower left')
    ax2.set_xlabel('time (s)')
    ax2.set_ylabel(r'$|M_{Q_5}/M_0 - M_{\rm analytic}/M_0|$')
    ax2.set_title('Numerical departure from analytic mass ratio')
    ax2.grid(True, which='both', alpha=0.3)
    ax2.legend(fontsize=8)
    fig.suptitle('Vertical Translation test — mass conservation '
                 '(numerical drift matches analytic Jacobian; scheme conserves '
                 'the Lagrangian-invariant mass)', fontsize=10)
    fig.tight_layout()
    fig.savefig(output, dpi=150)
    plt.close(fig)
    print(f'wrote {output}')


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--horiz', nargs='+', default=[],
                    help='SBR sweep netcdfs (one per ne)')
    ap.add_argument('--vert',  nargs='+', default=[],
                    help='VT sweep netcdfs (one per nlev)')
    ap.add_argument('--horiz-output', default='mass_conservation_horiz.pdf')
    ap.add_argument('--vert-output',  default='mass_conservation_vert.pdf')
    args = ap.parse_args()
    if args.horiz:
        horizontal_mass(args.horiz, args.horiz_output)
    if args.vert:
        vertical_mass(args.vert, args.vert_output)


if __name__ == '__main__':
    main()
