#!/usr/bin/env python3
"""Summary plot for the SBR space sweep (post Q7 subcell-indexing fix).

Left panel: log-log space-convergence of relative L2 errors against the
Python-computed analytic exact evaluated on the GLL grid.  Three curves:
  - Q  (SL)         — HOMME semi-Lagrangian, GLL-native
  - Q5 (PartMCSL)   — donor-cell FV, projected to GLL via gllfvremap
  - Q7 (analytic exact through gllfvremap fv->gll projection)
The Q7 curve is a validation of the projection kernel (Hannah et al. 2020
predicts 2nd-order; the fix restored it).
Reference slopes for 1st and 2nd order are shown.

Right panel: Q6 constant-tracer drift as grouped bars per ne (Σ frac = 1
diagnostic; unchanged by the Q7 indexing fix).
"""

import matplotlib.pyplot as plt
import numpy as np
import xarray as xr

FILES = {
    16:  "movies_sbr_3h/dcmip2012_test1_11_sweep_ne16.nc",
    30:  "movies_sbr_3h/dcmip2012_test1_11_sweep_ne30.nc",
    60:  "movies_sbr_3h/dcmip2012_test1_11_sweep_ne60.nc",
    120: "movies_sbr_3h/dcmip2012_test1_11_sweep_ne120.nc",
}
TIME_HOURS = [1, 3, 6]
OUT_PATH = "sbr_sweep_summary.png"

# Analytic-exact constants (must match dcmip12_wrapper.F90 module-scope decls).
PI       = np.pi
SBR_TAU  = 12.0 * 86400.0
SBR_ALPHA= PI / 4.0
OMEGA    = 2.0 * PI / SBR_TAU
GH_LAM0  = 5.0*PI/6.0;   GH_PHI0 = 0.0
GH_LAM1  = 7.0*PI/6.0;   GH_PHI1 = 0.0
GH_HMAX  = 0.95;         GH_B    = 5.0
GH_Z0    = 5000.0;       GH_ZZ   = 1000.0
Rd = 287.0; T0_H = 300.0; g = 9.80616
H_h  = Rd*T0_H/g


def q1_gh(lat, lon, height):
    xp, yp, zp = np.cos(lat)*np.cos(lon), np.cos(lat)*np.sin(lon), np.sin(lat)
    xc1, yc1 = np.cos(GH_PHI0)*np.cos(GH_LAM0), np.cos(GH_PHI0)*np.sin(GH_LAM0)
    xc2, yc2 = np.cos(GH_PHI1)*np.cos(GH_LAM1), np.cos(GH_PHI1)*np.sin(GH_LAM1)
    r2_1 = (xp-xc1)**2 + (yp-yc1)**2 + zp**2
    r2_2 = (xp-xc2)**2 + (yp-yc2)**2 + zp**2
    return GH_HMAX*np.exp(-((height-GH_Z0)/GH_ZZ)**2) \
           * (np.exp(-GH_B*r2_1) + np.exp(-GH_B*r2_2))


def q_exact(lat, lon, height, t_sec):
    nx, nz = -np.sin(SBR_ALPHA), np.cos(SBR_ALPHA)
    a = -OMEGA*t_sec
    ca, sa = np.cos(a), np.sin(a)
    x = np.cos(lat)*np.cos(lon); y = np.cos(lat)*np.sin(lon); z = np.sin(lat)
    dot = nx*x + nz*z
    xr = x*ca + (-nz*y)*sa + nx*dot*(1-ca)
    yr = y*ca + (nz*x - nx*z)*sa
    zr = z*ca + (nx*y)*sa   + nz*dot*(1-ca)
    r = np.sqrt(xr*xr + yr*yr + zr*zr)
    return q1_gh(np.arcsin(np.clip(zr/r, -1, 1)), np.arctan2(yr/r, xr/r), height)


def load_errors_and_drift():
    err = {sch: {t: {} for t in TIME_HOURS} for sch in ("Q", "Q5", "Q7")}
    q6  = {t: {} for t in TIME_HOURS}
    for ne, path in FILES.items():
        ds = xr.open_dataset(path)
        lat = np.deg2rad(ds.lat.values); lon = np.deg2rad(ds.lon.values)
        height = H_h*np.log(1.0/(ds.hyam.values + ds.hybm.values))
        times_h = np.round(ds.time.values*24, 2)
        for t in TIME_HOURS:
            it = int(np.argmin(np.abs(times_h - t)))
            q_ex = q_exact(lat[np.newaxis,:], lon[np.newaxis,:],
                           height[:,np.newaxis], t*3600.0)
            den = float(np.sqrt(np.mean(q_ex**2)))
            for sch in ("Q", "Q5", "Q7"):
                vals = ds[sch].isel(time=it).values
                err[sch][t][ne] = float(np.sqrt(np.mean((vals - q_ex)**2))) / den
            q6[t][ne] = float(np.max(np.abs(ds.Q6.isel(time=it).values - 1)))
    return err, q6


def main():
    err, q6 = load_errors_and_drift()
    ne_arr = np.array(sorted(FILES.keys()))

    fig, (ax_l, ax_r) = plt.subplots(1, 2, figsize=(13, 5))

    time_colors = {1: "#1f77b4", 3: "#2ca02c", 6: "#d62728"}
    style = {"Q": ("s--", "SL", "none"),
             "Q5": ("o-", "PartMCSL", None),
             "Q7": ("^:", "Q7 (fv→gll of analytic)", None)}
    for sch in ("Q5", "Q", "Q7"):
        marker, label, mfc = style[sch]
        for t in TIME_HOURS:
            y = [err[sch][t][ne] for ne in ne_arr]
            kwargs = dict(color=time_colors[t], label=f"{label}, t={t}h")
            if mfc is not None: kwargs["markerfacecolor"] = mfc
            ax_l.loglog(ne_arr, y, marker, **kwargs)

    # Reference slopes anchored to (ne=16, PartMCSL t=1h) for 1st, and (ne=16, Q7 t=1h) for 2nd.
    y1 = err["Q5"][1][16] * (ne_arr[0] / ne_arr)
    y2 = err["Q7"][1][16] * (ne_arr[0] / ne_arr)**2
    ax_l.loglog(ne_arr, y1, "k:",  alpha=0.4, label="1st-order slope")
    ax_l.loglog(ne_arr, y2, "k--", alpha=0.4, label="2nd-order slope")

    ax_l.set_xlabel("ne (elements per cube face)")
    ax_l.set_ylabel(r"$\|\cdot - Q_{\rm exact}\|_2 \, / \, \|Q_{\rm exact}\|_2$")
    ax_l.set_title("Space convergence vs Python analytic exact on GLL grid")
    ax_l.set_xticks(ne_arr); ax_l.set_xticklabels([str(n) for n in ne_arr])
    ax_l.grid(True, which="both", alpha=0.3)
    ax_l.legend(fontsize=7, ncol=2, loc="lower left")

    # Right panel: Q6 drift grouped bars
    x = np.arange(len(ne_arr))
    width = 0.26
    for i, t in enumerate(TIME_HOURS):
        vals = [q6[t][ne] for ne in ne_arr]
        ax_r.bar(x + (i - 1)*width, vals, width,
                 label=f"t={t}h", color=time_colors[t])
    ax_r.set_yscale("log")
    ax_r.set_xticks(x); ax_r.set_xticklabels([f"ne={n}" for n in ne_arr])
    ax_r.set_ylabel(r"$\max\,|Q_6 - 1|$")
    ax_r.set_title(r"Q6 constant-tracer drift ($\Sigma$ frac $=$ 1 diagnostic)")
    ax_r.axhline(1e-8, color="k", ls=":", alpha=0.4)
    ax_r.text(len(ne_arr) - 0.5, 1.4e-8, "~roundoff floor", fontsize=7,
              ha="right", va="bottom", alpha=0.6)
    ax_r.grid(True, which="both", axis="y", alpha=0.3)
    ax_r.legend(fontsize=8)

    fig.suptitle("SBR sweep — post Q7 subcell-indexing fix, dt·ne=900, "
                 "DCMIP Gaussian hills, nlev=20",
                 fontsize=11)
    fig.tight_layout(rect=(0, 0, 1, 0.97))
    fig.savefig(OUT_PATH, dpi=150, bbox_inches="tight")
    print(f"wrote {OUT_PATH}")


if __name__ == "__main__":
    main()
