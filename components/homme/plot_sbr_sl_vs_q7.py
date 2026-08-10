#!/usr/bin/env python3
"""Control plot for the SBR space sweep: ||Q - Q7||/||Q7|| at t=1h, 3h, 6h.

This is the "traditional" metric using the HOMME analytic exact (Q7) as the
reference, complementing the main summary plot which uses the Python analytic
exact.  For context we also overlay ||Q7 - Q_exact||/||Q_exact|| (dashed) so
the crossover where SL's true error falls below Q7's projection error is
visible -- past that crossover, ||Q - Q7|| stops reflecting SL's true
convergence rate and starts reflecting Q7's projection error rate instead.
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
OUT_PATH = "sbr_sweep_sl_vs_q7.png"

# Constants for the Python analytic-exact reference (must match F90).
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


def load_errors():
    err_vs_q7  = {t: {} for t in TIME_HOURS}
    err_q7_vs_ex = {t: {} for t in TIME_HOURS}
    for ne, path in FILES.items():
        ds = xr.open_dataset(path)
        lat = np.deg2rad(ds.lat.values); lon = np.deg2rad(ds.lon.values)
        height = H_h*np.log(1.0/(ds.hyam.values + ds.hybm.values))
        times_h = np.round(ds.time.values*24, 2)
        for t in TIME_HOURS:
            it = int(np.argmin(np.abs(times_h - t)))
            q  = ds.Q .isel(time=it).values
            q7 = ds.Q7.isel(time=it).values
            q_ex = q_exact(lat[np.newaxis,:], lon[np.newaxis,:],
                           height[:,np.newaxis], t*3600.0)
            d7  = float(np.sqrt(np.mean(q7**2)))
            dex = float(np.sqrt(np.mean(q_ex**2)))
            err_vs_q7[t][ne]    = float(np.sqrt(np.mean((q  - q7  )**2))) / d7
            err_q7_vs_ex[t][ne] = float(np.sqrt(np.mean((q7 - q_ex)**2))) / dex
    return err_vs_q7, err_q7_vs_ex


def main():
    err_vs_q7, err_q7_vs_ex = load_errors()
    ne_arr = np.array(sorted(FILES.keys()))

    fig, ax = plt.subplots(figsize=(7.5, 5.5))
    time_colors = {1: "#1f77b4", 3: "#2ca02c", 6: "#d62728"}

    for t in TIME_HOURS:
        y_sl = [err_vs_q7[t][ne]    for ne in ne_arr]
        y_q7 = [err_q7_vs_ex[t][ne] for ne in ne_arr]
        ax.loglog(ne_arr, y_sl, "s-",  color=time_colors[t],
                  label=fr"$\|Q-Q_7\|/\|Q_7\|$, t={t}h")
        ax.loglog(ne_arr, y_q7, "^:",  color=time_colors[t],
                  markerfacecolor="none",
                  label=fr"$\|Q_7-Q_{{\rm ex}}\|/\|Q_{{\rm ex}}\|$, t={t}h",
                  alpha=0.75)

    # Reference slopes anchored to the coarsest ||Q - Q7|| point at t=1h.
    y_anchor = err_vs_q7[1][ne_arr[0]]
    ax.loglog(ne_arr, y_anchor * (ne_arr[0]/ne_arr),    "k:",  alpha=0.4,
              label="1st-order slope")
    ax.loglog(ne_arr, y_anchor * (ne_arr[0]/ne_arr)**2, "k--", alpha=0.4,
              label="2nd-order slope")

    ax.set_xlabel("ne (elements per cube face)")
    ax.set_ylabel(r"relative $L_2$ error")
    ax.set_title(r"SL control: $\|Q-Q_7\|$ vs the projection-error floor "
                 r"$\|Q_7-Q_{\rm ex}\|$")
    ax.set_xticks(ne_arr); ax.set_xticklabels([str(n) for n in ne_arr])
    ax.grid(True, which="both", alpha=0.3)
    ax.legend(fontsize=8, loc="lower left")
    fig.tight_layout()
    fig.savefig(OUT_PATH, dpi=150, bbox_inches="tight")
    print(f"wrote {OUT_PATH}")


if __name__ == "__main__":
    main()
