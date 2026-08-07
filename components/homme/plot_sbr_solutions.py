#!/usr/bin/env python3
"""Plot SBR diagnostic solutions from a HOMME-partmcsl NetCDF output.

Layout: N-row x 4-column grid of lat-lon panels.
  rows: one per snapshot in the file (all times)
  cols: exact (Q7), SL (Q), PartMCSL (Q5), PartMCSL error (Q5 - Q7)

Data live on the native GLL grid (unstructured `ncol`), so panels are
drawn with `tricontourf` over (lon, lat).  A single vertical level is
sampled -- pass --level to override the mid-level default.
"""

import argparse

import matplotlib.pyplot as plt
import numpy as np
import xarray as xr


def parse_args():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("infile", help="NetCDF output from run_homme_partmcsl.sh")
    p.add_argument(
        "-o",
        "--outfile",
        default=None,
        help="output PNG path (default: <infile>.png)",
    )
    p.add_argument(
        "--level",
        type=int,
        default=None,
        help="vertical level index (default: nlev // 2)",
    )
    p.add_argument(
        "--levels",
        type=int,
        default=21,
        help="number of contour levels per panel",
    )
    p.add_argument(
        "--dpi",
        type=int,
        default=120,
    )
    return p.parse_args()


def panel(ax, lon, lat, field, vmin, vmax, cmap, nlevels):
    """Draw one lat-lon tricontourf panel and return the mappable."""
    contours = np.linspace(vmin, vmax, nlevels)
    tcf = ax.tricontourf(
        lon,
        lat,
        field,
        levels=contours,
        cmap=cmap,
        vmin=vmin,
        vmax=vmax,
        extend="both",
    )
    ax.set_xlim(0, 360)
    ax.set_ylim(-90, 90)
    ax.set_xticks([0, 90, 180, 270, 360])
    ax.set_yticks([-90, -45, 0, 45, 90])
    ax.tick_params(labelsize=7)
    return tcf


def main():
    args = parse_args()
    outfile = args.outfile or (args.infile.rsplit(".", 1)[0] + ".png")

    ds = xr.open_dataset(args.infile)

    lon = ds["lon"].values
    lat = ds["lat"].values
    times = ds["time"].values
    time_units = ds["time"].attrs.get("units", "")

    nlev = ds.sizes["lev"]
    lev_idx = args.level if args.level is not None else nlev // 2

    # exact = Q7, SL = Q, PartMCSL = Q5, error = Q5 - Q7
    exact = ds["Q7"].isel(lev=lev_idx).values
    sl = ds["Q"].isel(lev=lev_idx).values
    pmc = ds["Q5"].isel(lev=lev_idx).values
    err = pmc - exact

    # Shared color scale across exact/SL/PartMCSL for direct comparison.
    field_stack = np.concatenate([exact.ravel(), sl.ravel(), pmc.ravel()])
    fmin = float(np.nanmin(field_stack))
    fmax = float(np.nanmax(field_stack))
    # Diverging scale for error, symmetric about zero.
    emax = float(np.nanmax(np.abs(err)))
    if emax == 0.0:
        emax = 1.0

    ntimes = len(times)
    ncols = 4
    fig, axes = plt.subplots(
        ntimes,
        ncols,
        figsize=(4.2 * ncols, 2.4 * ntimes + 0.6),
        squeeze=False,
    )

    col_titles = ["exact (Q7)", "SL (Q)", "PartMCSL (Q5)", "PartMCSL err (Q5 - Q7)"]

    last_tcf_field = None
    last_tcf_err = None

    for i in range(ntimes):
        row_axes = axes[i]
        last_tcf_field = panel(
            row_axes[0], lon, lat, exact[i], fmin, fmax, "viridis", args.levels
        )
        panel(row_axes[1], lon, lat, sl[i], fmin, fmax, "viridis", args.levels)
        panel(row_axes[2], lon, lat, pmc[i], fmin, fmax, "viridis", args.levels)
        last_tcf_err = panel(
            row_axes[3], lon, lat, err[i], -emax, emax, "RdBu_r", args.levels
        )

        row_axes[0].set_ylabel(
            f"t = {times[i]:g} {time_units}\nlat",
            fontsize=8,
        )
        for j in range(1, ncols):
            row_axes[j].set_yticklabels([])

        if i == 0:
            for j, title in enumerate(col_titles):
                row_axes[j].set_title(title, fontsize=10)

        if i == ntimes - 1:
            for j in range(ncols):
                row_axes[j].set_xlabel("lon", fontsize=8)
        else:
            for j in range(ncols):
                row_axes[j].set_xticklabels([])

    fig.suptitle(
        f"SBR diagnostic  |  {args.infile}  |  lev index {lev_idx} / {nlev}",
        fontsize=11,
    )

    fig.tight_layout(rect=(0.02, 0.04, 0.94, 0.97))

    # Two shared colorbars: one for the three field columns, one for error.
    cbar_ax_field = fig.add_axes([0.945, 0.55, 0.012, 0.35])
    fig.colorbar(last_tcf_field, cax=cbar_ax_field, label="q")
    cbar_ax_err = fig.add_axes([0.945, 0.10, 0.012, 0.35])
    fig.colorbar(last_tcf_err, cax=cbar_ax_err, label="Q5 - Q7")

    fig.savefig(outfile, dpi=args.dpi, bbox_inches="tight")
    print(f"wrote {outfile}")


if __name__ == "__main__":
    main()
