#!/usr/bin/env python3
"""Generate concrete SBR-sweep namelists from partmcsl_sbr_sweep.nl.template.

Sweep points hold dt*ne = 900 s (constant CFL) with a 6-hour simulation
window and hourly output.  Each ne gets its own output_dir so files from
different resolutions don't collide.
"""

from pathlib import Path

TEMPLATE = Path("partmcsl_sbr_sweep.nl.template")
OUT_DIR  = Path(".")

SIM_SECONDS = 6 * 3600            # 6-hour simulation window
DT_NE_TARGET = 900                # constant-CFL invariant: tstep * ne

# ne -> (tstep_s, output_dir_name).  tstep is float for exact constant CFL.
SWEEP = [
    (16,  56.25, "movies_sbr_sweep_ne16"),
    (30,  30.0,  "movies_sbr_sweep_ne30"),
    (60,  15.0,  "movies_sbr_sweep_ne60"),
    (120, 7.5,   "movies_sbr_sweep_ne120"),
]


def fmt_tstep(t):
    # HOMME namelist reads real; use enough precision to preserve exactness.
    if abs(t - round(t)) < 1e-9:
        return f"{int(round(t))}"
    return f"{t:.4f}"


def main():
    tmpl = TEMPLATE.read_text()
    print(f"# ne  tstep    nmax    dt*ne    output_dir")
    for ne, tstep, out_name in SWEEP:
        nmax_f = SIM_SECONDS / tstep
        nmax = int(round(nmax_f))
        assert abs(nmax * tstep - SIM_SECONDS) < 1e-6, (
            f"non-integer nmax for ne={ne}, tstep={tstep}"
        )
        cfl = tstep * ne
        assert abs(cfl - DT_NE_TARGET) < 1e-6, (
            f"dt*ne={cfl} != target {DT_NE_TARGET} for ne={ne}"
        )
        out_dir = f"./{out_name}/"
        nl = (
            tmpl.replace("@NE@", str(ne))
                .replace("@TSTEP@", fmt_tstep(tstep))
                .replace("@NMAX@", str(nmax))
                .replace("@OUTPUT_DIR@", out_dir)
        )
        out_path = OUT_DIR / f"partmcsl_sbr_sweep_ne{ne}.nl"
        out_path.write_text(nl)
        print(f"  {ne:3d}  {fmt_tstep(tstep):>6}  {nmax:5d}  {int(cfl):5d}   {out_dir}")
        print(f"       wrote {out_path}")


if __name__ == "__main__":
    main()
