#!/bin/bash

e3sm=$HOME/e3sm-pclap
homme=$e3sm/components/homme
# mach=$homme/cmake/machineFiles/flight.cmake
mach=$homme/cmake/machineFiles/cee-compute.cmake
# wdir=/pscratch/pabosle/e3sm-pclap
wdir=/scratch/pabosle/e3sm-pclap
# source $e3sm/pclap-e3sm-test.F2010.ne4pg2_oQU480.partmcsl/.env_mach_specific.sh
# source $HOME/cee-homme-env.sh


# execName=theta-l-nlev128-native
execName=theta-l-nlev20-native
# SBR path requires -DPARTMCSL_SBR_DIAG at build (guards enable Test S wind
# override + Q7 analytic-exact); deformational path builds without it.
# namelistFile=$homme/partmcsl_dcmip12_transport.nl   # full 12-day deformational sweep
# namelistFile=$homme/partmcsl_dcmip12_shakedown.nl   # 3-day deformational-flow shakedown
namelistFile=$homme/partmcsl_sbr_3h.nl                # SBR shakedown (requires -DPARTMCSL_SBR_DIAG)

configFlag=
buildFlag=
runFlag=
submitFlag=

jobFile=batch_run.cmd
nnodes=1
nranksPerNode=2
wtime="00:15:00"
res=flight-cldera
acct=fy210162
ntasks=56

while getopts 'cbrs' OPTION
do
  case $OPTION in
    c) configFlag=1
       ;; 
    b) buildFlag=1
       ;;
    r) runFlag=1
       ;;
    s) submitFlag=1
       ;;
    ?) printf "Usage: %s: [-cbr] args\n" $(basename $0) >&2
       exit 2
       ;;
  esac
done
shift $(($OPTIND -1))

if [ "$configFlag" ]
then
  printf "Configuring standalone Homme\n"
# configure Homme with CMake
  cd $wdir
  cmake -B $wdir -Wno-dev -C $mach -DQSIZE_D=9 -DCMAKE_BUILD_TYPE=RelWithDebInfo -DHOMME_USE_MKL=FALSE -DPARTMCSL_SBR_DIAG $homme
fi

if [ "$buildFlag" ]
then
  printf "Building ${execName}\n"
  # build the executable
  cd $wdir
  make -j 24 $execName
fi

if [ "$submitFlag" ]
then
printf "creating job command to run.\n"
cat <<EOF > $jobFile
#!/bin/bash
#SBATCH -N $nnodes
#SBATCH -t $wtime
#SBATCH -A fy210162
#SBATCH -n 112
#SBATCH --reservation flight-cldera
mpirun --map-by core --bind-to core --n $ntasks $wdir/test_execs/$execName/$execName < $namelistFile
EOF
chmod +x $jobFile
cat $jobFile
fi

if [ "$runFlag" ]
then
#mpirun --map-by core --bind-to core --n $ntasks $wdir/test_execs/$execName/$execName < $namelistFile 2>&1 | tee homme-out.txt

# for cee-compute005:
ntasks=448
mpirun --map-by ppr:28:socket:PE=1 --bind-to core --n $ntasks $wdir/test_execs/$execName/$execName < $namelistFile 2>&1 | tee homme-out.txt
fi
