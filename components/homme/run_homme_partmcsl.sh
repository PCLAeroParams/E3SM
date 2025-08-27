#!/bin/bash

e3sm=$HOME/e3sm-pclap
homme=$e3sm/components/homme
mach=$homme/cmake/machineFiles/flight.cmake
wdir=/pscratch/pabosle/e3sm-pclap
source $e3sm/pclap-e3sm-test.F2010.ne4pg2_oQU480.partmcsl/.env_mach_specific.sh


execName=theta-l-nlev20-native
namelistFile=$homme/partmcsl_dcmip12_transport.nl

configFlag=
buildFlag=
runFlag=
submitFlag=

jobFile=batch_run.cmd
nnodes=1
nranksPerNode=2
wtime="00:30:00"
res=flight-cldera
acct=fy210162
ntasks=112

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
  cmake -Wno-dev -C $mach -DQSIZE_D=9 -DHOMME_USE_MKL=FALSE $homme
fi

if [ "$buildFlag" ]
then
  printf "Building ${execName}\n"
  # build the executable
  cd $wdir
  make -j 24 $execName
fi

if [ "$runFlag" ]
then
printf "creating job command to run.\n"
cat <<EOF > $jobFile
#!/bin/bash
#SBATCH -N $nnodes
#SBATCH -t $wtime
#SBATCH -A fy210162
#SBATCH -n 112
#SBATCH --reservation flight-cldera
mpirun --map-by ppr:56:socket:PE=1 --bind-to core --n $ntasks $wdir/test_execs/$execName/$execName < $namelistFile
EOF
chmod +x $jobFile
cat $jobFile
fi

if [ "$submitFlag" ]
then
sbatch $jobFile
fi