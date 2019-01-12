#!/bin/bash
# This script is used to make a local copy of GEFF data.
# Variable in capital letter are user-defined.
# Variables in lower letters are local.
# exec bash ---- to use a fresh & new environnement
# To generate Zenodo-like datasets, please select the following when prompted:
# 1. longitudes in range [-180, +180]
# 2. daily layers are assembled in one three-dimensional datacube.

echo -e "### USER DEFINED SETTINGS ############################################"

echo -e "This script is used to make a local copy of GEFF data"

echo -e "Please enter a start date"
echo -e "(e.g. 2015-09-07):"
read STARTINGDATE

echo -e "Please enter an end date"
echo -e "(e.g. 2015-09-07):"
read ENDINGDATE

echo -e "Please enter origin"
echo -e "(options are 'FWI' (default), 'MARK5', 'NFDRS', 'FO'):"
read ORIGIN
ORIGIN=${ORIGIN:-FWI}

echo -e "Please enter variable name"
echo -e "(options are 'fwi' (default)):"
read VARS
VARS=${VARS:-fwi}

echo -e "Do you wish to rotate longitudes from [0, 360] to [-180, +180]?"
echo -e "(options are 'y' (default), or 'n' to keep original range)"
read ROTATE
ROTATE=${ROTATE:-y}

echo -e "Do you wish to stack the layers into one NetCDF file?"
echo -e "(options are 'y' (default), or 'n' to keep original files)"
read STACK
STACK=${STACK:-y}

echo -e "Please enter data type"
echo -e "(options are 're' (reanalysis, default) or 'rt' (realtime forecast)):"
read DATATYPE
DATATYPE=${DATATYPE:-re}
if [ ${DATATYPE} != "re" ] && [ ${DATATYPE} != "rt" ]; then
  echo "ERROR: invalid DATATYPE!"
  exit
fi
if [ ${DATATYPE} == "re" ]; then
  echo -e "Please enter a path for the input folder"
  echo -e "(default is ec:/emos/geff/0001/re):"
  read INDIR
  INDIR=${INDIR:-ec:/emos/geff/0001/re}
  echo -e "Please enter a path for the output folder"
  echo -e "(default is /hugetmp/fire/geff/reanalysis):"
  read OUTDIR
  OUTDIR=${OUTDIR:-/hugetmp/fire/geff/reanalysis}
  FORECASTTYPE=re
fi
if [ ${DATATYPE} == "rt" ]; then
  echo -e "Please enter a path for the input folder"
  echo -e "(default is ec:/emos/geff/0001/rt):"
  read INDIR
  INDIR=${INDIR:-ec:/emos/geff/0001/rt}
  echo -e "Please enter a path for the output folder"
  echo -e "(default is /hugetmp/fire/geff/forecast):"
  read OUTDIR
  OUTDIR=${OUTDIR:-/hugetmp/fire/geff/forecast}
  echo -e "Please enter forecast type"
  echo -e "(options are 'hr' (HRES, default) or 'en' (ENS)):"
  read FORECASTTYPE
  FORECASTTYPE=${FORECASTTYPE:-hr}
fi

echo ""
echo "### EXECUTING THE SCRIPT ################################################"
echo ""

# Load modules to be used
module load cdo

# Create directory structure, if it does not already exist
mkdir -p ${OUTDIR}
for index in ${VARS[*]}; do
  mkdir -p ${OUTDIR}/${index}
  mkdir -p ${OUTDIR}/${index}_rotated
done

# You can use slightly malformed dates (2016-9-9 rather than 2016-09-09).
# The code below ensures that startdate and enddate are valid ISO 8601 dates.
# In case the script encounters unparseable data (ENDINGDATE=abcd) it will abort
startdate=$(date -I -d "$STARTINGDATE") || exit -1
enddateminusone=$(date -I -d "$ENDINGDATE") || exit -1
enddate=$(date -I -d "$enddateminusone + 1 day")

d="$startdate"
echo "Copying the following file into the current working directory"
while [ "$d" != "$enddate" ]; do
  # Get year/month/day from current date
  year=${d:0:4}
  month=${d:5:2}
  day=${d:8:2}
  infolder=${INDIR}/${year}/${month}
  # Read and copy files (one-by-one) locally
  infile=ECMWF_EFFIS_${year}${month}${day}_1200_${FORECASTTYPE}
  # echo ${infolder}/${infile}.tar
  ecp -o ${infolder}/${infile}.tar ${OUTDIR}
  # echo "De-compressing archive"
  # Decompress (tar -xvf)
  # and save in given folder (-C)
  # only the relevant file without folder structure(--strip-component 1)
  if [ "${FORECASTTYPE}" == "en" ]; then
    if [  "${ORIGIN}" == "FO" ]; then
      member=00
      ncfile=ECMWF_${ORIGIN}_${year}${month}${day}_1200_${member}
      tar -xvf ${OUTDIR}/${infile}.tar -C ${OUTDIR} \
        ${infile}/${ncfile}.nc --strip-components 1
      for index in ${VARS[*]}; do
        # echo "Extracting indices"
        if [ "${index}" != "all" ]; then
          cdo --silent -select,name=${index} ${OUTDIR}/${ncfile}.nc \
            ${OUTDIR}/${index}/${ncfile}_${index}.nc
        fi
      done
      rm ${OUTDIR}/${ncfile}.nc
    else
      for member in 0{0..9} {10..50}; do
        ncfile=ECMWF_${ORIGIN}_${year}${month}${day}_1200_${member}
        tar -xvf ${OUTDIR}/${infile}.tar -C ${OUTDIR} \
          ${infile}/${ncfile}.nc --strip-components 1
        for index in ${VARS[*]}; do
          # echo "Extracting indices"
          if [ "${index}" != "all" ]; then
            cdo --silent -select,name=${index} ${OUTDIR}/${ncfile}.nc \
              ${OUTDIR}/${index}/${ncfile}_${index}.nc
          fi
        done
        rm ${OUTDIR}/${ncfile}.nc
      done
    fi
  else
    ncfile=ECMWF_${ORIGIN}_${year}${month}${day}_1200_${FORECASTTYPE}
    tar -xvf ${OUTDIR}/${infile}.tar -C ${OUTDIR} \
      ${infile}/${ncfile}.nc --strip-components 1
    for index in ${VARS[*]}; do
      # echo "Extracting indices"
      if [ "${index}" != "all" ]; then
        cdo --silent -select,name=${index} ${OUTDIR}/${ncfile}.nc \
          ${OUTDIR}/${index}/${ncfile}_${index}.nc
        rm ${OUTDIR}/${ncfile}.nc
      fi
    done
  fi
  # echo "Removing unnecessary tar file"
  rm ${OUTDIR}/${infile}.tar
  # Increment of 1 day
  d=$(date -I -d "$d + 1 day")
done

if [ "${ROTATE}" == "y" ]; then
  echo "Rotating longitudes from [0,360] to [-180,+180]"
  for index in ${VARS[*]}; do
    # Rotate longitudes from [0,360] to [-180,+180]
    for filename in ${OUTDIR}/${index}/*.nc; do
      basefilename=${filename##*/}
      cdo --silent sellonlatbox,-180,180,-90,90 ${filename} \
          ${OUTDIR}/${index}_rotated/${basefilename}
    done
    # Remove non-rotated files
    rm -rf ${OUTDIR}/${index}
  done
  folder2stack=${OUTDIR}/${index}_rotated/
else
  folder2stack=${OUTDIR}/${index}/
fi

if [ "${STACK}" == "y" ]; then
  echo "Stacking all files into 1 netcdf"
  for index in ${VARS[*]}; do
    # Stack all files for a given variable into 1 netcdf
    cdo --silent cat ${folder2stack}/*.nc ${OUTDIR}/${index}.nc
    # Remove original files
    rm -rf ${folder2stack}
  done
fi

echo -e "######################################################################"
