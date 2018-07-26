MINICONDA_DIR=${HOME}/miniconda3
CONDA_PROFILE_PATH=${MINICONDA_DIR}/etc/profile.d/conda.sh

if [[ -f ${CONDA_PROFILE_PATH} ]]; then
    source ${CONDA_PROFILE_PATH}
fi
