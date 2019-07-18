#!/bin/bash

DESTDIR=/local/u46/dra547/miniconda3

pushd $TMPDIR

wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
chmod +x Miniconda3-latest-Linux-x86_64.sh

./Miniconda3-latest-Linux-x86_64.sh -b -p $DESTDIR -u


__conda_setup="$('/local/v10/dra547/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
eval "$__conda_setup"

conda update -n base -y conda

conda create --name omad -y python=3.6 psycopg2 click gdal jupyterlab nodejs rasterio xarray pyyaml dask boltons netcdf4 lark-parser pypeg2 cachetools singledispatch sqlalchemy structlog scipy

conda activate omad
# for eo-datasets2
conda install cattrs ciso8601 h5py pyproj ruamel.yaml shapely scikit-image deepdiff flake8 pep8-naming python-rapidjson rio-cogeo

# from https://github.com/jwkvam/jupyterlab-vim
#jupyter labextension install jupyterlab_vim

# from https://github.com/pbugnion/jupyterlab-sql


#pip install jupyterlab_sql
#jupyter serverextension enable jupyterlab_sql --py --sys-prefix
#jupyter lab build
#
#jupyter lab
