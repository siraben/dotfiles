{ pkgs }:

pkgs.python311.withPackages (p: with p; [
  aiohttp # async HTTP
  beautifulsoup4 # web scraping
  ipython # interactive shell
  jupyter # interactive notebooks
  matplotlib # plots
  numpy # numerical computation
  pandas # data analysis
  pylint # static checking
  requests # HTTP library
  setuptools # setup.py
  scipy
  scikit-learn
  z3 # Z3 theorem prover
])
