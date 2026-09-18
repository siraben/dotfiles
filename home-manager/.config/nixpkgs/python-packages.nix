{ pkgs }:

pkgs.python3.withPackages (p: with p; [
  aiohttp # async HTTP
  beautifulsoup4 # web scraping
  ipython # interactive shell
  matplotlib # plots
  numpy # numerical computation
  pandas # data analysis
  requests # HTTP library
  setuptools # setup.py
  scipy
  scikit-learn
  z3-solver # Z3 theorem prover
])
