{ pkgs }:

pkgs.python3.withPackages (p: with p; [
  aiohttp # async HTTP
  beautifulsoup4 # web scraping
  matplotlib # plots
  numpy # numerical computation
  pyyaml # YAML parsing
  requests # HTTP library
  setuptools # setup.py
])
