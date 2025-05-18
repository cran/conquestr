# README

## Who do I talk to? 

* for help with conquestr: dan.cloney@acer.org
* for help with ACER ConQuest: sales@acer.org or conquest@acer.org
* ACER ConQuest Manual https://conquestmanual.acer.org

## conquestr is built and tested on Win, OS X using rhub2

* see more on [R-hub Documentation](https://r-hub.github.io/rhub/)
* rhub::rhub_check()

## conquestr is built and tested on linux using a docker container

* rhub2 build servers for Ubuntu seem to fail when reading ConQuest system
  files. The reason is unknown, however all checks pass when using rstudio
  on Ubuntu. See `conquestr.Dockerfile` in this repo.